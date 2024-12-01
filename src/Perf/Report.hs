{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reporting on performance, potentially checking versus a canned results.
module Perf.Report
  ( Name,
    Header (..),
    parseHeader,
    CompareLevels (..),
    defaultCompareLevels,
    parseCompareLevels,
    ReportOptions (..),
    defaultReportOptions,
    parseReportOptions,
    PerfDumpOptions (..),
    defaultPerfDumpOptions,
    parsePerfDumpOptions,
    fromDump,
    report,
    reportMain,
    writeResult,
    readResult,
    CompareResult (..),
    compareNote,
    reportOrg2D,
    Golden (..),
    defaultGolden,
    parseGolden,
    replaceDefaultFilePath,
    parseClock,
  )
where

import Control.Exception
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.FormatN hiding (format)
import Data.List (intercalate)
import Data.List qualified as List
import Data.Map.Merge.Strict
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics
import Optics.Core
import Options.Applicative
import Perf.Chart
import Perf.Measure
import Perf.Stats
import Perf.Time (defaultClock)
import Perf.Types
import System.Clock
import System.Exit
import System.Mem
import Text.Printf hiding (parseFormat)
import Text.Read
import Chart

-- | Benchmark name
type Name = String

-- | Whether to include header information.
data Header = Header | NoHeader deriving (Eq, Show, Generic)

-- | Command-line parser for 'Header'
parseHeader :: Parser Header
parseHeader =
  flag' Header (long "header" <> help "include headers")
    <|> flag' NoHeader (long "noheader" <> help "dont include headers")
    <|> pure Header

-- | Options for production of a performance report.
data ReportOptions = ReportOptions
  { -- | Number of times to run a benchmark.
    reportN :: Int,
    reportClock :: Clock,
    reportStatDType :: StatDType,
    reportMeasureType :: MeasureType,
    reportGolden :: Golden,
    reportHeader :: Header,
    reportCompare :: CompareLevels,
    reportChart :: PerfChartOptions,
    reportDump :: PerfDumpOptions,
    reportGC :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Default options
--
-- >>> defaultReportOptions
-- ReportOptions {reportN = 1000, reportClock = MonotonicRaw, reportStatDType = StatAverage, reportMeasureType = MeasureTime, reportGolden = Golden {golden = "other/bench.perf", check = True, record = False}, reportHeader = Header, reportCompare = CompareLevels {errorLevel = 0.2, warningLevel = 5.0e-2, improvedLevel = 5.0e-2}}
defaultReportOptions :: ReportOptions
defaultReportOptions =
  ReportOptions
    1000
    defaultClock
    StatAverage
    MeasureTime
    defaultGolden
    Header
    defaultCompareLevels
    defaultPerfChartOptions
    defaultPerfDumpOptions
    False

-- | Command-line parser for 'ReportOptions'
parseReportOptions :: ReportOptions -> Parser ReportOptions
parseReportOptions def =
  ReportOptions
    <$> option auto (value (view #reportN def) <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> parseClock
    <*> parseStatD
    <*> parseMeasure
    <*> parseGolden
    <*> parseHeader
    <*> parseCompareLevels defaultCompareLevels
    <*> parsePerfChartOptions defaultPerfChartOptions
    <*> parsePerfDumpOptions defaultPerfDumpOptions
    <*> switch (long "gc" <> help "run the GC prior to measurement")

-- | Parse command-line 'Clock' options.
parseClock :: Parser Clock
parseClock =
  flag' Monotonic (long "Monotonic")
    <|> flag' Realtime (long "Realtime")
    <|> flag' ProcessCPUTime (long "ProcessCPUTime")
    <|> flag' ThreadCPUTime (long "ThreadCPUTime")
#ifdef mingw32_HOST_OS
    <|> pure ThreadCPUTime
#else
    <|> flag' MonotonicRaw (long "MonotonicRaw")
    <|> pure MonotonicRaw
#endif

data PerfDumpOptions = PerfDumpOptions { dumpFilepath :: FilePath, doDump :: Bool } deriving (Eq, Show, Generic)

defaultPerfDumpOptions :: PerfDumpOptions
defaultPerfDumpOptions = PerfDumpOptions "other/perf.map" False

-- | Parse charting options.
parsePerfDumpOptions :: PerfDumpOptions -> Parser PerfDumpOptions
parsePerfDumpOptions def =
  PerfDumpOptions <$>
    option str (value (view #dumpFilepath def) <> long "dumppath" <> help "dump file name") <*>
    switch (long "dump" <> help "dump raw performance data as a Map Text [[Double]]")

fromDump :: PerfDumpOptions -> IO (Map.Map Text [[Double]])
fromDump cfg = read <$> readFile (view #dumpFilepath cfg)

-- | Run and report a benchmark with the specified reporting options.
reportMain :: ReportOptions -> Name -> PerfT IO [[Double]] a -> IO a
reportMain o name t = do
  let !n = reportN o
  let s = reportStatDType o
  let c = reportClock o
  let mt = reportMeasureType o
  let o' = replaceDefaultFilePath (intercalate "-" [name, show n, show mt, show s]) o
  when (reportGC o) performGC
  (a, m) <- runPerfT (measureDs mt c n) t
  report o' (statify s m)
  (\cfg -> when (view #doChart cfg) (writeChartOptions (view #chartFilepath cfg) (perfCharts cfg (Just mt) m))) (reportChart o)
  (\cfg -> when (view #doDump cfg) (writeFile (view #dumpFilepath cfg) (show m))) (reportDump o)
  pure a

-- | Levels of geometric difference in compared performance that triggers reporting.
data CompareLevels = CompareLevels {errorLevel :: Double, warningLevel :: Double, improvedLevel :: Double} deriving (Eq, Show)

-- |
-- >>> defaultCompareLevels
-- CompareLevels {errorLevel = 0.2, warningLevel = 5.0e-2, improvedLevel = 5.0e-2}
defaultCompareLevels :: CompareLevels
defaultCompareLevels = CompareLevels 0.2 0.05 0.05

-- | Command-line parser for 'CompareLevels'
parseCompareLevels :: CompareLevels -> Parser CompareLevels
parseCompareLevels c =
  CompareLevels
    <$> option auto (value (errorLevel c) <> long "error" <> help "error level")
    <*> option auto (value (warningLevel c) <> long "warning" <> help "warning level")
    <*> option auto (value (improvedLevel c) <> long "improved" <> help "improved level")

-- | Write results to file
writeResult :: FilePath -> Map.Map [Text] Double -> IO ()
writeResult f m = writeFile f (show m)

-- | Read results from a file.
readResult :: FilePath -> IO (Either String (Map.Map [Text] Double))
readResult f = do
  a :: Either SomeException String <- try (readFile f)
  pure $ either (Left . show) readEither a

-- | Comparison data between two results.
data CompareResult = CompareResult {oldResult :: Maybe Double, newResult :: Maybe Double, noteResult :: Text} deriving (Show, Eq)

hasDegraded :: Map.Map a CompareResult -> Bool
hasDegraded m = any (((== "degraded") . noteResult) . snd) (Map.toList m)

-- | Compare two results and produce some notes given level triggers.
compareNote :: (Ord a) => CompareLevels -> Map.Map a Double -> Map.Map a Double -> Map.Map a CompareResult
compareNote cfg x y =
  merge
    (mapMissing (\_ x' -> CompareResult Nothing (Just x') "new result"))
    (mapMissing (\_ x' -> CompareResult (Just x') Nothing "old result not found"))
    ( zipWithMatched
        ( \_ x' y' ->
            CompareResult (Just x') (Just y') (note' x' y')
        )
    )
    x
    y
  where
    note' x' y'
      | y' / x' > 1 + errorLevel cfg = "degraded"
      | y' / x' > 1 + warningLevel cfg = "slightly-degraded"
      | y' / x' < (1 - improvedLevel cfg) = "improvement"
      | otherwise = ""

-- | Console-style header information.
formatHeader :: Map.Map [Text] a -> [Text] -> [Text]
formatHeader m ts =
  [mconcat $ Text.pack . printf "%-16s" <$> ((("label" <>) . Text.pack . show <$> [1 .. labelCols]) <> ts), mempty]
  where
    labelCols = maximum $ length <$> Map.keys m

-- | Format a comparison.
formatCompare :: Header -> Map.Map [Text] CompareResult -> [Text]
formatCompare h m =
  bool [] (formatHeader m ["old result", "new result", "change"]) (h == Header)
    <> Map.elems (Map.mapWithKey (\k a -> Text.pack . mconcat $ printf "%-16s" <$> (k <> compareReport a)) m)
  where
    compareReport (CompareResult x y n) =
      [ maybe mempty (expt (Just 3)) x,
        maybe mempty (expt (Just 3)) y,
        n
      ]

-- | Format a result as lines of text.
formatText :: Header -> Map.Map [Text] Text -> [Text]
formatText h m =
  bool [] (formatHeader m ["results"]) (h == Header)
    <> Map.elems (Map.mapWithKey (\k a -> Text.pack . mconcat $ printf "%-16s" <$> (k <> [a])) m)

-- | Format a result as a table.
reportOrg2D :: Map.Map [Text] Text -> IO ()
reportOrg2D m = do
  let rs = List.nub ((List.!! 1) . fst <$> Map.toList m)
  let cs = List.nub ((List.!! 0) . fst <$> Map.toList m)
  Text.putStrLn ("||" <> Text.intercalate "|" rs <> "|")
  mapM_
    ( \c ->
        Text.putStrLn
          ( "|"
              <> c
              <> "|"
              <> Text.intercalate "|" ((\r -> m Map.! [c, r]) <$> rs)
              <> "|"
          )
    )
    cs

reportToConsole :: [Text] -> IO ()
reportToConsole xs = traverse_ Text.putStrLn xs

-- | Golden file options.
data Golden = Golden {golden :: FilePath, check :: CheckGolden, record :: RecordGolden} deriving (Generic, Eq, Show)

-- | Whether to check against a golden file
data CheckGolden = CheckGolden | NoCheckGolden deriving (Eq, Show, Generic)

-- | Whether to overwrite a golden file
data RecordGolden = RecordGolden | NoRecordGolden deriving (Eq, Show, Generic)

-- | Default is Golden "other/bench.perf" CheckGolden NoRecordGolden
defaultGolden :: Golden
defaultGolden = Golden "other/bench.perf" CheckGolden NoRecordGolden

-- | Replace the golden file name stem if it's the default.
replaceGoldenDefault :: FilePath -> Golden -> Golden
replaceGoldenDefault s g = bool g g {golden = s} (golden g == golden defaultGolden)

defaultGoldenPath :: FilePath -> FilePath
defaultGoldenPath fp = "other/" <> fp <> ".perf"

-- | Replace the Golden file path with the suggested stem, but only if the user did not specify a specific file path at the command line.
replaceDefaultFilePath :: FilePath -> ReportOptions -> ReportOptions
replaceDefaultFilePath fp o =
  o {reportGolden = replaceGoldenDefault (defaultGoldenPath fp) (reportGolden o)}

-- | Parse command-line golden file options.
parseGolden :: Parser Golden
parseGolden =
  Golden
    <$> option str (Options.Applicative.value (golden defaultGolden) <> long "golden" <> short 'g' <> help "golden file name")
    -- True is the default for 'check'.
    <*> (bool NoCheckGolden CheckGolden <$> flag True False (long "nocheck" <> help "do not check versus the golden file"))
    <*> (bool NoRecordGolden RecordGolden <$> switch (long "record" <> short 'r' <> help "record the result to the golden file"))

reportConsoleNoCompare :: Header -> Map.Map [Text] Double -> IO ()
reportConsoleNoCompare h m =
  reportToConsole (formatText h (expt (Just 3) <$> m))

reportConsoleCompare :: Header -> Map.Map [Text] CompareResult -> IO ()
reportConsoleCompare h m =
  reportToConsole (formatCompare h m)

-- | Report results
--
-- If a goldenFile is checked, and performance has degraded, the function will exit with 'ExitFailure' so that 'cabal bench' and other types of processes can signal performance issues.
report :: ReportOptions -> Map.Map [Text] [Double] -> IO ()
report o m = do
  when
    ((== RecordGolden) $ record (reportGolden o))
    (writeResult (golden (reportGolden o)) m')
  case check (reportGolden o) of
    NoCheckGolden -> reportConsoleNoCompare (reportHeader o) m'
    CheckGolden -> do
      mOrig <- readResult (golden (reportGolden o))
      case mOrig of
        Left _ -> do
          reportConsoleNoCompare (reportHeader o) m'
          unless
            ((RecordGolden ==) $ record (reportGolden o))
            (putStrLn "No golden file found. To create one, run with '-r'")
        Right orig -> do
          let n = compareNote (reportCompare o) orig m'
          _ <- reportConsoleCompare (reportHeader o) n
          when (hasDegraded n) (exitWith $ ExitFailure 1)
  where
    m' = Map.fromList $ mconcat $ (\(ks, xss) -> zipWith (\x l -> (ks <> [l], x)) xss (measureLabels (reportMeasureType o))) <$> Map.toList m

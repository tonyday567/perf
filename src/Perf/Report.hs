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
    report2D,
    Golden (..),
    defaultGolden,
    parseGolden,
    replaceDefaultFilePath,
    parseClock,
    reportToConsole,
  )
where

import Chart
import Control.Exception
import Control.Monad
import Data.Bool
import Data.Foldable
import Data.List (intercalate)
import Data.List qualified as List
import Data.Map.Merge.Strict
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics
import Optics.Core
import Options.Applicative as OA
import Options.Applicative.Help.Pretty
import Perf.Algos
import Perf.BigO
import Perf.Chart
import Perf.Measure
import Perf.Stats
import Perf.Time (defaultClock)
import Perf.Types
import Prettyprinter.Render.Text qualified as PP
import System.Clock
import System.Exit
import System.Mem
import Test.Tasty
import Test.Tasty.Bench
import Text.PrettyPrint.Boxes qualified as B
import Text.Printf hiding (parseFormat)
import Text.Read

-- | Benchmark name
type Name = String

-- | Whether to include header information.
data Header = Header | NoHeader deriving (Eq, Show, Generic)

-- | Command-line parser for 'Header'
parseHeader :: Parser Header
parseHeader =
  flag' Header (long "header" <> help "include headers in reporting")
    <|> flag' NoHeader (long "noheader" <> help "dont include headers in reporting")
    <|> pure Header

-- | Options for production of a performance report.
data ReportOptions = ReportOptions
  { -- | Number of times to run a benchmark.
    reportN :: Int,
    reportLength :: Int,
    reportClock :: Clock,
    reportStatDType :: StatDType,
    reportMeasureType :: MeasureType,
    reportGolden :: Golden,
    reportHeader :: Header,
    reportCompare :: CompareLevels,
    reportChart :: PerfChartOptions,
    reportDump :: PerfDumpOptions,
    reportGC :: Bool,
    reportOrder :: OrderOptions,
    reportTasty :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Default reporting options
defaultReportOptions :: ReportOptions
defaultReportOptions =
  ReportOptions
    1000
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
    defaultOrderOptions
    False

-- | Command-line parser for 'ReportOptions'
parseReportOptions :: ReportOptions -> Parser ReportOptions
parseReportOptions def =
  ReportOptions
    <$> option auto (value (view #reportN def) <> showDefaultWith show <> long "runs" <> short 'n' <> metavar "INT" <> help "number of runs to perform")
    <*> option auto (value (view #reportLength def) <> long "length" <> showDefaultWith show <> short 'l' <> metavar "INT" <> help "length-like variable eg, used to alter list length and compute order")
    <*> parseClock
    <*> parseStatD
    <*> parseMeasure
    <*> parseGolden
    <*> parseHeader
    <*> parseCompareLevels defaultCompareLevels
    <*> parsePerfChartOptions defaultPerfChartOptions
    <*> parsePerfDumpOptions defaultPerfDumpOptions
    <*> switch (long "gc" <> help "run the GC prior to measurement")
    <*> parseOrderOptions defaultOrderOptions
    <*> switch (long "tasty" <> help "run tasty-bench")

-- | Parse command-line 'Clock' options.
parseClock :: Parser Clock
parseClock =
  flag' Monotonic (long "Monotonic" <> OA.style (annotate bold) <> help "use Monotonic clock")
    <|> flag' Realtime (long "Realtime" <> help "use Realtime clock")
    <|> flag' ProcessCPUTime (long "ProcessCPUTime" <> help "use ProcessCPUTime clock")
    <|> flag' ThreadCPUTime (long "ThreadCPUTime" <> help "use ThreadCPUTime clock")
    <|> flag' MonotonicRaw (long "MonotonicRaw" <> help "use MonotonicRaw clock")
    <|> pure MonotonicRaw

data PerfDumpOptions = PerfDumpOptions {dumpFilepath :: FilePath, doDump :: Bool} deriving (Eq, Show, Generic)

defaultPerfDumpOptions :: PerfDumpOptions
defaultPerfDumpOptions = PerfDumpOptions "other/perf.map" False

-- | Parse charting options.
parsePerfDumpOptions :: PerfDumpOptions -> Parser PerfDumpOptions
parsePerfDumpOptions def =
  PerfDumpOptions
    <$> option str (value (view #dumpFilepath def) <> showDefaultWith show <> long "dumppath" <> metavar "FILE" <> help "dump file name")
    <*> switch (long "dump" <> help "dump raw performance data as a Map Text [[Double]]")

fromDump :: PerfDumpOptions -> IO (Map.Map Text [[Double]])
fromDump cfg = read <$> readFile (view #dumpFilepath cfg)

-- | Run and report a benchmark with the specified reporting options.
reportMain :: Example -> ReportOptions -> Name -> (Int -> PerfT IO [[Double]] a) -> IO a
reportMain ex o name t = do
  let !n = reportN o
  let l = reportLength o
  let s = reportStatDType o
  let c = reportClock o
  let mt = reportMeasureType o
  let o' = replaceDefaultFilePath (intercalate "-" [name, show n, show mt, show s]) o
  when (reportGC o) performGC
  (a, m) <- runPerfT (measureDs mt c n) (t l)
  report o' (statify s m)
  (\cfg -> when (view #doChart cfg) (writeChartOptions (view #chartFilepath cfg) (perfCharts cfg (Just (measureLabels mt)) m))) (reportChart o)
  (\cfg -> when (view #doDump cfg) (writeFile (view #dumpFilepath cfg) (show m))) (reportDump o)
  when (view (#reportOrder % #doOrder) o) (reportBigO o t)
  when (view #reportTasty o) (reportTasty' ex o)
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
    <$> option auto (value (errorLevel c) <> showDefaultWith show <> long "error" <> metavar "DOUBLE" <> help "report an error if performance degrades by more than this")
    <*> option auto (value (warningLevel c) <> showDefaultWith show <> long "warning" <> metavar "DOUBLE" <> help "report a warning if performance degrades by more than this")
    <*> option auto (value (improvedLevel c) <> showDefaultWith show <> long "improved" <> metavar "DOUBLE" <> help "report if performance improves by more than this")

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
report2D :: Map.Map [Text] Double -> IO ()
report2D m = putStrLn $ B.render $ B.hsep 1 B.left $ cs' : rs'
  where
    rs = List.nub ((List.!! 1) . fst <$> Map.toList m)
    cs = List.nub ((List.!! 0) . fst <$> Map.toList m)
    bx = B.text . Text.unpack
    xs = (\c -> (\r -> m Map.! [c, r]) <$> rs) <$> cs
    xs' = fmap (fmap (bx . expt (Just 3))) xs
    cs' = B.vcat B.left (bx <$> ("algo" : cs))
    rs' = B.vcat B.right <$> zipWith (:) (bx <$> rs) (List.transpose xs')

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
    <$> option str (value (golden defaultGolden) <> showDefaultWith show <> long "golden" <> short 'g' <> metavar "FILE" <> help "golden file name")
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

reportBigO :: ReportOptions -> (Int -> PerfT IO [[Double]] a) -> IO ()
reportBigO o p = do
  m <- mapM (execPerfT (measureDs (view #reportMeasureType o) (view #reportClock o) (view #reportN o)) . p) ns
  putStrLn mempty
  reportToConsole $ PP.renderStrict . layoutPretty defaultLayoutOptions <$> os'' m
  pure ()
  where
    l = view #reportLength o
    ns = makeNs l (view (#reportOrder % #orderDivisor) o) (view (#reportOrder % #orderLow) o)
    ms m' = fmap (fmap (statD (view #reportStatDType o)) . List.transpose) <$> m'
    os m' = fmap (fmap (pretty . fromOrder . fst . estO (fromIntegral <$> ns)) . List.transpose) (Map.unionsWith (<>) (fmap (fmap (: [])) (ms m')))
    os' m' = mconcat $ (\(ks, xss) -> zipWith (\x l' -> ([ks] <> [l'], x)) xss (measureLabels (reportMeasureType o))) <$> Map.toList (os m')
    os'' m' = (\(k, v) -> (pretty . Text.intercalate ":") k <> " " <> v) <$> os' m'

reportTasty' :: Example -> ReportOptions -> IO ()
reportTasty' ex o = do
  t <- measureCpuTime (mkTimeout 1000000) (RelStDev 0.05) (tastyExample (examplePattern ex (view #reportLength o)))
  Text.putStrLn $ "tasty:time: " <> decimal (Just 3) (t * 1e9)

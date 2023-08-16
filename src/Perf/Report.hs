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
    infoReportOptions,
    report,
    reportMain,
    reportMainWith,
    writeResult,
    readResult,
    CompareResult (..),
    compareNote,
    reportOrg2D,
    Golden (..),
    defaultGolden,
    parseGolden,
    replaceDefaultFilePath,
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
import Options.Applicative
import Perf.Measure
import Perf.Stats
import Perf.Types
import System.Exit
import Text.Printf hiding (parseFormat)
import Text.Read

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
    reportStatDType :: StatDType,
    reportMeasureType :: MeasureType,
    reportGolden :: Golden,
    reportHeader :: Header,
    reportCompare :: CompareLevels
  }
  deriving (Eq, Show, Generic)

-- | Default options
--
-- >>> defaultReportOptions
-- ReportOptions {reportN = 1000, reportStatDType = StatAverage, reportMeasureType = MeasureTime, reportGolden = Golden {golden = "other/bench.perf", check = True, record = False}, reportHeader = Header, reportCompare = CompareLevels {errorLevel = 0.2, warningLevel = 5.0e-2, improvedLevel = 5.0e-2}}
defaultReportOptions :: ReportOptions
defaultReportOptions =
  ReportOptions
    1000
    StatAverage
    MeasureTime
    defaultGolden
    Header
    defaultCompareLevels

-- | Command-line parser for 'ReportOptions'
parseReportOptions :: Parser ReportOptions
parseReportOptions =
  ReportOptions
    <$> option auto (value 1000 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> parseStatD
    <*> parseMeasure
    <*> parseGolden
    <*> parseHeader
    <*> parseCompareLevels defaultCompareLevels

-- | Default command-line parser.
infoReportOptions :: ParserInfo ReportOptions
infoReportOptions =
  info
    (parseReportOptions <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "reporting options")

-- | Run and report a benchmark to the console. For example,
--
-- @reportMain "foo" (fap "sum" sum [1..1000])@ would:
--
-- - run a benchmark for summing the numbers 1 to a thousand.
--
-- - look for saved performance data in other/foo-1000-MeasureTime-StatAverage.perf
--
-- - report on performance in isolation or versus the canned data file if it exists.
--
-- - exit with failure if the performace had degraded.
reportMain :: Name -> PerfT IO [[Double]] a -> IO ()
reportMain name t = do
  o <- execParser infoReportOptions
  reportMainWith o name t

-- | Run and report a benchmark to the console with the supplied options.
reportMainWith :: ReportOptions -> Name -> PerfT IO [[Double]] a -> IO ()
reportMainWith o name t = do
  let !n = reportN o
  let s = reportStatDType o
  let mt = reportMeasureType o
  let o' = replaceDefaultFilePath (intercalate "-" [name, show n, show mt, show s]) o
  m <- execPerfT (measureDs mt n) t
  report o' (statify s m)

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
hasDegraded m = any ((== "degraded") . noteResult) $ fmap snd (Map.toList m)

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
data Golden = Golden {golden :: FilePath, check :: Bool, record :: Bool} deriving (Generic, Eq, Show)

-- | Default filepath is "other/bench.perf"
defaultGolden :: Golden
defaultGolden = Golden "other/bench.perf" True False

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
    <*> flag True False (long "nocheck" <> help "do not check versus the golden file")
    <*> switch (long "record" <> short 'r' <> help "record the result to the golden file")

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
    (record (reportGolden o))
    (writeResult (golden (reportGolden o)) m')
  case check (reportGolden o) of
    False -> reportConsoleNoCompare (reportHeader o) m'
    True -> do
      mOrig <- readResult (golden (reportGolden o))
      case mOrig of
        Left _ -> do
          reportConsoleNoCompare (reportHeader o) m'
          unless
            (record (reportGolden o))
            (putStrLn "No golden file found. To create one, run with '-r'")
        Right orig -> do
          let n = compareNote (reportCompare o) orig m'
          _ <- reportConsoleCompare (reportHeader o) n
          when (hasDegraded n) (exitWith $ ExitFailure 1)
  where
    m' = Map.fromList $ mconcat $ (\(ks, xss) -> zipWith (\x l -> (ks <> [l], x)) xss (measureLabels (reportMeasureType o))) <$> Map.toList m

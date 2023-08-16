{-# LANGUAGE OverloadedStrings #-}

-- | Reporting on performance, potentially checking versus a canned results.
module Perf.Report
  ( Format (..),
    Name,
    parseFormat,
    Header (..),
    parseHeader,
    CompareLevels (..),
    defaultCompareLevels,
    parseCompareLevels,
    ReportConfig (..),
    defaultReportConfig,
    parseReportConfig,
    writeResult,
    readResult,
    CompareResult (..),
    compareNote,
    outercalate,
    reportOrg2D,
    Golden (..),
    defaultGolden,
    replaceDefault,
    defaultPath,
    parseGolden,
    report,
    hasDegraded,
    ReportOptions (..),
    parseReportOptions,
    infoReportOptions,
    reportMain,
    reportMainWith,
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
import System.Exit
import Text.Printf hiding (parseFormat)
import Text.Read
import Perf.Measure
import Perf.Stats
import Perf.Types

-- | Type of format for report
data Format = OrgMode | ConsoleMode deriving (Eq, Show, Generic)

-- | Benchmark name
type Name = String

-- | Command-line parser for 'Format'
parseFormat :: Format -> Parser Format
parseFormat f =
  flag' OrgMode (long "orgmode" <> help "report using orgmode table format")
    <|> flag' ConsoleMode (long "console" <> help "report using plain table format")
    <|> pure f

-- | Whether to include header information.
data Header = Header | NoHeader deriving (Eq, Show, Generic)

-- | Command-line parser for 'Header'
parseHeader :: Header -> Parser Header
parseHeader h =
  flag' Header (long "header" <> help "include headers")
    <|> flag' NoHeader (long "noheader" <> help "dont include headers")
    <|> pure h

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

-- | Report configuration options
data ReportConfig = ReportConfig
  { format :: Format,
    includeHeader :: Header,
    levels :: CompareLevels
  }
  deriving (Eq, Show, Generic)

-- |
-- >>> defaultReportConfig
-- ReportConfig {format = ConsoleMode, includeHeader = Header, levels = CompareLevels {errorLevel = 0.2, warningLevel = 5.0e-2, improvedLevel = 5.0e-2}}
defaultReportConfig :: ReportConfig
defaultReportConfig = ReportConfig ConsoleMode Header defaultCompareLevels

-- | Parse 'ReportConfig' command line options.
parseReportConfig :: ReportConfig -> Parser ReportConfig
parseReportConfig c =
  ReportConfig
    <$> parseFormat (format c)
    <*> parseHeader (includeHeader c)
    <*> parseCompareLevels (levels c)

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

-- | Like intercalate, but on the outside as well.
outercalate :: Text -> [Text] -> Text
outercalate c ts = c <> Text.intercalate c ts <> c

-- | Report to a console, comparing the measurement versus a canned file.
--
-- Returns a Left 'ExitFailure' if performance degradation is detected, Returns a Left if the golden file doesn't exist or contains bad data.
reportGolden' :: ReportConfig -> FilePath -> Map.Map [Text] Double -> IO (Either String ExitCode)
reportGolden' cfg f m = do
  mOrig <- readResult f
  case mOrig of
    Left s -> do
      reportToConsole (formatIn (format cfg) (includeHeader cfg) (expt (Just 3) <$> m))
      pure $ Left s
    Right o -> do
      let n = compareNote (levels cfg) o m
      _ <- reportToConsole $ formatCompare (format cfg) (includeHeader cfg) n
      pure $ Right $ bool ExitSuccess (ExitFailure 1) (hasDegraded n)

-- | Org-mode style header.
formatOrgHeader :: Map.Map [Text] a -> [Text] -> [Text]
formatOrgHeader m ts =
  [ outercalate "|" ((("label" <>) . Text.pack . show <$> [1 .. labelCols]) <> ts),
    outercalate "|" (replicate (labelCols + 1) "---")
  ]
  where
    labelCols = maximum $ length <$> Map.keys m

-- | Console-style header information.
formatConsoleHeader :: Map.Map [Text] a -> [Text] -> [Text]
formatConsoleHeader m ts =
  [mconcat $ Text.pack . printf "%-20s" <$> ((("label" <>) . Text.pack . show <$> [1 .. labelCols]) <> ts), mempty]
  where
    labelCols = maximum $ length <$> Map.keys m

-- | Format a comparison.
formatCompare :: Format -> Header -> Map.Map [Text] CompareResult -> [Text]
formatCompare f h m =
  case f of
    OrgMode ->
      bool [] (formatOrgHeader m ["old_result", "new_result", "status"]) (h == Header)
        <> Map.elems (Map.mapWithKey (\k a -> outercalate "|" (k <> compareReport a)) m)
    ConsoleMode ->
      bool [] (formatConsoleHeader m ["old_result", "new_result", "status"]) (h == Header)
        <> Map.elems (Map.mapWithKey (\k a -> Text.pack . mconcat $ printf "%-20s" <$> (k <> compareReport a)) m)
  where
    compareReport (CompareResult x y n) =
      [ maybe mempty (expt (Just 3)) x,
        maybe mempty (expt (Just 3)) y,
        n
      ]

-- | Format a result in org-mode style
formatOrg :: Header -> Map.Map [Text] Text -> [Text]
formatOrg h m =
  bool [] (formatOrgHeader m ["results"]) (h == Header)
    <> Map.elems (Map.mapWithKey (\k a -> outercalate "|" (k <> [a])) m)

-- | Format a result in console-style
formatConsole :: Header -> Map.Map [Text] Text -> [Text]
formatConsole h m =
  bool [] (formatConsoleHeader m ["results"]) (h == Header)
    <> Map.elems (Map.mapWithKey (\k a -> Text.pack . mconcat $ printf "%-20s" <$> (k <> [a])) m)

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
replaceDefault :: FilePath -> Golden -> Golden
replaceDefault s g = bool g g {golden = s} (golden g == golden defaultGolden)

defaultPath :: FilePath -> FilePath
defaultPath fp = "other/" <> fp <> ".perf"

-- | Parse command-line golden file options.
--
parseGolden :: Parser Golden
parseGolden =
  Golden
    <$> option str (Options.Applicative.value (golden defaultGolden) <> long "golden" <> short 'g' <> help "golden file name")
        -- True is the default for 'check'.
    <*> flag True False (long "nocheck" <> help "do not check versus the golden file")
    <*> switch (long "record" <> short 'r' <> help "record the result to the golden file")

-- | Report results
--
-- If a goldenFile is checked, and performance has degraded, the function will exit with 'ExitFailure' so that 'cabal bench' and other types of processes can signal performance issues.
report :: ReportConfig -> Golden -> [Text] -> Map.Map [Text] [Double] -> IO ExitCode
report cfg g labels m = do
  x <-
    bool
      (reportToConsole (formatIn (format cfg) (includeHeader cfg) (expt (Just 3) <$> m')) >> pure ExitSuccess)
      ( do
          x' <- reportGolden' cfg (golden g) m'
          case x' of
            Left _ -> putStrLn "No golden file found to check. To create a golden file, run with '-r'" >> pure ExitSuccess
            Right e -> pure e
      )
      (check g)
  when
    (record g)
    (writeResult (golden g) m')
  pure x
  where
    m' = Map.fromList $ mconcat $ (\(ks, xss) -> zipWith (\x l -> (ks <> [l], x)) xss labels) <$> Map.toList m

-- | Format a result given 'Format' and 'Header' preferences.
formatIn :: Format -> Header -> Map.Map [Text] Text -> [Text]
formatIn f h = case f of
  OrgMode -> formatOrg h
  ConsoleMode -> formatConsole h

data ReportOptions = ReportOptions
  { reportN :: Int,
    reportStatDType :: StatDType,
    reportMeasureType :: MeasureType,
    reportGolden :: Golden,
    reportReportConfig :: ReportConfig
  }
  deriving (Eq, Show, Generic)

parseReportOptions :: Parser ReportOptions
parseReportOptions =
  ReportOptions
    <$> option auto (value 1000 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> parseStatD
    <*> parseMeasure
    <*> parseGolden
    <*> parseReportConfig defaultReportConfig

infoReportOptions :: ParserInfo ReportOptions
infoReportOptions =
  info
    (parseReportOptions <**> helper)
    (fullDesc <> progDesc "perf benchmarking" <> header "reporting options")

reportMain :: Name -> PerfT IO [[Double]] a -> IO ()
reportMain name t = do
  o <- execParser infoReportOptions
  reportMainWith o name t

reportMainWith :: ReportOptions -> Name -> PerfT IO [[Double]] a -> IO b
reportMainWith o name t = do
  let !n = reportN o
  let s = reportStatDType o
  let mt = reportMeasureType o
  let gold =
        replaceDefault
        (defaultPath $ intercalate "-" [name, show n, show mt, show s])
        (reportGolden o)
  let cfg = reportReportConfig o
  m <- execPerfT (measureDs mt n) t
  code <- report cfg gold (measureLabels mt) (statify s m)
  exitWith code

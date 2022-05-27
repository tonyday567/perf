{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Check versus a canned file for performance degradation
module Perf.Report
  ( Format (..),
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
    compareNote,
    outercalate,
    reportGolden,
    reportOrg2D,
    Golden (..),
    parseGolden,
    report,
  )
where

import Box hiding (value)
import qualified Box.Csv as Csv
import Colourista.Pure
import Control.Monad
import qualified Data.Attoparsec.Text as A
import Data.Bool
import Data.Either (fromRight)
import Data.Foldable
import Data.FormatN hiding (format)
import qualified Data.List as List
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.Generics
import Options.Applicative
import Text.Printf hiding (parseFormat)

data Format = OrgMode | ConsoleMode deriving (Eq, Show, Generic)

parseFormat :: Format -> Parser Format
parseFormat f =
  flag' OrgMode (long "orgmode" <> help "report using orgmode table format")
    <|> flag' ConsoleMode (long "console" <> help "report using plain table format")
    <|> pure f

data Header = Header | NoHeader deriving (Eq, Show, Generic)

parseHeader :: Header -> Parser Header
parseHeader h =
  flag' Header (long "header" <> help "include headers")
    <|> flag' NoHeader (long "noheader" <> help "dont include headers")
    <|> pure h

data CompareLevels = CompareLevels {errorLevel :: Double, warningLevel :: Double, improvedLevel :: Double} deriving (Eq, Show)

defaultCompareLevels :: CompareLevels
defaultCompareLevels = CompareLevels 0.2 0.05 0.05

parseCompareLevels :: CompareLevels -> Parser CompareLevels
parseCompareLevels c =
  CompareLevels
    <$> option auto (value (errorLevel c) <> long "error" <> help "error level")
    <*> option auto (value (warningLevel c) <> long "warning" <> help "warning level")
    <*> option auto (value (improvedLevel c) <> long "improved" <> help "improved level")

data ReportConfig = ReportConfig
  { format :: Format,
    includeHeader :: Header,
    levels :: CompareLevels
  }
  deriving (Eq, Show, Generic)

defaultReportConfig :: ReportConfig
defaultReportConfig = ReportConfig ConsoleMode Header defaultCompareLevels

parseReportConfig :: ReportConfig -> Parser ReportConfig
parseReportConfig c =
  ReportConfig
    <$> parseFormat (format c)
    <*> parseHeader (includeHeader c)
    <*> parseCompareLevels (levels c)

writeResult :: FilePath -> Map.Map [Text] Double -> IO ()
writeResult f m = glue <$> Csv.rowCommitter (Csv.CsvConfig f ',' Csv.NoHeader) (\(ls, v) -> ls <> [expt (Just 3) v]) <*|> qList (Map.toList m)

readResult :: FilePath -> IO (Map.Map [Text] Double)
readResult f = do
  r <- Csv.runCsv (Csv.CsvConfig f ',' Csv.NoHeader) Csv.fields
  let r' = [x | (Right x) <- r]
  let l = (\x -> (List.init x, fromRight 0 (A.parseOnly Csv.double (List.last x)))) <$> r'
  pure $ Map.fromList l

data CompareResult = CompareResult {oldResult :: Maybe Double, newResult :: Maybe Double, noteResult :: Text} deriving (Show, Eq)

compareNote :: (Ord a) => CompareLevels -> Map.Map a Double -> Map.Map a Double -> Map.Map a CompareResult
compareNote cfg x y =
  merge
    (mapMissing (\_ x' -> CompareResult Nothing (Just x') (formatWith [yellow] "new result")))
    (mapMissing (\_ x' -> CompareResult (Just x') Nothing (formatWith [yellow] "old result not found")))
    ( zipWithMatched
        ( \_ x' y' ->
            CompareResult (Just x') (Just y') (note' x' y')
        )
    )
    x
    y
  where
    note' x' y'
      | y' / x' > 1 + errorLevel cfg = formatWith [red] "degraded"
      | y' / x' > 1 + warningLevel cfg = formatWith [yellow] "slightly-degraded"
      | y' / x' < (1 - improvedLevel cfg) = formatWith [green] "improvement"
      | otherwise = ""

outercalate :: Text -> [Text] -> Text
outercalate c ts = c <> Text.intercalate c ts <> c

reportGolden :: ReportConfig -> FilePath -> Map.Map [Text] Double -> IO ()
reportGolden cfg f m = do
  mOrig <- readResult f
  let n = compareNote (levels cfg) mOrig m
  reportToConsole $ formatCompare (format cfg) (includeHeader cfg) n

formatOrgHeader :: Map.Map [Text] a -> [Text] -> [Text]
formatOrgHeader m ts =
  [ outercalate "|" ((("label" <>) . Text.pack . show <$> [1 .. labelCols]) <> ts),
    outercalate "|" (replicate (labelCols + 1) "---")
  ]
  where
    labelCols = maximum $ length <$> Map.keys m

formatConsoleHeader :: Map.Map [Text] a -> [Text] -> [Text]
formatConsoleHeader m ts =
  [mconcat $ Text.pack . printf "%-20s" <$> ((("label" <>) . Text.pack . show <$> [1 .. labelCols]) <> ts), mempty]
  where
    labelCols = maximum $ length <$> Map.keys m

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

formatOrg :: Header -> Map.Map [Text] Text -> [Text]
formatOrg h m =
  bool [] (formatOrgHeader m ["results"]) (h == Header)
    <> Map.elems (Map.mapWithKey (\k a -> outercalate "|" (k <> [a])) m)

formatConsole :: Header -> Map.Map [Text] Text -> [Text]
formatConsole h m =
  bool [] (formatConsoleHeader m ["results"]) (h == Header)
    <> Map.elems (Map.mapWithKey (\k a -> Text.pack . mconcat $ printf "%-20s" <$> (k <> [a])) m)

reportOrg2D :: Map.Map [Text] Text -> IO ()
reportOrg2D m = do
  let rs = List.nub ((List.!! 1) . fst <$> Map.toList m)
  let cs = List.nub ((List.!! 0) . fst <$> Map.toList m)
  Text.putStrLn ("||" <> Text.intercalate "|" rs <> "|")
  sequence_ $
    ( \c ->
        Text.putStrLn
          ( "|"
              <> c
              <> "|"
              <> Text.intercalate "|" ((\r -> m Map.! [c, r]) <$> rs)
              <> "|"
          )
    )
      <$> cs

reportToConsole :: [Text] -> IO ()
reportToConsole xs = traverse_ Text.putStrLn xs

data Golden = Golden {golden :: FilePath, check :: Bool, record :: Bool} deriving (Generic, Eq, Show)

parseGolden :: String -> Parser Golden
parseGolden def =
  Golden
    <$> option str (Options.Applicative.value ("other/" <> def <> ".csv") <> long "golden" <> short 'g' <> help "golden file name")
    <*> switch (long "check" <> short 'c' <> help "check versus a golden file")
    <*> switch (long "record" <> short 'r' <> help "record the result to a golden file")

report :: ReportConfig -> Golden -> [Text] -> Map.Map [Text] [Double] -> IO ()
report cfg g labels m = do
  bool
    (reportToConsole (formatIn (format cfg) (includeHeader cfg) (expt (Just 3) <$> m')))
    (reportGolden cfg (golden g) m')
    (check g)
  when
    (record g)
    (writeResult (golden g) m')
  where
    m' = Map.fromList $ mconcat $ (\(ks, xss) -> zipWith (\x l -> (ks <> [l], x)) xss labels) <$> Map.toList m

formatIn :: Format -> Header -> Map.Map [Text] Text -> [Text]
formatIn f h = case f of
  OrgMode -> formatOrg h
  ConsoleMode -> formatConsole h

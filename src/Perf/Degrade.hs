{-# LANGUAGE OverloadedStrings #-}

-- | Check versus a canned file for performance degradation

module Perf.Degrade
  ( writeResult,
    readResult,
    DegradeConfig (..),
    defaultDegradeConfig,
    compareNote,
    degradeCheck,
    degradePrint,
  ) where

import Box.Csv hiding (header)
import Box
import Data.FormatN
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import Data.Either (fromRight)
import Colourista.Pure

writeResult :: FilePath -> Map.Map [Text] Double -> IO ()
writeResult fp m = glue <$> rowCommitter (CsvConfig fp ',' NoHeader) (\(ls,v) -> ls <> [expt (Just 3) v]) <*|> qList (Map.toList m)

readResult :: FilePath -> IO (Map.Map [Text] Double)
readResult fp = do
  r <- runCsv (CsvConfig fp ',' NoHeader) fields
  let r' = [x | (Right x) <- r]
  let l = (\x -> (List.init x, fromRight 0 (A.parseOnly double (List.last x)))) <$> r'
  pure $ Map.fromList l

data DegradeConfig = DegradeConfig { errorLevel :: Double, warningLevel :: Double, improvedLevel :: Double } deriving (Eq, Show)

defaultDegradeConfig :: DegradeConfig
defaultDegradeConfig = DegradeConfig 0.2 0.05 0.05

data CompareResult = CompareResult { oldResult :: Maybe Double, newResult :: Maybe Double, noteResult :: Text } deriving (Show, Eq)

compareNote :: (Ord a) => DegradeConfig -> Map.Map a Double -> Map.Map a Double -> Map.Map a CompareResult
compareNote cfg x y =
  merge
  (mapMissing (\_ x' -> CompareResult Nothing (Just x') (formatWith [yellow] "new result")))
  (mapMissing (\_ x' -> CompareResult (Just x') Nothing (formatWith [yellow] "old result not found")))
  (zipWithMatched
   (\_ x' y' ->
       CompareResult (Just x') (Just y') (note' x' y'))) x y
  where
    note' x' y'
         | y' / x' > 1 + errorLevel cfg = formatWith [red] "degraded"
         | y' / x' > 1 + warningLevel cfg = formatWith [yellow] "slightly-degraded"
         | y' / x' < (1 - improvedLevel cfg) = formatWith [green] "improvement"
         | otherwise = ""

degradeCheck :: DegradeConfig -> FilePath -> Map.Map [Text] Double -> IO [([Text], CompareResult)]
degradeCheck cfg fp m = do
  mOrig <- readResult fp
  pure $ Map.toList (compareNote cfg mOrig m)

outercalate :: Text -> [Text] -> Text
outercalate c ts = c <> Text.intercalate c ts <> c

degradePrint :: DegradeConfig -> FilePath -> Map.Map [Text] Double -> IO ()
degradePrint cfg fp m = do
  xs <- degradeCheck cfg fp m
  mapM_ Text.putStrLn $ fmap (\(l,c) -> outercalate "|" (l <> comparePrint c)) xs
  where
    comparePrint (CompareResult x y n) = [maybe mempty (expt (Just 3)) x, maybe mempty (expt (Just 3)) y, n]

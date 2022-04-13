{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Check versus a canned file for performance degradation

module Perf.Report
  ( writeResult,
    readResult,
    DegradeConfig (..),
    defaultDegradeConfig,
    compareNote,
    degradeCheck,
    degradePrint,
    outercalate,
    printOrg,
    printOrgHeader,
    printOrg2D,
    Golden (..),
    parseGolden,
    rioOrg,
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
import GHC.Generics
import Options.Applicative

writeResult :: FilePath -> Map.Map [Text] Double -> IO ()
writeResult f m = glue <$> rowCommitter (CsvConfig f ',' NoHeader) (\(ls,v) -> ls <> [expt (Just 3) v]) <*|> qList (Map.toList m)

readResult :: FilePath -> IO (Map.Map [Text] Double)
readResult f = do
  r <- runCsv (CsvConfig f ',' NoHeader) fields
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
degradeCheck cfg f m = do
  mOrig <- readResult f
  pure $ Map.toList (compareNote cfg mOrig m)

outercalate :: Text -> [Text] -> Text
outercalate c ts = c <> Text.intercalate c ts <> c

degradePrint :: DegradeConfig -> FilePath -> Map.Map [Text] Double -> IO ()
degradePrint cfg f m = do
  xs <- degradeCheck cfg f m
  mapM_ Text.putStrLn $ fmap (\(l,c) -> outercalate "|" (l <> comparePrint c)) xs
  where
    comparePrint (CompareResult x y n) = [maybe mempty (expt (Just 3)) x, maybe mempty (expt (Just 3)) y, n]

printOrgHeader :: Map.Map [Text] a -> [Text] -> IO ()
printOrgHeader m ts = do
  Text.putStrLn $ outercalate "|" ((("label" <>) . Text.pack . show <$> [1..labelCols]) <> ts)
  Text.putStrLn $ outercalate "|" (replicate (labelCols+1) "---")
  pure ()
    where
      labelCols = maximum $ length <$> Map.keys m

printOrg :: Map.Map [Text] Text -> IO ()
printOrg m = do
  printOrgHeader m ["results"]
  _ <- Map.traverseWithKey (\k a -> Text.putStrLn (outercalate "|" (k <> [a]))) m
  pure ()

printOrg2D :: Map.Map [Text] Text -> IO ()
printOrg2D m = do
    let rs = List.nub ((List.!! 1) . fst <$> Map.toList m)
    let cs = List.nub ((List.!! 0) . fst <$> Map.toList m)
    Text.putStrLn ("||" <> Text.intercalate "|" rs <> "|")
    sequence_ $
      (\c -> Text.putStrLn
        ("|" <> c <> "|" <>
          Text.intercalate "|" ((\r -> m Map.! [c,r]) <$> rs) <> "|")) <$> cs

data Golden = Golden { golden :: FilePath, check :: Bool, record :: Bool } deriving (Generic, Eq, Show)

parseGolden :: String -> Parser Golden
parseGolden def =
  Golden <$>
  option str (Options.Applicative.value ("other/" <> def <> ".csv") <> long "golden" <> short 'g' <> help "golden file name") <*>
  switch (long "check" <> short 'c' <> help "check versus a golden file") <*>
  switch (long "record" <> short 'r' <> help "record the result to a golden file")

rioOrg :: Golden -> [Text] -> Map.Map [Text] [Double] -> IO ()
rioOrg g labels m = do
    if check g then degradePrint defaultDegradeConfig (golden g) m' else printOrg (expt (Just 3) <$> m')
    if record g then writeResult (golden g) m' else pure ()
    where
      m' = Map.fromList $ mconcat $ (\(ks,xss) -> zipWith (\x l -> (ks <> [l], x)) xss labels) <$> Map.toList m

{-# LANGUAGE OverloadedStrings #-}

-- | Check versus a canned file for performance degradation

module Perf.Degrade
  ( writeResult,
    readResult,
    compareResults,
    resultToTrial,
    degradeCheck,
    degradePrint,
  ) where

import Box.Csv hiding (header)
import Box
import Data.FormatN
import Data.Map.Merge.Strict
import Data.Bool
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Trial
import Data.Semigroup
import Data.List.NonEmpty
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Bifunctor
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import Data.Either (fromRight)

writeResult :: FilePath -> Map.Map [Text] Double -> IO ()
writeResult fp m = glue <$> rowCommitter (CsvConfig fp ',' NoHeader) (\(ls,v) -> ls <> [expt (Just 3) v]) <*|> qList (Map.toList m)

readResult :: FilePath -> IO (Map.Map [Text] Double)
readResult fp = do
  r <- runCsv (CsvConfig fp ',' NoHeader) fields
  let r' = [x | (Right x) <- r]
  let l = (\x -> (List.init x, fromRight 0 (A.parseOnly double (List.last x)))) <$> r'
  pure $ Map.fromList l

data CompareResult a = Missing1 | Missing2 | Equivalent | Improved a | Degraded a deriving (Show, Eq)

compareResults :: (Ord a) => Map.Map a Double -> Map.Map a Double -> Map.Map a (CompareResult Double)
compareResults x y =
  merge
  (mapMissing (\_ _ -> Missing2))
  (mapMissing (\_ _ -> Missing1))
  (zipWithMatched (\_ x' y' ->
                     bool
                     (bool (Improved (y'/x'-1)) (Degraded (y'/x'-1)) (x' < y'))
                     Equivalent (x' == y'))) x y

resultToTrial :: Double -> Double -> ([Text], CompareResult Double) -> Trial (Text, [Text]) ()
resultToTrial _ _ (ts, Missing1) = result ("original missing", ts) ()
resultToTrial _ _ (ts, Missing2) = result ("latest missing", ts) ()
resultToTrial _ _ (_, Equivalent) = pure ()
resultToTrial _ _ (_, Improved _) = pure ()
resultToTrial w e (ts, Degraded x) =
  bool
    (bool
     (pure ())
     (result ("worse by " <> x', ts) ())
     (x > w))
    (fiasco ("degraded by " <> x', ts))
    (x > e)
  where
    x' = percent commaSF (Just 2) x

degradeCheck :: Double -> Double -> FilePath -> Map.Map [Text] Double -> IO (Trial (Text, [Text]) ())
degradeCheck w e fp m = do
  mOrig <- readResult fp
  pure $ sconcat $ fromList $ resultToTrial w e <$> Map.toList (compareResults mOrig m)

degradePrint :: Double -> Double -> FilePath -> Map.Map [Text] Double -> IO ()
degradePrint w e fp m =
  (Text.putStrLn . prettyTrial) .
  first (\(l,ts) -> Text.intercalate "|" ([l]<>ts)) =<<
  degradeCheck w e fp m

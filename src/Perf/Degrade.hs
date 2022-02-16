-- | Check versus a canned file for performance degradation

module Perf.Degrade where

import Box.Csv hiding (header)
import Box
import Data.FormatN
import Data.Map.Merge.Strict
import Data.Bool
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Monoid

csvFile :: CsvConfig
csvFile = CsvConfig "./other/example1.csv" ',' NoHeader

writeMap :: FilePath -> Map.Map Text Double -> IO ()
writeMap fp m = glue <$> rowCommitter (csvFile { file = fp }) (\(l,v) -> [l,prec (Just 3) v]) <*|> qList (Map.toList m)

readMap :: FilePath -> IO (Map.Map Text Double)
readMap fp = do
  r <- runCsv (csvFile { file = fp  }) (\c -> (,) <$> field c <*> double)
  pure $ Map.fromList [x | (Right x) <- r]

extract :: (a1, Map.Map a2 (Sum c)) -> c
extract = snd . head . Map.toList . Map.map getSum . snd

extractList :: (a1, Map.Map a2 [c]) -> [c]
extractList = snd . head . Map.toList . snd

divP :: Map.Map Text Double -> Map.Map Text Double -> Map.Map Text Double
divP x y = merge dropMissing dropMissing (zipWithMatched (\_ x' y' -> x' / y')) x y

divFilter :: (Double -> Bool) -> Map.Map Text Double -> Map.Map Text Double -> Map.Map Text Double
divFilter p x y = merge dropMissing dropMissing (zipWithMaybeMatched (\_ x' y' -> bool Nothing (Just (x' / y')) (p $ x' / y'))) x y

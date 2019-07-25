
module Perf.Criterion where

import Prelude

import Criterion.Measurement
import Criterion.Measurement.Types
import Control.DeepSeq
import Data.Text
import Formatting
import Perf.Analysis
import Data.Scientific
import Readme.Lhs

criNF :: (NFData b) => Int -> (a -> b) -> a -> IO Measured
criNF n a b = do
      (m, _) <- Criterion.Measurement.measure (nf a b) (fromIntegral n)
      return m

-- | format the cputime and cycle criterion measures
formatCriRun :: Int -> Text -> Measured -> [Text]
formatCriRun p label m =
  [ label
  , formatF p (measCpuTime (rescale m))
  , formatI p $ measCycles (rescale m)]

formatCriRuns :: Int -> [(Text, Measured)] -> Block
formatCriRuns p runs =
  table
  mempty
  ["run", "cputimes", "cycles"]
  [AlignLeft, AlignRight, AlignRight]
  []
  (fmap (\(l,m) -> formatCriRun p l m) runs)


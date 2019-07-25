
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
criRun :: Int -> Text -> Measured -> [Text]
criRun p label m =
  [ label
  , formatF p (measCpuTime (rescale m))
  , formatI p $ measCycles (rescale m)]

-- | format cpu versus gc timings
criSpeed :: Int -> Text -> Measured -> [Text]
criSpeed p label m =
  [ label
  , formatF p (measCpuTime (rescale m))
  , formatF p (measMutatorCpuSeconds (rescale m))
  , formatF p (measGcCpuSeconds (rescale m))
  , formatI p $ measCycles (rescale m)
  ]

formatCriRuns :: (Int -> Text -> Measured -> [Text]) -> Int -> [(Text, Measured)] -> Block
formatCriRuns r p runs =
  table
  mempty
  ["run", "cputimes", "cycles"]
  [AlignLeft, AlignRight, AlignRight]
  []
  (fmap (\(l,m) -> r p l m) runs)

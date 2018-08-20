
module Perf.Criterion where

import Prelude
-- import Perf.Measure

import Criterion.Measurement
import Criterion.Measurement.Types
import Control.DeepSeq
import Data.Text
import Formatting
import Perf.Analysis
import Data.Scientific

criNF :: (NFData b) => Int -> (a -> b) -> a -> IO Measured
criNF n a b = do
      (m, _) <- Criterion.Measurement.measure (nf a b) (fromIntegral n)
      return m

-- | format the Measured result
formatRun :: Text -> Int -> Measured -> Text
formatRun label p m =
  sformat
  ((right 24 ' ' %. stext) %
    (left 9 ' ' %. prec p) %
    (left 9 ' ' %. prec p))
  label
  (fromFloatDigits $ measCpuTime (rescale m))
  (int2Sci $ measCycles (rescale m))

-- | header for formatRun
formatRunHeader :: Text
formatRunHeader =
  sformat
  ((right 24 ' ' %. stext) %
    (left 8 ' ' %. stext) %
    (left 9 ' ' %. stext))
  "run"
  "cputime"
  "cycles"


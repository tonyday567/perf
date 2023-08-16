-- | Sum example, measured using default settings.
module Main where

import Data.List qualified as List
import Perf
import Prelude

main :: IO ()
main = do
  let l = 1000
  let a = ExampleSum
  reportMain (List.intercalate "-" [show a, show l]) $ testExample (examplePattern a l)

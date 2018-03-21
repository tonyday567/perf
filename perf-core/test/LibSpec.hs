{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.DocTest

main :: IO ()
main = do
  putStrLn ("Perf.Cycle DocTest")
  doctest ["src/Perf/Cycle.hs"]
  putStrLn ("Perf.Measure DocTest")
  doctest ["src/Perf/Measure.hs"]
  putStrLn ("Perf DocTest")
  doctest ["src/Perf.hs"]


{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.DocTest

main :: IO ()
main = do
  putStrLn ("Perf.Cycle DocTest" :: Text)
  doctest ["src/Perf/Cycle.hs"]
  putStrLn ("Perf.Measure DocTest" :: Text)
  doctest ["src/Perf/Measure.hs"]
  putStrLn ("Perf DocTest" :: Text)
  doctest ["src/Perf.hs"]

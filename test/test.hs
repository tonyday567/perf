{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.DocTest

main :: IO ()
main = do
  putStrLn ("Perf.Cycle DocTest" :: Text)
  doctest ["src/Perf/Cycle.hs"]

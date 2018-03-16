{-# language OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Test.DocTest

-- import Data.Text

main :: IO ()
main = do
  putStrLn ("Perf.Cycle DocTest")
  doctest ["src/Perf/Cycle.hs"]
  putStrLn ("Perf.Measure DocTest")
  doctest ["src/Perf/Measure.hs"]
  putStrLn ("Perf DocTest")
  doctest ["src/Perf.hs"]


-- main :: IO ()
-- main = hspec spec

-- spec :: Spec
-- spec =
--   describe "Lib" $ do
--     it "works" $ do
--       True `shouldBe` True
--     prop "ourAdd is commutative" $ \x y ->
--       ourAdd x y `shouldBe` ourAdd y x

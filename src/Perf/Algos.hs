{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Algorithms and functions for testing purposes

module Perf.Algos
  ( AlgoType (..),
    AlgoApplication(..),
    parseAlgo,
    allAlgos,
    allApps,
    recSum,
    fuseConst,
    fuseSum,
    monoSum,
    polySum,
    lambdaSum,
    mapInc,
  ) where

import Data.Foldable
import Data.Text (Text)
import Options.Applicative

data AlgoType = AlgoRecSum | AlgoFuseSum | AlgoFuseConst | AlgoMonoSum | AlgoPolySum | AlgoLambdaSum | AlgoMapInc deriving (Eq, Show)

allAlgos :: [AlgoType]
allAlgos =
  [
    AlgoRecSum,
    AlgoFuseSum,
    AlgoFuseConst,
    AlgoMonoSum,
    AlgoPolySum,
    AlgoLambdaSum,
    AlgoMapInc
  ]

data AlgoApplication a =
  ApplicationFuseSum Text (Int -> Int) Int |
  ApplicationFuseConst Text (Int -> ()) Int |
  ApplicationRecSum Text ((Num a) => [a] -> a) [a] |
  ApplicationMonoSum Text ([Int] -> Int) [Int] |
  ApplicationPolySum Text ((Num a) => [a] -> a) [a] |
  ApplicationLambdaSum Text ([Int] -> Int) [a] |
  ApplicationMapInc Text ([Int] -> [Int]) [Int]

allApps :: Int -> [AlgoApplication Int]
allApps l =
  [ ApplicationFuseSum "fuseSum" fuseSum l,
    ApplicationFuseConst "fuseConst" fuseConst l,
    ApplicationRecSum "recSum" recSum [1..l],
    ApplicationMonoSum "monoSum" monoSum [1..l],
    ApplicationPolySum "polySum" polySum [1..l],
    ApplicationLambdaSum "lambdaSum" lambdaSum [1..l],
    ApplicationMapInc "mapInc" mapInc [1..l]
  ]

parseAlgo :: Parser AlgoType
parseAlgo =
  flag' AlgoFuseSum (long "fsum" <> help "fused sum") <|>
  flag' AlgoFuseConst (long "fconst" <> help "fused const") <|>
  flag' AlgoMonoSum (long "monosum" <> help "monomorphic sum") <|>
  flag' AlgoPolySum (long "polysum" <> help "polymorphic sum") <|>
  flag' AlgoLambdaSum (long "lambdasum" <> help "lambdaed sum") <|>
  flag' AlgoRecSum (long "recsum" <> help "recursion sum") <|>
  flag' AlgoMapInc (long "mapinc" <> help "map inc") <|>
  pure AlgoFuseSum

-- various algos
recSum :: (Num a) => [a] -> a
recSum = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc+x) xs

fuseConst :: Int -> ()
fuseConst x = foldl' const () [1 .. x]

fuseSum :: Int -> Int
fuseSum x = sum [1 .. x]

monoSum :: [Int] -> Int
monoSum xs = foldl' (+) 0 xs

polySum :: (Num b) => [b] -> b
polySum xs = foldl' (+) 0 xs

lambdaSum :: [Int] -> Int
lambdaSum = \xs -> foldl' (+) 0 xs

mapInc :: [Int] -> [Int]
mapInc xs = fmap (+1) xs

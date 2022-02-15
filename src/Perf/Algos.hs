{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use sum" #-}

-- | Algorithms and functions for testing purposes

module Perf.Algos
  ( AlgoType (..),
    AlgoApplication(..),
    parseAlgo,
    allAlgos,
    allApps,
    SumAlgo (..),
    allSums,

    -- * sums
    recSum,
    fuseSum,
    fuseFoldl'Sum,
    fuseFoldrSum,
    tailSum,
    tailSumGo,
    cataSum,
    foldrSum,
    monoSum,
    polySum,
    lambdaSum,
    sumF,
    sumR,

    -- * length
    lengthTail,
    lengthTailLazy,
    lengthPoly,
    lengthMonoMaybe,
    lengthr,
    lengthR,
    lengthR',
    lengthRec,
    lengthCase,
    lengthAux,

    -- * counting
    maybeCount,
    maybeCountLazy,
    maybeCountR,


    -- * recursive patterns
    recurseTail,
    recurseCo,
    recurseTailLazy,
    accTail,
    accN,

    -- * other
    mapInc,
    fuseConst,
    splitHalf,

  ) where

import Data.Foldable
import Data.Text (Text)
import Options.Applicative
import Data.Bifunctor
import Data.Bool
import Data.Functor.Foldable

data AlgoType = AlgoRecSum | AlgoFuseSum | AlgoFuseConst | AlgoMonoSum | AlgoPolySum | AlgoLambdaSum deriving (Eq, Show)

allAlgos :: [AlgoType]
allAlgos =
  [
    AlgoRecSum,
    AlgoFuseSum,
    AlgoFuseConst,
    AlgoMonoSum,
    AlgoPolySum,
    AlgoLambdaSum
  ]

data AlgoApplication a =
  ApplicationFuseSum Text (Int -> Int) Int |
  ApplicationFuseConst Text (Int -> ()) Int |
  ApplicationRecSum Text ((Num a) => [a] -> a) [a] |
  ApplicationMonoSum Text ([Int] -> Int) [Int] |
  ApplicationPolySum Text ((Num a) => [a] -> a) [a] |
  ApplicationLambdaSum Text ([Int] -> Int) [a]

data SumAlgo a =
  SumFuse Text (Int -> Int) Int |
  SumPoly Text ((Num a) => [a] -> a) [a] |
  SumMono Text ([Int] -> Int) [Int] |
  SumLambda Text ([Int] -> Int) [a]

allSums :: Int -> [SumAlgo Int]
allSums l =
  [ SumFuse "fuseSum" fuseSum l,
    SumFuse "fuseFoldl'Sum" fuseFoldl'Sum l,
    SumFuse "fuseFoldrSum" fuseFoldrSum l,
    SumPoly "tailSum" tailSum [1..l],
    SumPoly "tailSumGo" tailSumGo [1..l],
    SumPoly "foldrSum" foldrSum [1..l],
    SumPoly "recSum" recSum [1..l],
    SumPoly "cataSum" cataSum [1..l],
    SumMono "monoSum" monoSum [1..l],
    SumPoly "polySum" polySum [1..l],
    SumLambda "lambdaSum" lambdaSum [1..l]
  ]

allApps :: Int -> [AlgoApplication Int]
allApps l =
  [ ApplicationFuseSum "fuseSum" fuseSum l,
    ApplicationFuseConst "fuseConst" fuseConst l,
    ApplicationRecSum "recSum" recSum [1..l],
    ApplicationMonoSum "monoSum" monoSum [1..l],
    ApplicationPolySum "polySum" polySum [1..l],
    ApplicationLambdaSum "lambdaSum" lambdaSum [1..l]
  ]

parseAlgo :: Parser AlgoType
parseAlgo =
  flag' AlgoFuseSum (long "fsum" <> help "fused sum") <|>
  flag' AlgoFuseConst (long "fconst" <> help "fused const") <|>
  flag' AlgoMonoSum (long "monosum" <> help "monomorphic sum") <|>
  flag' AlgoPolySum (long "polysum" <> help "polymorphic sum") <|>
  flag' AlgoLambdaSum (long "lambdasum" <> help "lambdaed sum") <|>
  flag' AlgoRecSum (long "recsum" <> help "recursion sum") <|>
  pure AlgoFuseSum

-- various sums

-- fusion pipelines
fuseSum :: Int -> Int
fuseSum x = sum [1 .. x]

fuseFoldl'Sum :: Int -> Int
fuseFoldl'Sum x = foldl' (+) 0 [1 .. x]

fuseFoldrSum :: Int -> Int
fuseFoldrSum x = foldr (+) 0 [1 .. x]

-- foldr sums
tailSum :: (Num a) => [a] -> a
tailSum [] = 0
tailSum (x:xs) = x + tailSum xs

tailSumGo :: (Num a) => [a] -> a
tailSumGo xs = go xs
  where
    go [] = 0
    go (x:xs) = x + go xs

foldrSum :: (Num a) => [a] -> a
foldrSum xs = foldr (+) 0 xs


-- * recursive sums
recSum :: (Num a) => [a] -> a
recSum = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (x+acc) xs

cataSum :: [Int] -> Int
cataSum= cata $ \case
  Nil -> 0
  Cons x acc -> x + acc

monoSum :: [Int] -> Int
monoSum xs = foldl' (+) 0 xs

polySum :: (Num b) => [b] -> b
polySum xs = foldl' (+) 0 xs

lambdaSum :: [Int] -> Int
lambdaSum = \xs -> foldl' (+) 0 xs


-- * incrementals

fuseConst :: Int -> ()
fuseConst x = foldl' const () [1 .. x]

mapInc :: [Int] -> [Int]
mapInc xs = fmap (+1) xs

lengthRec :: [a] -> Int
lengthRec [] = 0
lengthRec (_:xs) = 1 + lengthRec xs

lengthCase :: [a] -> Int
lengthCase = \case
  [] -> 0
  (_:xs) -> 1 + lengthCase xs

lengthAux :: [a] -> Int
lengthAux = \case
  [] -> b
  (x:xs) -> f x (lengthAux xs)
  where
    b = 0
    f _ xs = 1 + xs

lengthR :: [a] -> Int
lengthR = foldr f b
  where
    b = 0
    f _ xs = 1 + xs

lengthR' :: [a] -> Int
lengthR' = foldr (const (1+)) 0

{-
-- from base:
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC.List.html#length
-- The lambda form turns out to be necessary to make this inline
-- when we need it to and give good performance.
{-# INLINE [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r !a = r (a + 1)

-}

-- just some interesting algorithms
splitHalf :: [a] -> ([a],[a])
splitHalf xs = go xs xs
  where
    go (y:ys) (_:_:zs) = first (y:) (go ys zs)
    go ys _ = ([],ys)

lengthTail :: [Int] -> Int
lengthTail xs0 = go 0 xs0
  where
    go c [] = c
    go c (_:xs) = go (c+1) $! xs

lengthTailLazy :: [Int] -> Int
lengthTailLazy xs0 = go 0 xs0
  where
    go c [] = c
    go c (_:xs) = go (c+1) xs

lengthF :: (Num a) => x -> (a -> a) -> a -> a
lengthF _ r = \ !a -> r (a+1)

lengthr :: [Int] -> Int
lengthr xs0 = foldr lengthF id xs0 0

lengthPoly :: [a] -> Int
lengthPoly xs0 = foldr lengthF id xs0 0

lengthMonoMaybe :: [Maybe Int] -> Int
lengthMonoMaybe xs0 = foldr lengthF id xs0 0

maybeCount :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCount p xs0 = go 0 xs0
  where
    go :: Int -> [Maybe Int] -> Maybe Int
    go c [] = Just c
    go _ (Nothing:_) = Nothing
    go c (Just x:xs) =
      go (bool c (c+1) (p x)) $! xs

maybeCountLazy :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountLazy p xs0 = go 0 xs0
  where
    go :: Int -> [Maybe Int] -> Maybe Int
    go c [] = Just c
    go _ (Nothing:_) = Nothing
    go c (Just x:xs) =
      if p x then go (c + 1) xs else go c xs

sumF :: (Num a) => a -> (a -> a) -> a -> a
sumF x r = \ !a -> r (x + a)

sumR :: [Int] -> Int
sumR xs = foldr sumF id xs 0

maybeCountF :: (Int -> Bool) -> Maybe Int -> (Maybe Int -> Maybe Int) -> Maybe Int -> Maybe Int
maybeCountF p x r = \ !a ->
  case x of
    Nothing -> Nothing
    Just x' -> if p x' then r (fmap (+1) a) else r a

maybeCountR :: Foldable t => (Int -> Bool) -> t (Maybe Int) -> Maybe Int
maybeCountR p xs0 = foldr (maybeCountF p) id xs0 (Just 0)

recurseTail :: (a -> b -> b) -> b -> [a] -> b
recurseTail f = go
  where
    go s [] = s
    go s (x:xs) = go (f x s) $! xs

recurseTailLazy :: (a -> b -> b) -> b -> [a] -> b
recurseTailLazy f = go
  where
    go s [] = s
    go s (x:xs) = go (f x s) xs

accTail :: (Integral a) => a -> a
accTail x = go x 1 where
    go 1 y = y
    go x y = go (x-1) $! (x+y)

accN :: (Integral a) => a -> a
accN 1 = 1
accN x = x + accN (x-1)

recurseCo :: (a -> b -> b) -> b -> [a] -> b
recurseCo f s0 = go
  where
    go [] = s0
    go (x:xs) = f x (go xs)

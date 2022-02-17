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
  (
    -- * function application unifications
    AlgoExample (..),
    allAlgoExamples,
    parseAlgoExample,
    ExamplePattern (..),
    examplePattern,
    exampleLabel,

    SumPattern (..),
    allSums,

    LengthPattern (..),
    allLengths,

    -- * sums
    sumTail,
    sumTailLazy,
    sumFlip,
    sumFlipLazy,
    sumCo,
    sumCoGo,
    sumCoCase,
    sumAux,
    sumFoldr,
    sumCata,
    sumSum,
    sumMono,
    sumPoly,
    sumLambda,
    sumF,
    sumFuse,
    sumFusePoly,
    sumFuseFoldl',
    sumFuseFoldr,

    -- * length
    lengthTail,
    lengthTailLazy,
    lengthFlip,
    lengthFlipLazy,
    lengthCo,
    lengthCoCase,
    lengthAux,
    lengthFoldr,
    lengthFoldrConst,
    lengthF,
    lengthFMono,

    -- * recursion patterns
    recurseTail,
    recurseTailLazy,
    recurseFlip,
    recurseFlipLazy,
    recurseCo,
    recurseCoLazy,
    recurseCata,

    -- * miscellaneous
    mapInc,
    constFuse,
    splitHalf,
  ) where

import Data.Foldable
import Data.Text (Text)
import Options.Applicative
import Data.Bifunctor
import Data.Functor.Foldable

-- | Algorithm examples for testing
data AlgoExample = ExampleSumFuse | ExampleSum | ExampleLengthF | ExampleConstFuse | ExampleMapInc deriving (Eq, Show)

allAlgoExamples :: [AlgoExample]
allAlgoExamples =
  [
    ExampleSumFuse,
    ExampleSum,
    ExampleLengthF,
    ExampleConstFuse,
    ExampleMapInc
  ]

parseAlgoExample :: Parser AlgoExample
parseAlgoExample =
  flag' ExampleSumFuse (long "sumFuse" <> help "fused sum pipeline") <|>
  flag' ExampleSum (long "sum" <> help "sum") <|>
  flag' ExampleLengthF (long "lengthF" <> help "foldr id length") <|>
  flag' ExampleConstFuse (long "constFuse" <> help "fused const pipeline") <|>
  flag' ExampleMapInc (long "mapInc" <> help "fmap (+1)") <|>
  pure ExampleSum

-- | Unification of example function applications
data ExamplePattern a =
  PatternSumFuse Text ((Num a) => (a -> a)) a |
  PatternSum Text ((Num a) => [a] -> a) [a] |
  PatternLengthF Text ([a] -> Int) [a] |
  PatternConstFuse Text (Int -> ()) Int |
  PatternMapInc Text ([Int] -> [Int]) [Int]

exampleLabel :: ExamplePattern a -> Text
exampleLabel (PatternSumFuse l _ _) = l
exampleLabel (PatternSum l _ _) = l
exampleLabel (PatternLengthF l _ _) = l
exampleLabel (PatternConstFuse l _ _) = l
exampleLabel (PatternMapInc l _ _) = l


examplePattern :: AlgoExample -> Int -> ExamplePattern Int
examplePattern ExampleSumFuse l = PatternSumFuse "sumFuse" sumFuse l
examplePattern ExampleSum l = PatternSum "sum" sum [1..l]
examplePattern ExampleLengthF l = PatternLengthF "lengthF" lengthF [1..l]
examplePattern ExampleConstFuse l = PatternConstFuse "constFuse" constFuse l
examplePattern ExampleMapInc l = PatternMapInc "mapInc" mapInc [1..l]

-- | Unification of sum function applications
data SumPattern a =
  SumFuse Text (Int -> Int) Int |
  SumFusePoly Text ((Enum a, Num a) => a -> a) a |
  SumPoly Text ((Num a) => [a] -> a) [a] |
  SumMono Text ([Int] -> Int) [Int]

allSums :: Int -> [SumPattern Int]
allSums l =
  [ SumPoly "sumTail" sumTail [1..l],
    SumPoly "sumTailLazy" sumTailLazy [1..l],
    SumPoly "sumFlip" sumFlip [1..l],
    SumPoly "sumFlipLazy" sumFlipLazy [1..l],
    SumPoly "sumCo" sumCo [1..l],
    SumPoly "sumCoGo" sumCoGo [1..l],
    SumPoly "sumCoCase" sumCoCase [1..l],
    SumPoly "sumAux" sumAux [1..l],
    SumPoly "sumFoldr" sumFoldr [1..l],
    SumPoly "sumCata" sumCata [1..l],
    SumPoly "sumSum" sumSum [1..l],
    SumMono "sumMono" sumMono [1..l],
    SumPoly "sumPoly" sumPoly [1..l],
    SumPoly "sumLambda" sumLambda [1..l],
    SumPoly "sumF" sumF [1..l],
    SumFuse "sumFuse" sumFuse l,
    SumFusePoly "sumFusePoly" sumFusePoly l,
    SumFuse "sumFuseFoldl'" sumFuseFoldl' l,
    SumFuse "sumFuseFoldr" sumFuseFoldr l
  ]

-- | Unification of sum function applications
data LengthPattern a =
  LengthPoly Text ([a] -> Int) [a] |
  LengthMono Text ([Int] -> Int) [Int]

allLengths :: Int -> [LengthPattern Int]
allLengths l =
  [ LengthPoly "lengthTail" lengthTail [1..l],
    LengthPoly "lengthTailLazy" lengthTailLazy [1..l],
    LengthPoly "lengthFlip" lengthFlip [1..l],
    LengthPoly "lengthFlipLazy" lengthFlipLazy [1..l],
    LengthPoly "lengthCo" lengthCo [1..l],
    LengthPoly "lengthCoCase" lengthCoCase [1..l],
    LengthPoly "lengthAux" lengthAux [1..l],
    LengthPoly "lengthFoldr" lengthFoldr [1..l],
    LengthPoly "lengthFoldrConst" lengthFoldrConst [1..l],
    LengthPoly "lengthF" lengthF [1..l],
    LengthMono "lengthFMono" lengthFMono [1..l]
  ]

-- various sums
sumTail :: (Num a) => [a] -> a
sumTail = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (x+acc) $! xs

sumTailLazy :: (Num a) => [a] -> a
sumTailLazy = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (x+acc) $! xs

sumFlip :: (Num a) => [a] -> a
sumFlip xs0 = go xs0 0
  where
    go [] s = s
    go (x:xs) s = go xs $! x + s

sumFlipLazy :: (Num a) => [a] -> a
sumFlipLazy xs0 = go xs0 0
  where
    go [] s = s
    go (x:xs) s = go xs $ x + s

sumCo :: (Num a) => [a] -> a
sumCo [] = 0
sumCo (x:xs) = x + sumCo xs

sumCoGo :: (Num a) => [a] -> a
sumCoGo = go
  where
    go [] = 0
    go (x:xs) = x + go xs

sumCoCase :: (Num a) => [a] -> a
sumCoCase = \case
  [] -> 0
  (x:xs) -> x + sumCoCase xs

sumAux :: (Num a) => [a] -> a
sumAux = \case
  [] -> b
  (x:xs) -> f x (sumAux xs)
  where
    b = 0
    f x xs = x + xs

sumFoldr :: (Num a) => [a] -> a
sumFoldr xs = foldr (+) 0 xs

sumCata :: (Num a) => [a] -> a
sumCata = cata $ \case
  Nil -> 0
  Cons x acc -> x + acc

sumSum :: (Num a) => [a] -> a
sumSum xs = sum xs

sumMono :: [Int] -> Int
sumMono xs = foldl' (+) 0 xs

sumPoly :: (Num a) => [a] -> a
sumPoly xs = foldl' (+) 0 xs

sumLambda :: (Num a) => [a] -> a
sumLambda = \xs -> foldl' (+) 0 xs

sumF' :: (Num a) => a -> (a -> a) -> a -> a
sumF' x r = \ !a -> r (x + a)

sumF :: (Num a) => [a] -> a
sumF xs = foldr sumF' id xs 0

sumFuse :: Int -> Int
sumFuse x = sum [1 .. x]

sumFusePoly :: (Enum a, Num a) => a -> a
sumFusePoly x = sum [1 .. x]

sumFuseFoldl' :: Int -> Int
sumFuseFoldl' x = foldl' (+) 0 [1 .. x]

sumFuseFoldr :: Int -> Int
sumFuseFoldr x = foldr (+) 0 [1 .. x]

-- * lengths
lengthTail :: [a] -> Int
lengthTail xs0 = go 0 xs0
  where
    go s [] = s
    go s (_:xs) = go (s+1) $! xs

lengthTailLazy :: [a] -> Int
lengthTailLazy xs0 = go 0 xs0
  where
    go s [] = s
    go s (_:xs) = go (s+1) xs

lengthFlip :: [a] -> Int
lengthFlip xs0 = go xs0 0
  where
    go [] s = s
    go (_:xs) s = go xs $! s + 1

lengthFlipLazy :: [a] -> Int
lengthFlipLazy xs0 = go xs0 0
  where
    go [] s = s
    go (_:xs) s = go xs $ s + 1

lengthCo :: [a] -> Int
lengthCo [] = 0
lengthCo (_:xs) = 1 + lengthCo xs

lengthCoCase :: [a] -> Int
lengthCoCase = \case
  [] -> 0
  (_:xs) -> 1 + lengthCoCase xs

lengthAux :: [a] -> Int
lengthAux = \case
  [] -> b
  (x:xs) -> f x (lengthAux xs)
  where
    b = 0
    f _ xs = 1 + xs

lengthFoldr :: [a] -> Int
lengthFoldr = foldr f b
  where
    b = 0
    f _ xs = 1 + xs

lengthFoldrConst :: [a] -> Int
lengthFoldrConst = foldr (const (1+)) 0
{-
-- from base:
-- https://hackage.haskell.org/package/base-4.16.0.0/docs/src/GHC.List.html#length
-- The lambda form turns out to be necessary to make this inline
-- when we need it to and give good performance.
{-# INLINE [0] lengthFB #-}
lengthFB :: x -> (Int -> Int) -> Int -> Int
lengthFB _ r !a = r (a + 1)

-}
lengthF' :: (Num a) => x -> (a -> a) -> a -> a
lengthF' _ r = \ !a -> r (a+1)

lengthF :: [a] -> Int
lengthF xs0 = foldr lengthF' id xs0 0

lengthFMono :: [Int] -> Int
lengthFMono xs0 = foldr lengthF' id xs0 0

-- * recursion patterns
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

recurseFlip :: (a -> b -> b) -> b -> [a] -> b
recurseFlip f s0 xs0 = go xs0 s0
  where
    go [] s = s
    go (x:xs) s = go xs $! f x s

recurseFlipLazy :: (a -> b -> b) -> b -> [a] -> b
recurseFlipLazy f s0 xs0 = go xs0 s0
  where
    go [] s = s
    go (x:xs) s = go xs $ f x s

recurseCo :: (a -> b -> b) -> b -> [a] -> b
recurseCo f s0 = go
  where
    go [] = s0
    go (x:xs) = f x $! go xs

recurseCoLazy :: (a -> b -> b) -> b -> [a] -> b
recurseCoLazy f s0 = go
  where
    go [] = s0
    go (x:xs) = f x $ go xs

recurseCata :: (a -> b -> b) -> b -> [a] -> b
recurseCata f s0 = cata $ \case
  Nil -> s0
  Cons x acc -> f x acc

-- * miscellaneous
constFuse :: Int -> ()
constFuse x = foldl' const () [1 .. x]

mapInc :: [Int] -> [Int]
mapInc xs = fmap (+1) xs

splitHalf :: [a] -> ([a],[a])
splitHalf xs = go xs xs
  where
    go (y:ys) (_:_:zs) = first (y:) (go ys zs)
    go ys _ = ([],ys)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use sum" #-}

-- | Algorithms and functions for testing purposes
module Perf.Algos
  ( -- * command-line options
    Example (..),
    parseExample,
    ExamplePattern (..),
    examplePattern,
    exampleLabel,
    testExample,
    tastyExample,

    -- * sum algorithms
    SumPattern (..),
    allSums,
    testSum,
    statSums,
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

    -- * length algorithms
    LengthPattern (..),
    allLengths,
    testLength,
    statLengths,

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
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Foldable
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf.Types
import Test.Tasty.Bench

-- | Algorithm examples for testing
data Example = ExampleSumFuse | ExampleSum | ExampleLengthF | ExampleConstFuse | ExampleMapInc | ExampleNoOp | ExampleNub | ExampleFib deriving (Eq, Show)

-- | Parse command-line options for algorithm examples.
parseExample :: Parser Example
parseExample =
  flag' ExampleSumFuse (long "sumFuse" <> help "fused sum pipeline")
    <|> flag' ExampleSum (long "sum" <> style (annotate bold) <> help "sum")
    <|> flag' ExampleLengthF (long "lengthF" <> help "foldr id length")
    <|> flag' ExampleConstFuse (long "constFuse" <> help "fused const pipeline")
    <|> flag' ExampleMapInc (long "mapInc" <> help "fmap (+1)")
    <|> flag' ExampleNoOp (long "noOp" <> help "const ()")
    <|> flag' ExampleFib (long "fib" <> help "fibonacci")
    <|> flag' ExampleNub (long "nub" <> help "List.nub")
    <|> pure ExampleSum

-- | Unification of example function applications
data ExamplePattern a
  = PatternSumFuse Text ((Num a) => (a -> a)) a
  | PatternSum Text ((Num a) => [a] -> a) [a]
  | PatternLengthF Text ([a] -> Int) [a]
  | PatternConstFuse Text (Int -> ()) Int
  | PatternMapInc Text ([Int] -> [Int]) [Int]
  | PatternNoOp Text (() -> ()) ()
  | PatternNub Text ([Int] -> [Int]) [Int]
  | PatternFib Text (Int -> Integer) Int

-- | Labels
exampleLabel :: ExamplePattern a -> Text
exampleLabel (PatternSumFuse l _ _) = l
exampleLabel (PatternSum l _ _) = l
exampleLabel (PatternLengthF l _ _) = l
exampleLabel (PatternConstFuse l _ _) = l
exampleLabel (PatternMapInc l _ _) = l
exampleLabel (PatternNoOp l _ _) = l
exampleLabel (PatternNub l _ _) = l
exampleLabel (PatternFib l _ _) = l

-- | Convert an 'Example' to an 'ExamplePattern'.
examplePattern :: Example -> Int -> ExamplePattern Int
examplePattern ExampleSumFuse l = PatternSumFuse "sumFuse" sumFuse l
examplePattern ExampleSum l = PatternSum "sum" sum [1 .. l]
examplePattern ExampleLengthF l = PatternLengthF "lengthF" lengthF [1 .. l]
examplePattern ExampleConstFuse l = PatternConstFuse "constFuse" constFuse l
examplePattern ExampleMapInc l = PatternMapInc "mapInc" mapInc [1 .. l]
examplePattern ExampleNoOp _ = PatternNoOp "noop" (const ()) ()
examplePattern ExampleNub l = PatternNub "nub" List.nub [1 .. l]
examplePattern ExampleFib l = PatternFib "fib" fib l

-- | Convert an 'ExamplePattern' to a 'PerfT'.
testExample :: (Semigroup a, MonadIO m) => ExamplePattern Int -> PerfT m a ()
testExample (PatternSumFuse label f a) = void $ ffap label f a
testExample (PatternSum label f a) = void $ ffap label f a
testExample (PatternLengthF label f a) = void $ ffap label f a
testExample (PatternConstFuse label f a) = void $ ffap label f a
testExample (PatternMapInc label f a) = void $ ffap label f a
testExample (PatternNoOp label f a) = void $ ffap label f a
testExample (PatternNub label f a) = void $ ffap label f a
testExample (PatternFib label f a) = void $ ffap label f a

-- | Convert an 'ExamplePattern' to a tasty-bench run.
tastyExample :: ExamplePattern Int -> Benchmarkable
tastyExample (PatternSumFuse _ f a) = nf f a
tastyExample (PatternSum _ f a) = nf f a
tastyExample (PatternLengthF _ f a) = nf f a
tastyExample (PatternConstFuse _ f a) = nf f a
tastyExample (PatternMapInc _ f a) = nf f a
tastyExample (PatternNoOp _ f a) = nf f a
tastyExample (PatternNub _ f a) = nf f a
tastyExample (PatternFib _ f a) = nf f a

-- | Unification of sum function applications
data SumPattern a
  = SumFuse Text (Int -> Int) Int
  | SumFusePoly Text ((Enum a, Num a) => a -> a) a
  | SumPoly Text ((Num a) => [a] -> a) [a]
  | SumMono Text ([Int] -> Int) [Int]

-- | All the sum algorithms.
allSums :: Int -> [SumPattern Int]
allSums l =
  [ SumPoly "sumTail" sumTail [1 .. l],
    SumPoly "sumTailLazy" sumTailLazy [1 .. l],
    SumPoly "sumFlip" sumFlip [1 .. l],
    SumPoly "sumFlipLazy" sumFlipLazy [1 .. l],
    SumPoly "sumCo" sumCo [1 .. l],
    SumPoly "sumCoGo" sumCoGo [1 .. l],
    SumPoly "sumCoCase" sumCoCase [1 .. l],
    SumPoly "sumAux" sumAux [1 .. l],
    SumPoly "sumFoldr" sumFoldr [1 .. l],
    SumPoly "sumCata" sumCata [1 .. l],
    SumPoly "sumSum" sumSum [1 .. l],
    SumMono "sumMono" sumMono [1 .. l],
    SumPoly "sumPoly" sumPoly [1 .. l],
    SumPoly "sumLambda" sumLambda [1 .. l],
    SumPoly "sumF" sumF [1 .. l],
    SumFuse "sumFuse" sumFuse l,
    SumFusePoly "sumFusePoly" sumFusePoly l,
    SumFuse "sumFuseFoldl'" sumFuseFoldl' l,
    SumFuse "sumFuseFoldr" sumFuseFoldr l
  ]

-- | Convert an 'SumPattern' to a 'PerfT'.
testSum :: (Semigroup a, MonadIO m) => SumPattern Int -> PerfT m a Int
testSum (SumFuse label f a) = fap label f a
testSum (SumFusePoly label f a) = fap label f a
testSum (SumMono label f a) = fap label f a
testSum (SumPoly label f a) = fap label f a

-- | Run a sum algorithm measurement.
statSums :: (MonadIO m) => Int -> Int -> (Int -> Measure m [a]) -> m (Map.Map Text [a])
statSums n l m = execPerfT (m n) $ mapM_ testSum (allSums l)

-- | tail resursive
sumTail :: (Num a) => [a] -> a
sumTail = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (x + acc) $! xs

-- | lazy recursion.
sumTailLazy :: (Num a) => [a] -> a
sumTailLazy = go 0
  where
    go acc [] = acc
    go acc (x : xs) = go (x + acc) $! xs

-- | With argument order flipped
sumFlip :: (Num a) => [a] -> a
sumFlip xs0 = go xs0 0
  where
    go [] s = s
    go (x : xs) s = go xs $! x + s

-- | Lazy with argument order flipped.
sumFlipLazy :: (Num a) => [a] -> a
sumFlipLazy xs0 = go xs0 0
  where
    go [] s = s
    go (x : xs) s = go xs $ x + s

-- | Co-routine style
sumCo :: (Num a) => [a] -> a
sumCo [] = 0
sumCo (x : xs) = x + sumCo xs

-- | Co-routine, go style
sumCoGo :: (Num a) => [a] -> a
sumCoGo = go
  where
    go [] = 0
    go (x : xs) = x + go xs

-- | Co-routine, case-style
sumCoCase :: (Num a) => [a] -> a
sumCoCase = \case
  [] -> 0
  (x : xs) -> x + sumCoCase xs

-- | Auxillary style.
sumAux :: (Num a) => [a] -> a
sumAux = \case
  [] -> b
  (x : xs) -> f x (sumAux xs)
  where
    b = 0
    f x xs = x + xs

-- | foldr style
sumFoldr :: (Num a) => [a] -> a
sumFoldr xs = foldr (+) 0 xs

-- | cata style
sumCata :: (Num a) => [a] -> a
sumCata = cata $ \case
  Nil -> 0
  Cons x acc -> x + acc

-- | sum
sumSum :: (Num a) => [a] -> a
sumSum xs = sum xs

-- | Monomorphic sum
sumMono :: [Int] -> Int
sumMono xs = foldl' (+) 0 xs

-- | Polymorphic sum
sumPoly :: (Num a) => [a] -> a
sumPoly xs = foldl' (+) 0 xs

-- | Lambda-style sum
sumLambda :: (Num a) => [a] -> a
sumLambda = \xs -> foldl' (+) 0 xs

sumF' :: (Num a) => a -> (a -> a) -> a -> a
sumF' x r = \ !a -> r (x + a)

-- | GHC-style foldr method.
sumF :: (Num a) => [a] -> a
sumF xs = foldr sumF' id xs 0

-- | Fusion check
sumFuse :: Int -> Int
sumFuse x = sum [1 .. x]

-- | Fusion under polymorph
sumFusePoly :: (Enum a, Num a) => a -> a
sumFusePoly x = sum [1 .. x]

-- | foldl' fusion
sumFuseFoldl' :: Int -> Int
sumFuseFoldl' x = foldl' (+) 0 [1 .. x]

-- | foldr fusion
sumFuseFoldr :: Int -> Int
sumFuseFoldr x = foldr (+) 0 [1 .. x]

-- | Unification of length function applications
data LengthPattern a
  = LengthPoly Text ([a] -> Int) [a]
  | LengthMono Text ([Int] -> Int) [Int]

-- | All the length algorithms.
allLengths :: Int -> [LengthPattern Int]
allLengths l =
  [ LengthPoly "lengthTail" lengthTail [1 .. l],
    LengthPoly "lengthTailLazy" lengthTailLazy [1 .. l],
    LengthPoly "lengthFlip" lengthFlip [1 .. l],
    LengthPoly "lengthFlipLazy" lengthFlipLazy [1 .. l],
    LengthPoly "lengthCo" lengthCo [1 .. l],
    LengthPoly "lengthCoCase" lengthCoCase [1 .. l],
    LengthPoly "lengthAux" lengthAux [1 .. l],
    LengthPoly "lengthFoldr" lengthFoldr [1 .. l],
    LengthPoly "lengthFoldrConst" lengthFoldrConst [1 .. l],
    LengthPoly "lengthF" lengthF [1 .. l],
    LengthMono "lengthFMono" lengthFMono [1 .. l]
  ]

-- | Convert an 'LengthPattern' to a 'PerfT'.
testLength :: (Semigroup a, MonadIO m) => LengthPattern Int -> PerfT m a Int
testLength (LengthMono label f a) = fap label f a
testLength (LengthPoly label f a) = fap label f a

-- | Run a lengths algorithm
statLengths :: (MonadIO m) => Int -> Int -> (Int -> Measure m [a]) -> m (Map.Map Text [a])
statLengths n l m = execPerfT (m n) $ mapM_ testLength (allLengths l)

-- | tail resursive
lengthTail :: [a] -> Int
lengthTail xs0 = go 0 xs0
  where
    go s [] = s
    go s (_ : xs) = go (s + 1) $! xs

-- | lazy recursion.
lengthTailLazy :: [a] -> Int
lengthTailLazy xs0 = go 0 xs0
  where
    go s [] = s
    go s (_ : xs) = go (s + 1) xs

-- | With argument order flipped
lengthFlip :: [a] -> Int
lengthFlip xs0 = go xs0 0
  where
    go [] s = s
    go (_ : xs) s = go xs $! s + 1

-- | Lazy with argument order flipped.
lengthFlipLazy :: [a] -> Int
lengthFlipLazy xs0 = go xs0 0
  where
    go [] s = s
    go (_ : xs) s = go xs $ s + 1

-- | Co-routine style
lengthCo :: [a] -> Int
lengthCo [] = 0
lengthCo (_ : xs) = 1 + lengthCo xs

-- | Co-routine style as a Case statement.
lengthCoCase :: [a] -> Int
lengthCoCase = \case
  [] -> 0
  (_ : xs) -> 1 + lengthCoCase xs

-- | Auxillary version.
lengthAux :: [a] -> Int
lengthAux = \case
  [] -> b
  (x : xs) -> f x (lengthAux xs)
  where
    b = 0
    f _ xs = 1 + xs

-- | foldr style
lengthFoldr :: [a] -> Int
lengthFoldr = foldr f b
  where
    b = 0
    f _ xs = 1 + xs

-- | foldr style with explicit const usage.
lengthFoldrConst :: [a] -> Int
lengthFoldrConst = foldr (const (1 +)) 0

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
lengthF' _ r = \ !a -> r (a + 1)

-- | GHC style
lengthF :: [a] -> Int
lengthF xs0 = foldr lengthF' id xs0 0

-- | Monomorphic, GHC style
lengthFMono :: [Int] -> Int
lengthFMono xs0 = foldr lengthF' id xs0 0

-- * recursion patterns

-- | Tail recursion
recurseTail :: (a -> b -> b) -> b -> [a] -> b
recurseTail f = go
  where
    go s [] = s
    go s (x : xs) = go (f x s) $! xs

-- | Lazy tail recursion
recurseTailLazy :: (a -> b -> b) -> b -> [a] -> b
recurseTailLazy f = go
  where
    go s [] = s
    go s (x : xs) = go (f x s) xs

-- | Tail resursion with flipped argument order.
recurseFlip :: (a -> b -> b) -> b -> [a] -> b
recurseFlip f s0 xs0 = go xs0 s0
  where
    go [] s = s
    go (x : xs) s = go xs $! f x s

-- | Lazy tail resursion with flipped argument order.
recurseFlipLazy :: (a -> b -> b) -> b -> [a] -> b
recurseFlipLazy f s0 xs0 = go xs0 s0
  where
    go [] s = s
    go (x : xs) s = go xs $ f x s

-- | Coroutine
recurseCo :: (a -> b -> b) -> b -> [a] -> b
recurseCo f s0 = go
  where
    go [] = s0
    go (x : xs) = f x $! go xs

-- | Lazy, coroutine
recurseCoLazy :: (a -> b -> b) -> b -> [a] -> b
recurseCoLazy f s0 = go
  where
    go [] = s0
    go (x : xs) = f x $ go xs

-- | Cata style
recurseCata :: (a -> b -> b) -> b -> [a] -> b
recurseCata f s0 = cata $ \case
  Nil -> s0
  Cons x acc -> f x acc

-- * miscellaneous

-- | Test of const fusion
constFuse :: Int -> ()
constFuse x = foldl' const () [1 .. x]

-- | Increment a list.
mapInc :: [Int] -> [Int]
mapInc xs = fmap (+ 1) xs

-- | Split a list.
splitHalf :: [a] -> ([a], [a])
splitHalf xs = go xs xs
  where
    go (y : ys) (_ : _ : zs) = first (y :) (go ys zs)
    go ys _ = ([], ys)

-- | Fibonnacci
fib :: Int -> Integer
fib n = if n < 2 then toInteger n else fib (n - 1) + fib (n - 2)

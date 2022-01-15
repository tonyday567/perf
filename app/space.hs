{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}

import Perf.Algos
import Prelude
import System.Environment (getArgs)
import Control.Monad

validate10 :: (Ord a, Num a) => a -> Maybe a
validate10 x
  | x < 1 || x > 10 = Nothing
  | otherwise = Just x

validateGeneric :: (a -> Bool) -> a -> Maybe a
validateGeneric p x
  | p x = Just x
  | otherwise = Nothing

-- sum: 500000500000
--          54,256 bytes allocated in the heap
testSum :: Int -> Int
testSum n =
  sum .
  take n $
  [1..]

-- inflist: 1000000
--          54,224 bytes allocated in the heap
testInfList :: Int -> Int
testInfList n =
  length .
  take n $
  [(1::Int)..]

-- A polymorphic list seems to badly effect space usage.
--
-- I think both [1..n] and [(1:Int)..] pin down the list to a concrete type.
--
-- inflistpoly: 1000000
--       16,055,384 bytes allocated in the heap
testInfListPoly :: Int -> Int
testInfListPoly n =
  length .
  take n $
  [1..]

-- flist: 1000000
--         54,104 bytes allocated in the heap
testFList :: Int -> Int
testFList n =
  length .
  take n $
  [1..n]

-- tail recursion for length creates significant heap allocation
-- lengthtail: 1000000
--      88,055,272 bytes allocated in the heap
testLengthTail :: Int -> Int
testLengthTail n =
  lengthTailLocal .
  take n $
  [1..n]

lengthTailLocal :: [Int] -> Int
lengthTailLocal xs0 = go 0 xs0
  where
    go c [] = c
    go c (_:xs) = go (c+1) $! xs

-- lengthtaillazy: 1000000
--       88,056,664 bytes allocated in the heap
testLengthTailLazy :: Int -> Int
testLengthTailLazy n =
  lengthTailLazy .
  take n $
  [1..n]

lengthF2 :: (Num a) => x -> (a -> a) -> a -> a
lengthF2 _ r = \ !a -> r (a+1)

lengthr2 :: [a] -> Int
lengthr2 xs0 = foldr lengthF2 id xs0 0

-- The right-associating CPS trick doesn't work here because the function is in another module.
--
-- What is the right inline incantation?
-- lengthr: 1000000
--      72,054,480 bytes allocated in the heap
testLengthR :: Int -> Int
testLengthR n =
  lengthr .
  take n $
  [(1::Int)..]

-- foldr (\ _ r !a -> r (a+1)) id xs0 0
--
-- This reproduces Data.List.length: the computation passes a continuation (a function) rather than a value and this associates the computation to the right.
--
-- lengthr: 1000000
--          54,296 bytes allocated in the heap
testLengthR2 :: Int -> Int
testLengthR2 n =
  lengthr2 .
  take n $
  [(1::Int)..]

-- Filter presevres constant space.
--
-- filter: 1
--          53,600 bytes allocated in the heap
testFilter :: Int -> Int
testFilter n =
  lengthr2 .
  filter (<=n) .
  take n $
  [1..n]

-- Introduction of a generic predicate, fmapping the Int list to a Maybe Int list.
--
-- validate: 1000000
--           54,800 bytes allocated in the heap
testValidate :: Int -> Int
testValidate n =
  length .
  filter (\x -> x == Nothing || x <= Just n) .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

-- maybeCount is a tail recursive attempt, which blows up significantly.
--
-- maybecount: Just 1000000
--      177,024,968 bytes allocated in the heap
testMaybeCount :: Int -> Maybe Int
testMaybeCount n =
  maybeCount (<= n) .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

-- The testXs are a series of transformations starting with testValidate
--
-- Adding Just.
--
-- x1: Just 1000000
--           55,144 bytes allocated in the heap
testX1 :: Int -> Maybe Int
testX1 n =
  Just .
  lengthr2 .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

-- cps version of maybeCount that returns an Int (compared with the Maybe Int of the problem).
--
-- x1: Just 1000000
--           55,144 bytes allocated in the heap
testXTest :: Int -> Int
testXTest n =
  maybeCountRTest (\x -> x == Nothing || x <= Just n) .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

maybeCountRTest :: (Maybe Int -> Bool) -> [Maybe Int] -> Int
maybeCountRTest p xs0 = foldr (maybeCountFTest p) id xs0 0

maybeCountFTest :: (Maybe Int -> Bool) -> Maybe Int -> (Int -> Int) -> Int -> Int
maybeCountFTest p x r = \ !a ->
  case x of
    Nothing -> 999
    Just _ -> case p x of
      True -> r (a+1)
      False -> r a

-- Using a negative Int as failure
--
-- Note: with any of the xtest tests, using the same function at 2 usage sites was detrimental to the results from both tests.
--
-- xtest2: Just 1000000
--           56,680 bytes allocated in the heap
--
testXTest2 :: Int -> Maybe Int
testXTest2 n =
  maybeCountRTest2 (\x -> x <= Just n) .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

maybeCountFTest2 :: (Maybe Int -> Bool) -> Maybe Int -> (Int -> Int) -> Int -> Int
maybeCountFTest2 p x r = \ !a ->
  case x of
    Nothing -> -1
    Just x' -> case p x of
      True -> r (a+1)
      False -> r a

maybeCountRTest2 :: (Maybe Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountRTest2 p xs0 = case foldr (maybeCountFTest2 p) id xs0 0 of
  -1 -> Nothing
  x -> Just x

-- case analysis on end result
--
-- xtest3: Just 1000000
--           56,680 bytes allocated in the heap
testXTest3 :: Int -> Maybe Int
testXTest3 n =
  maybeCountRTest3 (<= n) .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

maybeCountRTest3 :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountRTest3 p xs0 = case foldr (maybeCountFTest3 p) id xs0 0 of
  -1 -> Nothing
  x -> Just x

maybeCountFTest3 :: (Int -> Bool) -> Maybe Int -> (Int -> Int) -> Int -> Int
maybeCountFTest3 p x r = \ !a ->
  case x of
    Nothing -> -1
    Just x' -> case p x' of
      True -> r (a+1)
      False -> r a

-- Early termination and switch to (Maybe Int -> Maybe Int) basis for cps
--
-- testx4: Just 100000
--          56,992 bytes allocated in the heap
testXTest4 :: Int -> Maybe Int
testXTest4 n =
  maybeCountRTest4 (<= n) .
  fmap (validateGeneric (<= n)) .
  take n $
  [1..n]

maybeCountRTest4 :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountRTest4 p xs0 = foldr (maybeCountFTest4 p) id xs0 (Just 0)

maybeCountFTest4 :: (Int -> Bool) -> Maybe Int -> (Maybe Int -> Maybe Int) -> Maybe Int -> Maybe Int
maybeCountFTest4 p x r = \ !a ->
  case a of
    Nothing -> undefined
    Just a' ->
      case x of
        Nothing -> Nothing
        Just x' -> case p x' of
          True -> r (Just (a'+1))
          False -> r a

-- Substitute cycle [1..10] for better problem representation.
--
-- cycle: Just 100000
--          55,528 bytes allocated in the heap
testCycle :: Int -> Maybe Int
testCycle n =
  maybeCountRLocal (== 1) .
  fmap (validateGeneric (\x -> x >= 1 && x <= 10)) .
  take n $
  cycle [1..10]

maybeCountRLocal :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountRLocal p xs0 = case foldr (maybeCountFLocal p) id xs0 0 of
  -1 -> Nothing
  x -> Just x

maybeCountFLocal :: (Int -> Bool) -> Maybe Int -> (Int -> Int) -> Int -> Int
maybeCountFLocal p x r = \ !a ->
  case x of
    Nothing -> -1
    Just x' -> case p x' of
      True -> r (a+1)
      False -> r a

-- (Failed) attempt to switch to proper Maybe fold accumulator function.
--
-- cycle2: Just 100000
--       8,179,160 bytes allocated in the heap
testCycle2 :: Int -> Maybe Int
testCycle2 n =
  maybeCountRLocal' (== 1) .
  fmap (validateGeneric (\x -> x >= 1 && x <= 10)) .
  take n $
  cycle [1..10]

maybeCountRLocal' :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountRLocal' p xs0 = foldr (maybeCountFLocal' p) id xs0 (Just 0)

maybeCountFLocal' :: (Int -> Bool) -> Maybe Int -> (Maybe Int -> Maybe Int) -> Maybe Int -> Maybe Int
maybeCountFLocal' p x r = \ !a ->
  case a of
    Nothing -> undefined
    Just a' ->
      case x of
        Nothing -> Nothing
        Just x' -> case p x' of
          True -> r (Just (a'+1))
          False -> r a

-- cyclefail: Nothing
--          55,944 bytes allocated in the heap
testCycleFail :: Int -> Maybe Int
testCycleFail n =
  maybeCountRTest3'' (==1) .
  fmap validate10 .
  take n $
  cycle [1..11]

maybeCountRTest3'' :: (Int -> Bool) -> [Maybe Int] -> Maybe Int
maybeCountRTest3'' p xs0 = case foldr (maybeCountFTest3'' p) id xs0 0 of
  -1 -> Nothing
  x -> Just x

maybeCountFTest3'' :: (Int -> Bool) -> Maybe Int -> (Int -> Int) -> Int -> Int
maybeCountFTest3'' p x r = \ !a ->
  case x of
    Nothing -> -1
    Just x' -> case p x' of
      True -> r (a+1)
      False -> r a

-- * independent recursion tests

-- How else can left recursion proceed?
--
-- recursetail: 500000500000
--      88,055,536 bytes allocated in the heap
testRecurseTail :: Int -> Int
testRecurseTail n =
  recurseTail (+) 0 .
  take n $
  [1..n]

recurseTailLocal :: (a -> b -> b) -> b -> [a] -> b
recurseTailLocal f = go
  where
    go s [] = s
    go s (x:xs) = go (f x s) $! xs

-- recursetaillocal: 500000500000
--      88,059,688 bytes allocated in the heap
testRecurseTailLocal :: Int -> Int
testRecurseTailLocal n =
  recurseTailLocal (+) 0 .
  take n $
  [1..n]

-- recursetail: 500000500000
--      88,055,536 bytes allocated in the heap
testRecurseTailLazy :: Int -> Int
testRecurseTailLazy n =
  recurseTailLazy (+) 0 .
  take n $
  [1..n]

-- * starting with "not a list" recursion

-- recursetailnolist: 500000500000
--          58,768 bytes allocated in the heap
testRecurseTailNoList :: Integral a => a -> a
testRecurseTailNoList n =
  accTail n

-- Adding a list feature inside the function.
--
-- recursetailnolist2: 500000500000
--      72,061,240 bytes allocated in the heap
testRecurseTailNoList2 :: Int -> Int
testRecurseTailNoList2 n =
  accTail2 n

accTail2 :: Int -> Int
accTail2 n = go [1..n] 0 where
    go [] y = y
    go (x:xs) y = go xs $! (x+y)

-- recursetailnolist3: 500000500000
--       72,062,536 bytes allocated in the heap
testRecurseTailNoList3 :: Int -> Int
testRecurseTailNoList3 n =
  accTail3 n

accTail3 :: Int -> Int
accTail3 n = go 0 [1..n] where
    go y [] = y
    go y (x:xs) = go (x+y) $! xs

-- recursenolist: 500000500000
--      16,669,240 bytes allocated in the heap
testRecurseNoList :: Integral a => a -> a
testRecurseNoList n =
  acc n

-- * mapM experiment

-- mapM induces a left-bias sequencing
--
-- https://stackoverflow.com/questions/3270255/is-haskells-mapm-not-lazy
-- In general, for any monadic expression, if the value is used at all, the entire chain of >>=s leading to the expression must be forced, so applying sequence to an infinite list can't ever finish.
-- monadic effects must happen in deterministic order
-- https://github.com/ndmitchell/spaceleak
--
-- mapm: Just 100000
--      56,702,336 bytes allocated in the heap
testMapM :: Int -> Maybe Int
testMapM n =
  fmap (length . filter (== 1)) .
  mapM validate10 .
  take n $
  cycle [(1::Int)..10]

-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)
-- instance Traversable Maybe where
--     traverse _ Nothing = pure Nothing
--     traverse f (Just x) = Just <$> f x
-- traverse validate10 = foldr (\x ys -> liftA2 (:) (validate10 x) ys) (pure [])

-- foldM doesn't seem to help.
--
-- foldvalidate: Just 100000
--      24,057,176 bytes allocated in the heap
testFoldValidate :: Int -> Maybe Int
testFoldValidate n =
  fmap (length . filter (== 1)) .
  foldValidate .
  take n $
  cycle [(1::Int)..10]

foldValidate :: [Int] -> Maybe [Int]
foldValidate xs = foldM (\b !a -> case validate10 a of
                            Nothing -> Nothing
                            Just x -> Just (x:b)) [] xs

-- https://stackoverflow.com/questions/3270255/is-haskells-mapm-not-lazy
-- In general, for any monadic expression, if the value is used at all, the entire chain of >>=s leading to the expression must be forced, so applying sequence to an infinite list can't ever finish.
-- monadic effects must happen in deterministic order
-- https://github.com/ndmitchell/spaceleak

-- | run using
--
-- > cabal install -O2
-- > purple lengthtail +RTS -s
main :: IO ()
main = do
  args <- getArgs
  let (t,n) = case args of
        [] -> ("validate", 1000000)
        [x] -> (x,1000000)
        (x:y:_) -> (x, read y)
  let res =
        case t of
          "inflist" -> show $ testInfList n
          "inflistpoly" -> show $ testInfListPoly n
          "flist" -> show $ testFList n
          "sum" -> show $ testSum n
          "lengthtail" -> show $ testLengthTail n
          "lengthtaillazy" -> show $ testLengthTailLazy n
          "lengthr" -> show $ testLengthR n
          "lengthr2" -> show $ testLengthR2 n
          "filter" -> show $ testFilter n
          "validate" -> show $ testValidate n
          "maybecount" -> show $ testMaybeCount n
          "recursetail" -> show $ testRecurseTail n
          "recursetaillocal" -> show $ testRecurseTailLocal n
          "recursetaillazy" -> show $ testRecurseTailLazy n
          "recursenolist" -> show $ testRecurseNoList n
          "recursetailnolist" -> show $ testRecurseTailNoList n
          "recursetailnolist2" -> show $ testRecurseTailNoList2 n
          "recursetailnolist3" -> show $ testRecurseTailNoList3 n
          "x1" -> show $ testX1 n
          "xtest" -> show $ testXTest n
          "xtest2" -> show $ testXTest2 n
          "xtest3" -> show $ testXTest3 n
          "xtest4" -> show $ testXTest4 n
          "cycle" -> show $ testCycle n
          "cycle2" -> show $ testCycle2 n
          "cyclefail" -> show $ testCycleFail n
          "mapm" -> show $ testMapM n
          "foldvalidate" -> show $ testFoldValidate n
          _ -> show $ testCycle n
  putStrLn $ t <> ": " <> res

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | BigO numbers
--
-- https://en.wikibooks.org/wiki/Optimizing_Code_for_Speed/Order_of_Complexity_Optimizations#:~:text=of%2DComplexity%20Reduction-,What%20is%20order%20of%20complexity%3F,*log(N))%20etc.
--
-- https://donsbot.wordpress.com/2008/06/04/haskell-as-fast-as-c-working-at-a-high-altitude-for-low-level-performance/
--
-- https://www.fpcomplete.com/haskell/tutorial/profiling/
-- https://www.reddit.com/r/haskell/comments/nl0rkl/looking_for_good_rules_of_thumbs_on_what_haskell/
module Perf.BigO
  ( O (..),
    olist,
    promote,
    promote_,
    demote,
    Order (..),
    order,
    runtime,
    bigO,
    stepO,
    stepOs,
    stepOsB,
    bigOTest,
    bigOT,
  )
where

import qualified Data.List as GHC.List
import qualified Data.List as List
import qualified Data.Vector as V
import Perf.Types
import Perf.Time
import Prelude
import GHC.Generics
import Data.Bool
import Data.Maybe

-- $setup
-- >>> import qualified Data.List as List

-- data PerfTest = PerformanceExample Expression BigOExpression
-- data Expression
-- data BigOExpression

data O
  = N3
  | N2
  | N32
  | NLogN
  | N1
  | N12
  | LogN
  | N0
  deriving (Eq, Ord, Show, Generic, Enum)

-- | enumeration of O types
olist :: [O]
olist = [N3 .. N0]

-- | functions to compute performance measure
--
-- >>> fmap ($ 0) promote_
-- [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
--
-- >>> fmap ($ 1) promote_
-- [1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]
--
-- Ordering makes sense around N=10
--
-- >>> fmap ($ 10) promote_
-- [1000.0,100.0,31.622776601683793,23.02585092994046,10.0,3.1622776601683795,2.302585092994046,1.0]
--
-- Having NP may cause big num problems
--
-- >>> fmap ($ 1000) promote_
-- [1.0e9,1000000.0,31622.776601683792,6907.755278982137,1000.0,31.622776601683793,6.907755278982137,1.0]
promote_ :: [Double -> Double]
promote_ =
  [ -- \n -> min maxBound (bool (2**n) zero (n<=zero)),
    (^ 3),
    (^ 2),
    (** 1.5),
    \n -> bool (bool (n * log n) 1 (n <= 1)) 0 (n <= 0),
    id,
    (** 0.5),
    \n -> bool (bool (log n) 1 (n <= 1)) 0 (n <= 0),
    \n -> bool 1 0 (n <= 0)
  ]

-- | a set of factors for each O, which represents a full Order specification.
newtype Order a = Order {factors :: [a]} deriving (Eq, Ord, Show, Generic, Functor)

-- | create an Order
--
-- >>> order N1 10
-- Order {factors = [0,0,0,0,10,0,0,0]}
order :: (Num a) => O -> a -> Order a
order o a = Order $ replicate n 0 <> [a] <> replicate (7 - n) 0
  where
    n = fromEnum o

-- | Calculate the performance measure
--
-- FIXME:
-- >>> promote (order NLogN 1000) 1
-- 1000.0
promote :: Order Double -> Double -> Double
promote (Order fs) n = sum (zipWith (*) fs (($ n) <$> promote_))

-- | Calculate an Order from a measure, and an N
--
-- >>> demote N2 1000 1000000
-- Order {factors = [0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0]}
--
-- > promote (demote N2 n m) n m == m
demote :: O -> Double -> Double -> Order Double
demote o n m = order o (m / (promote_ List.!! fromEnum o) n)

-- | report the biggest O
bigO :: (Ord a, Num a) => Order a -> (O, a)
bigO (Order os) = (toEnum b, os List.!! b)
  where
    b = fromMaybe 7 $ List.findIndex (> 0) os

-- | calculate the runtime, which is defined as the gap between bigO and Orderfor 1 run.
runtime :: Order Double -> Double
runtime (Order os) = promote (Order r) 1
  where
    b = fromMaybe 7 $ List.findIndex (> 0) os
    r = take b os <> [0] <> drop (b + 1) os

instance (Num a) => Num (Order a) where
  -- 0 = Order $ replicate 9 0
  (+) (Order o) (Order o') =
    Order (zipWith (+) o o')
  negate (Order o) = Order $ negate <$> o
  (*) (Order o) (Order o') =
    Order (zipWith (*) o o')
  abs = undefined
  signum = undefined
  fromInteger x = Order $ replicate 9 (fromInteger x)

-- n' = [1,2,3,4,5,10,20,100,1000,10000]
-- cs' <- warmup 1000 >> (sequence $ (\n -> fst <$> tick (\x -> List.nub [0 .. (x - 1)]) n) <$> [1,2,3,4,5,10,20,100,1000,10000])
-- sum $ fmap abs $ zipWith (\n c -> c - demote N2 (n' List.!! 8) (P.fromIntegral (cs' List.!! 8))) n' (P.fromIntegral <$> cs')
-- >>> estimateO (\x -> List.nub [0 .. (x - 1)]) [1,10,100,1000]
--
{-
estimateO :: (Int -> a) -> [Int] -> Order Double
estimateO f ns = do
  warmup 100
  cs <- sequence $ (\n -> fst <$> tick f n) <$> ns
  undefined

-}

stepO :: [Double] -> [Double] -> (Order Double, [Double])
stepO [] _ = (0, [])
stepO cs' ns' =
  bool
    (lasto, diff)
    (order N0 (maximum cs'), [])
    (List.last cs' < 0)
  where
    diff = diffs List.!! fromEnum o
    diffs =
      ( \o ->
          zipWith
            ( \n c ->
                c
                  - promote
                    ( demote o (List.last ns') (List.last cs')
                    )
                    n
            )
            ns'
            cs'
      )
        <$> [N3 .. N0]
    o =
      toEnum $
        V.minIndex $
          V.fromList
            (sum <$> fmap (fmap abs) diffs)
    lasto = demote o (List.last ns') (List.last cs')

stepOs_ :: Int -> [Double] -> [Double] -> (Order Double, [Double])
stepOs_ n cs ns = go n 0 cs ns
  where
    go _ o [] _ = (o, cs)
    go n o cs ns =
      bool
        ( bool
            ( let (o', res) = stepO cs ns
               in case res of
                    [] -> (o + o', [])
                    r -> go (n - 1) (o' + o) (List.init r) (List.init ns)
            )
            (o, cs)
            (n == 0)
        )
        (o + bool (order N0 (List.head cs)) 0 (length cs == 0), [])
        (length cs <= 1)

stepOs :: [Double] -> [Double] -> Order Double
stepOs cs ns = fst $ stepOs_ (length ns) cs ns

stepOsB :: [Double] -> [Double] -> (O, Double, Double)
stepOsB cs ns = (o, f, r)
  where
    o' = stepOs cs ns
    (o, f) = bigO o'
    r = promote (o' - order o f) (List.last (List.init ns))

-- |
-- > bigOTest 100000
-- (N2,13.377369999999999,65811.87373047954)
bigOTest :: Double -> IO (O, Double, Double)
bigOTest n = do
  _ <- warmup 1000
  cs <- sequence $ (\n -> fst <$> tick (\x -> List.nub [0 .. (x - 1)]) n) <$> ns
  pure (stepOsB (fromIntegral <$> cs) ns)
  where
    ns = reverse $ List.unfoldr (\n -> let n' = (fromIntegral (floor (n / 10) :: Integer) :: Double) in bool (Just (n', n')) Nothing (n' == 0)) n

-- |
-- > bigOT (\x -> List.nub [0 .. (x - 1)]) 10000
-- (N2,13.503969999999999,28089.766030000013)
bigOT :: (Double -> a) -> Double -> IO (O, Double, Double)
bigOT f n = do
  _ <- warmup 1000
  cs <- sequence $ (\n -> fst <$> tick f n) <$> ns
  pure (stepOsB (fromIntegral <$> cs) ns)
  where
    ns = reverse $ List.unfoldr (\n -> let n' = (fromIntegral (floor (n / 10) :: Integer)) in bool (Just (n', n')) Nothing (n' == 0)) n

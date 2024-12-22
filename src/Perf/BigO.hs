{-# LANGUAGE OverloadedStrings #-}

-- | [Order of complexity](https://en.wikibooks.org/wiki/Optimizing_Code_for_Speed/Order_of_Complexity_Optimizations#:~:text=of%2DComplexity%20Reduction-,What%20is%20order%20of%20complexity%3F,*log(N)) calculations.
module Perf.BigO
  ( O (..),
    olist,
    promote,
    promote1,
    promote_,
    demote,
    demote1,
    spectrum,
    Order (..),
    bigO,
    runtime,
    BigOrder (..),
    fromOrder,
    toOrder,
    order,
    diffs,
    bestO,
    estO,
    estOs,
    makeNs,
    OrderOptions (..),
    defaultOrderOptions,
    parseOrderOptions,
  )
where

import Data.Bool
import Data.FormatN
import Data.List qualified as List
import Data.Maybe
import Data.Monoid
import Data.Vector qualified as V
import GHC.Generics
import Options.Applicative
import Prettyprinter
import Prelude

-- $setup
-- >>> import qualified Data.List as List
-- >>> o = Order [0.0,1.0,100.0,0.0,0.0,0.0,0.0,0.0]
-- >>> ms = [2805.0,3476.0,9989.0,92590.0,1029074.6947660954]
-- >>> ns = [1,10,100,1000,10000]

-- | order type
data O
  = -- | cubic
    N3
  | -- | quadratic
    N2
  | -- | ^3/2
    N32
  | -- | N * log N
    NLogN
  | -- | linear
    N1
  | -- | sqrt N
    N12
  | -- | log N
    LogN
  | -- | constant
    N0
  deriving (Eq, Ord, Show, Generic, Enum)

-- | enumeration of O types
--
-- >>> olist
-- [N3,N2,N32,NLogN,N1,N12,LogN,N0]
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
    (^ (3 :: Integer)),
    (^ (2 :: Integer)),
    (** 1.5),
    \n -> bool (bool (n * log n) 1 (n <= 1)) 0 (n <= 0),
    id,
    (** 0.5),
    \n -> bool (bool (log n) 1 (n <= 1)) 0 (n <= 0),
    \n -> bool 1 0 (n <= 0)
  ]

-- | a set of factors for each order, which represents a full Order specification.
newtype Order a = Order {factors :: [a]} deriving (Eq, Ord, Show, Generic, Functor)

-- | create an Order
--
-- >>> order N2 1
-- Order {factors = [0,1,0,0,0,0,0,0]}
order :: (Num a) => O -> a -> Order a
order o a = Order $ replicate n 0 <> [a] <> replicate (7 - n) 0
  where
    n = fromEnum o

-- | Calculate the expected performance measure
--
-- >>> promote (order N2 1) 10
-- 100.0
promote :: Order Double -> Double -> Double
promote (Order fs) n = sum (zipWith (*) fs (($ n) <$> promote_))

-- | Calculate the expected performance measure per n
--
-- >>> promote (order N2 1) 10
-- 100.0
promote1 :: Order Double -> Double
promote1 o = promote o 1

-- | Calculate an Order from a given O, an n, and a total performance measurement
--
-- A measurement of 1e6 for n=1000 with an order of N2 is:
--
-- >>> demote N2 1000 1000000
-- Order {factors = [0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0]}
--
-- > promote (demote N2 n m) n m == m
demote :: O -> Double -> Double -> Order Double
demote o n m = order o (m / (promote_ List.!! fromEnum o) n)

-- | Calculate an Order from a measure, and an O
--
-- >>> demote1 N2 1000
-- Order {factors = [0.0,1000.0,0.0,0.0,0.0,0.0,0.0,0.0]}
--
-- > demote1 N2 m == demote o 1 m
demote1 :: O -> Double -> Order Double
demote1 o m = demote o 1 m

-- | find the dominant order, and it's factor
--
-- >>> bigO o
-- (N2,1.0)
bigO :: (Ord a, Num a) => Order a -> (O, a)
bigO (Order os) = (toEnum b, os List.!! b)
  where
    b = fromMaybe 7 $ List.findIndex (> 0) os

-- | compute the runtime component of an Order, defined as the
-- difference between the dominant order and the total for a single run.
--
-- >>> runtime o
-- 100.0
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

-- | A set of factors consisting of the dominant order, the dominant order factor and a constant factor
data BigOrder a = BigOrder {bigOrder :: O, bigFactor :: a} deriving (Eq, Ord, Show, Generic, Functor)

instance Pretty (BigOrder Double) where
  pretty (BigOrder o f) = pretty (decimal (Just 2) f) <> " * O(" <> viaShow o <> ")"

-- | compute the BigOrder
--
-- >>> fromOrder o
-- BigOrder {bigOrder = N2, bigFactor = 1.0}
fromOrder :: Order Double -> BigOrder Double
fromOrder o' = BigOrder o f
  where
    (o, f) = bigO o'

-- | convert a BigOrder to an Order.
--
-- toOrder . fromOrder is not a round trip iso.
--
-- >>> toOrder (fromOrder o)
-- Order {factors = [0.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0]}
toOrder :: BigOrder Double -> Order Double
toOrder (BigOrder o f) = order o f

-- | The factor for each O given an n, and a measurement.
--
-- >>> spectrum 100 10000
-- Order {factors = [1.0e-2,1.0,10.0,21.71472409516259,100.0,1000.0,2171.4724095162587,10000.0]}
spectrum :: Double -> Double -> Order Double
spectrum n m = Order ((m /) . ($ n) <$> promote_)

-- | The errors for a list of n's and measurements, based on the spectrum of the last measurement.
diffs :: [Double] -> [Double] -> [[Double]]
diffs ns ms = List.transpose $ zipWith (\n m -> zipWith (\o' f -> m - promote (order o' f) n) olist fs) ns ms
  where
    fs = factors (spectrum (List.last ns) (List.last ms))

-- | minimum error order for a list of measurements
--
-- >>> bestO ns ms
-- N1
bestO :: [Double] -> [Double] -> O
bestO ns ms =
  toEnum $
    V.minIndex $
      V.fromList
        (sum <$> fmap (fmap abs) (diffs ns ms))

-- | fit the best order for the last measurement and return it, and the error terms for the measurements
--
-- >>> estO ns ms
-- (Order {factors = [0.0,0.0,0.0,0.0,102.90746947660953,0.0,0.0,0.0]},[2702.0925305233905,2446.9253052339045,-301.7469476609531,-10317.469476609534,0.0])
estO :: [Double] -> [Double] -> (Order Double, [Double])
estO [] _ = (0, [])
estO ns ms = (lasto, diff)
  where
    diff = diffs ns ms List.!! fromEnum o
    o = bestO ns ms
    lasto = demote o (List.last ns) (List.last ms)

-- | fit orders from the last measurement to the first, using the residuals at each step.
--
-- >>> estOs ns ms
-- [Order {factors = [0.0,0.0,0.0,0.0,102.90746947660953,0.0,0.0,0.0]},Order {factors = [0.0,0.0,-0.32626703235351473,0.0,0.0,0.0,0.0,0.0]},Order {factors = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,24.520084692561625]},Order {factors = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,2432.722690017952]},Order {factors = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,245.1760228452299]}]
estOs :: [Double] -> [Double] -> [Order Double]
estOs ns ms = go [] ns ms
  where
    go os _ [] = os
    go os _ [m] = os <> [order N0 m]
    go os ns' ms' = let (o', res) = estO ns' ms' in go (os <> [o']) (List.init ns') (List.init res)

makeNs :: Int -> Double -> Int -> [Int]
makeNs n0 d low = reverse $ go (next n0) [n0]
  where
    next n = floor (fromIntegral n / d)
    go :: Int -> [Int] -> [Int]
    go n acc = bool (go (next n) (acc <> [n])) acc (low >= n)

data OrderOptions = OrderOptions
  { doOrder :: Bool,
    orderLow :: Int,
    orderDivisor :: Double
  }
  deriving (Eq, Show, Generic)

defaultOrderOptions :: OrderOptions
defaultOrderOptions = OrderOptions False 10 9

parseOrderOptions :: OrderOptions -> Parser OrderOptions
parseOrderOptions def =
  OrderOptions
    <$> switch (long "order" <> short 'o' <> help "calculate order")
    <*> option auto (value (orderLow def) <> long "orderlowest" <> showDefaultWith show <> metavar "DOUBLE" <> help "smallest order test")
    <*> option auto (value (orderDivisor def) <> long "orderdivisor" <> showDefaultWith show <> metavar "DOUBLE" <> help "divisor for order computation")

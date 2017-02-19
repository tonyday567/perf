{-# LANGUAGE ExistentialQuantification #-}

module Perf.Quantiles where

import Protolude hiding (zipWith, drop, take, empty, toList, map)
import qualified Control.Foldl as L
import Data.Vector as V

data Quantiles f a = (Ord a) => Quantiles
  { qSize    :: Int
    -- ^ number of quantiles (_qSize-1 = number of buckets)
  , qMarkers :: f a
    -- ^ quantile points
  , qCounts  :: f a
    -- ^ cumulative frequency (> except for first quantile)
  }

-- online quantiles
quantiles :: (Ord a, Fractional a) => Int -> (a -> a) -> a -> L.Fold a [a]
quantiles n f decay = L.Fold step begin extract
  where
    begin = Quantiles n empty empty
    extract (Quantiles _ ms _) = toList ms
    step x a =
        Quantiles
        s'
        (map (\x' -> decay*x'+(1-decay)*f a) m')
        (map (\x' -> decay*x') c')
      where
        (Quantiles s' m' c') = stepP2 x (f a)

quantiles'' :: Int -> (a -> Double) -> Double -> L.Fold a (Quantiles Vector Double)
quantiles'' n f decay = L.Fold step begin identity
  where
    begin = Quantiles n empty empty
    step x a =
        Quantiles
        s'
        (map (\x' -> decay*x'+(1-decay)*f a) m')
        (map (\x' -> decay*x') c')
      where
        (Quantiles s' m' c') = stepP2 x (f a)


quantiles' :: Int -> L.Fold Double [Double]
quantiles' n = L.Fold stepP2 (Quantiles n empty empty) (\(Quantiles _ m _) -> toList m)

-- the quantile of the final value of the foldable
digitize :: (Ord a, Fractional a) => Int -> (a -> a) -> a -> L.Fold a Int
digitize n f decay = L.Fold step begin extract
  where
    begin = (Quantiles n empty empty, -1/0)
    extract (qs, a) = bucket qs a
    step (x, _) a =
        (Quantiles
        s'
        (map (\x' -> decay*x'+(1-decay)*f a) m')
        (map (\x' -> decay*x') c')
        , f a)
      where
        (Quantiles s' m' c') = stepP2 x (f a)

max :: Quantiles Vector a -> a
max (Quantiles s m _) = m ! (s - 1)

min :: Quantiles Vector a -> a
min (Quantiles _ m _) = m ! 0

nth :: Int -> Quantiles Vector a -> a
nth n (Quantiles _ m _) = m ! n

stepP2 :: (Fractional a) => Quantiles Vector a -> a -> Quantiles Vector a
stepP2 q@(Quantiles b ms _) a
  | V.length ms < b =
        Quantiles b
        (fromList (sort $ toList $ cons a ms))
        (generate (fromIntegral $ V.length ms + 1) (\x -> fromIntegral x + 1))
  | otherwise = Quantiles b markersFinal countsFinal
  where
    countsFinal = take 1 counts' V.++ cs24 V.++ drop (b - 1) counts'
    (Quantiles _ markers' counts') = addOne q a
    markersFinal = take 1 markers' V.++ ms24'' V.++ drop (b - 1) markers'
    cs24 = zipWith3 (\x y z-> if x then y+z else y) isOut (init $ tail counts') (init $ tail dsign)
    csIdeal = generate b (\i -> 1 + (last counts' - 1) * fromIntegral i/(fromIntegral b - 1))
    d = zipWith (-) csIdeal counts'
    dsign = map (\x -> if x>0 then 1 else 0) d
    c' = zipWith (-) (tail counts') (init counts')
    isOut = zipWith3 (\d' nd1 nd0 -> d' >= 1 && nd1 > 1  || d' <= (-1) && nd0 > 1) (init $ tail d) (tail c') c'
    p2' = p2 markers' counts' dsign
    l2' = l2 markers' counts' dsign
    ms24 = take (b - 2) $ tail markers'
    okP2 = zipWith3 (\x x0 x1 -> x > x0 && x < x1) p2' markers' (drop 2 markers')
    ms24' = zipWith3 (\x y z-> if x then y else z) okP2 p2' l2'
    ms24'' = zipWith3 (\x y z-> if x then y else z) isOut ms24' ms24

addOne :: (Num a) => Quantiles Vector a -> a -> Quantiles Vector a
addOne (Quantiles b ms qs) a
    | a < ms!0 =
      Quantiles b (cons a (tail ms))
      (cons 1 (map (+1) $ tail qs))
    | a > last ms =
      Quantiles b
      (snoc (init ms) a)
      (qs // [(b - 1, 1 + qs!(b - 1))])
    | otherwise =
      Quantiles b
      ms
      (imap (\i q -> if ms!i > a then q+1 else q) qs)

-- bucket of the value
bucket :: Quantiles Vector a -> a -> Int
bucket (Quantiles _ ms _) a
    | V.null ms = 0
    | a < ms!0 = 0
    | a > last ms = fromIntegral $ V.length ms - 1
    | otherwise = L.fold L.sum [if ms!x > a then 0 else 1 | x <- [0..(V.length ms - 1) - 1]]

p2 :: (Fractional a) => Vector a -> Vector a -> Vector a -> Vector a
p2 q n d = res
  where
    d'  = init $ tail d
    ndelta  = zipWith (-) (tail n) n
    qdelta  = zipWith (-) (tail q) q
    n2delta = zipWith (-) (drop 2 n) n
    dq0 = zipWith4 (\d'' nd' q' n' -> (nd' + d'') * q' / n') d (init ndelta) (tail qdelta) (tail ndelta)
    dq1 = zipWith4 (\d'' nd' q' n' -> (nd' - d'') * q' / n') d (tail ndelta) (init qdelta) (init ndelta)
    delta = zipWith3 (\n2' dq0' dq1' -> (dq0' + dq1') / n2') n2delta dq0 dq1
    res = zipWith3 (\q'' d'' delta' -> q'' + d'' * delta') (init $ tail q) d' delta

l2 :: (Ord a, Fractional a) => Vector a -> Vector a -> Vector a -> Vector a
l2 q n d =
  zipWith6
  (\q'' d'' qd' nd' qd'' nd'' ->
    ((+) q''
     (if d''>0 then d'' * qd' / nd' else d'' * qd'' / nd'')))
  q' d' (tail qdelta) (tail ndelta) (init qdelta) (init ndelta)
  where
    q' = init (tail q)
    d' = init (tail d)
    ndelta  = zipWith (-) (tail n) n
    qdelta  = zipWith (-) (tail q) q





{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}

-- | analytical functionality for perf
module Perf.Analysis where

import Data.Scientific
import Data.TDigest
import Perf
import Protolude
import Readme.Lhs hiding (Format)
import qualified Data.Text as Text
import qualified Data.Text.Format as Text
import Data.Text.Lazy.Builder (toLazyText)

-- | compute deciles
--
-- >>> deciles 5 [1..1000]
-- [1.0,200.5,400.5,600.5,800.5,1000.0]
--
-- > c5 <- decile 5 <$> ticks n f a
--
deciles :: (Functor f, Foldable f, Integral a) => a -> f a -> [Double]
deciles n xs =
  (\x -> fromMaybe 0 $
    quantile x (tdigest (fromIntegral <$> xs) :: TDigest 25)) <$>
  ((/ fromIntegral n) . fromIntegral <$> [0 .. n])

-- | compute a percentile
--
-- >>> percentile 0.1 [1..1000]
-- 100.5
--
-- > c <- percentile 0.4 . fst <$> ticks n f a
--
percentile :: (Functor f, Foldable f, Integral a) => Double -> f a -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest (fromIntegral <$> xs) :: TDigest 25)

-- | fixed precision
prec :: (Real a) => Int -> a -> Text
prec n x = toStrict $ toLazyText $ Text.prec n x

-- | fixed
fixed :: (Real a) => Int -> a -> Text
fixed n x = toStrict $ toLazyText $ Text.fixed n x

-- | fixed precision for a Scientific
sciprec :: Int -> Scientific -> Text
sciprec n x = Text.pack $ formatScientific Exponent (Just n) x

commas :: (RealFrac a) => Int -> a -> Text
commas n a
  | a < 1000 = fixed n a
  | otherwise = go (floor a) ""
  where
    go :: Int -> Text -> Text
    go x t
      | x < 0 = "-" <> go (- x) ""
      | x < 1000 = Text.pack (show x) <> t
      | otherwise =
        let (d, m) = divMod x 1000
         in go d ("," <> Text.pack (show m))

-- | convert an integral to a Scientific
--
-- >>> sformat (left 8 ' ' %. prec 1) (int2Sci 1000)
-- "   1.0e3"
--
int2Sci :: (Integral a) => a -> Scientific
int2Sci n = scientific (fromIntegral n) 0

formatSecs :: (RealFloat a) => Int -> a -> Text
formatSecs p s
    | s < 0      = "-" <> formatSecs p (-s)
    | s >= 1     = formatFixed p s <> " s"
    | s >= 1e-3  = formatFixed p (s*1e3) <> " ms"
    | s >= 1e-6  = formatFixed p (s*1e6)  <> " μs"
    | s >= 1e-9  = formatFixed p (s*1e9) <> " ns"
    | otherwise  = formatFixed p (s*1e12) <> " ps"

-- | format an Integral as a Scientific with a precision
formatI :: (Integral a) => Int -> a -> Text
formatI p x = sciprec p $ int2Sci x

-- | format a Float as a Scientific with a precision
formatF :: (RealFloat a) => Int -> a -> Text
formatF p x = sciprec p (fromFloatDigits x)

-- | format a Float as a Scientific with a precision
formatFixed :: (RealFloat a) => Int -> a -> Text
formatFixed p x = toStrict $ toLazyText $ Text.fixed p (fromFloatDigits x)

-- | format the first few results, the median and average
formatRun :: (Integral a) => Text -> Int -> Int -> [a] -> [Text]
formatRun label n p xs =
  [ label] <>
  (formatI p <$> take n xs) <>
  [ formatF p $ average xs
  , formatF p $ percentile 0.5 xs
  ]

-- | average and time
formatRunTime :: (Integral a) => Text -> Double -> Int -> [a] -> [Text]
formatRunTime label npc p xs =
  [ label] <>
  [ formatF p $ npc * average xs
  , formatF p $ average xs
  ]

formatRuns :: (Integral a) => Int -> Int -> [(Text, [a])] -> Block
formatRuns n p runs =
  table
  mempty
  (["run"] <>
   take n (["first", "second", "third"] <> ((<>"th") . show <$> [4::Int ..])) <>
    ["average", "median"])
  ([AlignLeft] <> replicate (n+2) AlignRight)
  (replicate (n+3) 0)
  (fmap (\(l,as) -> formatRun l n p as) runs)

formatRunsTime :: (Integral a) => Double -> Int -> [(Text, [a])] -> Block
formatRunsTime npc p runs =
  table
  mempty
  ["run", "cputime", "cycles"]
  [AlignLeft, AlignRight, AlignRight]
  []
  (fmap (\(l,as) -> formatRunTime l npc p as) runs)


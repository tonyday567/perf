{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | analytical functionality for perf
module Perf.Analysis where

import Data.Scientific
import Data.TDigest
import Formatting
import Perf
import Protolude hiding ((%))
import qualified Data.Text as Text

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

-- | fixed precision for a Scientific
prec :: Int -> Format r (Scientific -> r)
prec n = scifmt Exponent (Just n)

-- | convert an integral to a Scientific
--
-- >>> sformat (left 8 ' ' %. prec 1) (int2Sci 1000)
-- "   1.0e3"
--
int2Sci :: (Integral a) => a -> Scientific
int2Sci n = scientific (fromIntegral n) 0

-- | format an Integral as a Scientific with a label and precision
formatInt :: (Integral a) => Text -> Int -> a -> Text
formatInt label p x =
        sformat
          ((right 24 ' ' %. stext) %
           (left 8 ' ' %. prec p)) label (int2Sci x)

-- | format an Integral as a Scientific with a label and precision
formatDouble :: (Integral a) => Text -> Int -> a -> Text
formatDouble label p x =
        sformat
          ((right 24 ' ' %. stext) %
           (left 8 ' ' %. prec p)) label (int2Sci x)

-- | format the first 5 results, and then the 40th percentile
formatRun :: (Integral a) => Text -> Int -> [a] -> Text
formatRun label p xs =
  sformat
  ((right 24 ' ' %. stext) % stext %
    (left 9 ' ' %. prec p) %
    (left 9 ' ' %. prec p))
  label
  (Text.intercalate " " $ sformat (left 8 ' ' %. prec p) .
    int2Sci <$>
    take 3 xs)
  (fromFloatDigits $ percentile 0.5 xs)
  (fromFloatDigits $ average xs)

-- | header for formatRun
formatRunHeader :: Text
formatRunHeader =
  sformat
  ((right 24 ' ' %. stext) %
    (left 8 ' ' %. stext) %
    (left 9 ' ' %. stext) %
    (left 9 ' ' %. stext) %
    (left 9 ' ' %. stext) %
    (left 9 ' ' %. stext))
  "run"
  "first"
  "2nd"
  "3rd"
  "median"
  "av."

-- | format a tick result with an inside and outside measurement
formatGap :: (Integral a) => Int -> (a, ([a],b)) -> Text
formatGap a (co, (ci, _)) =
  sformat
  ("n = " %(left 7 ' ' %. prec 3)%" outside: "%(left 7 ' ' %. prec 3)%" inside: "%(left 7 ' ' %. prec 3)%" gap: "%(left 7 ' ' %. prec 3))
  (int2Sci a) (int2Sci co) (int2Sci $ sum ci) (int2Sci $ co - sum ci)

-- | place markdown backtics around text
code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

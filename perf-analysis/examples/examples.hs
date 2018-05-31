{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Data.Scientific
import Formatting
import Options.Generic
import Perf
import Perf.Analysis
import Protolude hiding ((%))
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  , sumsTo :: Maybe [Int] -- <?> "sum to these numbers"
  } deriving (Generic, Show)

instance ParseField [Int]

instance ParseRecord Opts

-- | useful as a benchmark versus the `ticks` functions being imported
ticksInFile :: (NFData b) => Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInFile n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t,_) <- tick f a
          go f a (n - 1) (t:ts)

-- | interposition of a lambda
fLambda :: Int -> Int
fLambda = \x -> foldl' (+) 0 [1 .. x]

-- | monomorphic version
fMono :: Int -> Int
fMono x = foldl' (+) 0 [1 .. x]

-- | polymorphic function version
fPoly :: (Integral b) => b -> b
fPoly x = foldl' (+) 0 [1 .. x]

main :: IO ()
main = do
  o :: Opts <- getRecord "perf examples"
  let !n = fromMaybe 1000 (runs o)
  let !a = fromMaybe 1000 (sumTo o)
  let !as = [1, 10, 100, 1000 :: Int]

  writeFile "other/run.md" $
    code
      [ formatInt "number of runs:" 2 n
      , formatInt "accumulate to:" 2 a
      , sformat ((right 24 ' ' %. stext)%stext) "function:" "foldl' (+) 0"
      ]

  -- | tick_ testing
  prewarmup <- tick_
  _ <- warmup 100
  onetick <- tick_
  ticks' <- replicateM 10 tick_
  manyticks <- replicateM 1000000 tick_
  let avticks = average manyticks
  let qticks = deciles 5 manyticks
  let tick999 = percentile 0.999 manyticks
  let tick99999 = percentile 0.99999 manyticks
  let tick99 = percentile 0.99 manyticks
  let tick40 = percentile 0.4 manyticks
  writeFile "other/tick_.md" $
    code
      [ "pre warmup: " <> show prewarmup <> " cycles"
      , "one tick_: " <> show onetick <> " cycles"
      , "next 10: " <> show ticks'
      , "average over 1m: " <> sformat (fixed 2) avticks <> " cycles"
      , "99.999% perc: " <> sformat commas (floor tick99999 :: Integer)
      , "99.9% perc: " <> sformat (fixed 2) tick999
      , "99th perc:  " <> sformat (fixed 2) tick99
      , "40th perc:  " <> sformat (fixed 2) tick40
      , "[min, 20th, .. 80th, max]:"
      , mconcat (sformat (" " % prec 4) . fromFloatDigits <$> qticks)
      ]

  -- | tick tests
  (t, resultPrime) <- tick fMono a
  print resultPrime
  (t2,_) <- tick fMono a
  (t3,_) <- tick fMono a
  (t',_) <- tick' fMono a
  (tio,_) <- tickIO (pure $ fMono a)
  -- mystery: if tickNoinline is used at all, all tick computations slow down
  -- by a factor of 2 (or by a function call)
  -- (tn,_) <- tickNoinline fMono a
  t10 <- sequence $ replicate 10 (tick fMono a)
  t10io <- sequence $ replicate 10 (tickIO $ pure (fMono a))
  -- t10n <- sequence $ replicate 10 (tickNoinline fMono a)
  t10' <- sequence $ replicate 10 (tick fMono a)
  writeFile "other/tick.md" $
    code
      [ "sum to " <> show a
      , "first measure: " <> show t <> " cycles"
      , "second measure: " <> show t2 <> " cycles"
      , "third measure: " <> show t3 <> " cycles"
      , "tick': " <> show t' <> " cycles"
      , "tickIO: " <> show tio <> " cycles"
      -- , "tickNoinline: " <> show tn <> " cycles"
      , "tick * 10: " <> show (fst <$> t10)
      , "tickIO * 10: " <> show (fst <$> t10io)
      -- , "tickNoinline * 10: " <> show (fst <$> t10n)
      , "tick * 10: " <> show (fst <$> t10')
      ]

  -- | ticks and ticksIO
  (rpure, _) <- ticks n fMono a
  (rpurePoly, _) <- ticks n fPoly a
  (rpureLambda, _) <- ticks n fLambda a
  (rio, _) <- ticksIO n (pure $ fMono a)
  (rioPoly, _) <- ticksIO n (pure $ fPoly a)
  (rioLambda, _) <- ticksIO n (pure $ fLambda a)

  writeFile "other/ticks.md" $
    code [ "acc = " <> show a <> " n = " <> show n
         , formatRunHeader
         , formatRun "monomorphic" 2 $ rpure
         , formatRun "includes lambda" 2 $ rpureLambda
         , formatRun "polymorphic" 2 $ rpurePoly
         , formatRun "ticksIO mono" 2 $ rio
         , formatRun "ticksIO lambda" 2 $ rioLambda
         , formatRun "ticksIO poly" 2 $ rioPoly
         ]

  gaps <- sequence $ (\a -> formatGap a <$> tickIO (ticks n fPoly a)) <$> as
  writeFile "other/ticksCost.md" $ code gaps

  -- | ns testing
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick fMono) <$> as)
  let r12 =
        "(replicateM n . tick fMono) <$> as: " <>
        mconcat (sformat (" " %prec 3) <$>
                 (fromFloatDigits . percentile 0.4 <$> css))
  (ts, _) <- ns (ticks n fMono) as
  let r13 =
        "ns (ticks n fMono) as: " <>
        mconcat (sformat (" " %prec 3) <$>
                 (fromFloatDigits . percentile 0.4 <$> ts))
  writeFile "other/tickns.md" $
    code ["sum to's " <> show as, r13, r12]

  -- | vectors
  let asl :: [Int]
      asl = [1 .. a]
  let suml :: [Int] -> Int
      suml = foldl' (+) 0
  (rlist, _) <- ticks n suml asl
  let sumv :: V.Vector Int -> Int
      sumv = V.foldl (+) 0
  let asv :: V.Vector Int =
        V.generate a identity
  (rboxed, _) <- ticks n sumv asv
  let sums :: S.Vector Int -> Int
      sums = S.foldl (+) 0
  let ass :: S.Vector Int =
        S.generate a identity
  (rstorable, _) <- ticks n sums ass
  let sumu :: U.Vector Int -> Int
      sumu = U.foldl (+) 0
  let asu :: U.Vector Int =
        U.generate a identity
  (runboxed, _) <- ticks n sumu asu
  writeFile "other/vector.md" $
    code
    [ "sum to " <> show a
    , formatRun "ticks list" 2 $ rlist
    , formatRun "ticks boxed" 2 $ rboxed
    , formatRun "ticks storable" 2 $ rstorable
    , formatRun "ticks unboxed" 2 $ runboxed
    ]

  -- WHNF
  (rnf, _) <- tick (fmap fMono) (Just a)
  (rwhnf, _) <- tick (fmap fMono) (Just a)
  (rnfs, _) <- ticks n (fmap fMono) (Just a)
  (rwhnfs, _) <- ticksWHNF n (fmap fMono) (Just a)
  (rnfio, _) <- tickIO (pure $ fmap fMono (Just a))
  (rwhnfio, _) <- tickWHNFIO (pure $ fmap fMono (Just a))
  (rnfsio, _) <- ticksIO n (pure $ fmap fMono (Just a))
  (rwhnfsio, _) <- ticksWHNFIO n (pure $ fmap fMono (Just a))

  writeFile "other/whnf.md" $ code
    [ "sum to " <> show a
    , formatInt "tick" 2 $ rnf
    , formatInt "tickWHNF" 2 $ rwhnf
    , formatRun "ticks" 2 rnfs
    , formatRun "ticksWHNF" 2 rwhnfs
    , formatInt "tickIO" 2 rnfio
    , formatInt "tickWHNFIO" 2 rwhnfio
    , formatRun "ticksIO" 2 rnfsio
    , formatRun "ticksWHNFIO" 2 rwhnfsio
    ]

  -- perf basics
  (result, cs) <- runPerfT $
      perf "sum" cycles (pure $ foldl' (+) 0 [0 .. a])
  putStrLn (show (result, cs) :: Text)

  -- PerfT example
  -- prior to Perfification
  result <- do
      txt <- readFile "perf-analysis/examples/examples.hs"
      let n = Text.length txt
      let x = foldl' (+) 0 [1..n]
      putStrLn $ "sum of one to number of characters is: " <>
          (show x :: Text)
      pure (n, x)

  -- post-Perfification
  (result', ms) <- runPerfT $ do
          txt <- perf "file read" cycles $ readFile "perf-analysis/examples/examples.hs"
          n <- perf "length" cycles $ pure (Text.length txt)
          x <- perf "sum" cycles $ pure (foldl' (+) 0 [1..n])
          perf "print to screen" cycles $
              putStrLn $ "sum of one to number of characters is: " <>
              (show x :: Text)
          pure (n, x)

  when (result == result') $ print "PerfT has no effect on original computation"

  let fmt = sformat ((right 40 ' ' %. stext) %prec 3 % " " % stext)
  writeFile "other/perf.md" $
    "\nperf cycle measurements\n---\n" <>
    code ((\(t,c) -> fmt t (int2Sci c) "cycles") <$> Map.toList ms)


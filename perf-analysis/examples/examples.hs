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

import Readme.Lhs
import Options.Generic
import Perf
import Perf.Analysis
import Protolude
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

  -- | ticks and ticksIO
  (rpure, _) <- ticks n fMono a
  (rpurePoly, _) <- ticks n fPoly a
  (rpureLambda, _) <- ticks n fLambda a
  (rio, _) <- ticksIO n (pure $ fMono a)
  (rioPoly, _) <- ticksIO n (pure $ fPoly a)
  (rioLambda, _) <- ticksIO n (pure $ fLambda a)

  -- | gaps
  gaps <- sequence $ (tickIO . ticks n fPoly) <$> as

  -- | ns testing
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick fMono) <$> as)
  (ts, _) <- ns (ticks n fMono) as

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

  -- WHNF
  (rnf, _) <- tick (fmap fMono) (Just a)
  (rwhnf, _) <- tick (fmap fMono) (Just a)
  (rnfs, _) <- ticks n (fmap fMono) (Just a)
  (rwhnfs, _) <- ticksWHNF n (fmap fMono) (Just a)
  (rnfio, _) <- tickIO (pure $ fmap fMono (Just a))
  (rwhnfio, _) <- tickWHNFIO (pure $ fmap fMono (Just a))
  (rnfsio, _) <- ticksIO n (pure $ fmap fMono (Just a))
  (rwhnfsio, _) <- ticksWHNFIO n (pure $ fmap fMono (Just a))

  void $ runOutput
    ("other/readme_.md", GitHubMarkdown)
    ("readme.md", GitHubMarkdown) $ do

    output "run" $ Native $ (:[]) $ table
      "run details"
      []
      [AlignLeft, AlignRight]
      [0, 0]
      [ ["number of runs", formatI 2 n]
      , ["accumulate to", formatI 2 a]
      , ["function", "foldl' (+) 0"]
      ]

    output "tick_" $ Native $ (:[]) $ table mempty ["stat", "cycles"] mempty mempty
      [ ["pre warmup", show prewarmup]
      , ["one tick_", show onetick]
      , ["next 10", show ticks']
      , ["average over one million", fixed 2 avticks]
      , ["99.999% perc", commas 0 tick99999]
      , ["99.9% perc", fixed 2 tick999]
      , ["99th perc", fixed 2 tick99]
      , ["40th perc", fixed 2 tick40]
      , ["[min, 20th, .. 80th, max]",
         Text.intercalate " " (formatF 4 <$> qticks)]
      ]

    output "tick" $ Native
      [ plain ("sum to " <> show a)
      , table mempty ["stat", "cycles"] mempty mempty
        [ ["first measure", show t]
        , ["second measure", show t2]
        , ["third measure", show t3]
        , ["tick'", show t']
        , ["tickIO", show tio]
        , ["tick * 10", show (fst <$> t10)]
        , ["tickIO * 10", show (fst <$> t10io)]
        , ["tick' * 10", show (fst <$> t10')]
        ]
      ]

    output "ticks" $ Native [formatRuns 3 2
      [ ("monomorphic", rpure)
      , ("includes lambda", rpureLambda)
      , ("polymorphic", rpurePoly)
      , ("ticksIO mono", rio)
      , ("ticksIO lambda", rioLambda)
      , ("ticksIO poly", rioPoly)
      ]]



    output "gaps" $ Native $ (:[]) $ table mempty
      ["number runs", "outside cycles", "inside cycles", "gap"]
      mempty mempty
      (zipWith (\a (co, (ci, _)) ->
                  [ formatI 1 a
                  , formatI 3 co
                  , formatI 3 (sum ci)
                  , formatI 3 (co - sum ci)
                  ]) as gaps)

    output "tickns" $ Native
      [ table mempty (["sum to:"] <> (show <$> as)) mempty mempty
        [ ["(replicateM n . tick fMono) <$> as"] <>
          (formatF 3 . percentile 0.5 <$> css)
        , ["ns (ticks n fMono) as"] <>
          (formatF 3 . percentile 0.5 <$> ts)
        ]
      ]

    output "vector" $ Native $ [plain ("sum to " <> show a)] <>
      [formatRuns 3 2 
      [ ("ticks list", rlist)
      , ("ticks boxed", rboxed)
      , ("ticks storable", rstorable)
      , ("ticks unboxed", runboxed)
      ]]

    output "whnf" $ Native
      [ plain ("sum to " <> show a)
      , table mempty ["function", "cycles"] mempty mempty
        [ ["tick", formatI 3 rnf]
        , ["tickWHNF", formatI 3 rwhnf]
        , formatRun "ticks" 3 3 rnfs
        , formatRun "ticksWHNF" 3 3 rwhnfs
        , ["tickIO", formatI 3 rnfio]
        , ["tickWHNFIO", formatI 3 rwhnfio]
        , formatRun "ticksIO" 3 3 rnfsio
        , formatRun "ticksWHNFIO" 3 3 rwhnfsio
        ]
      ]

    output "perf" $ Native
      [ plain "perf cycle measurements"
      , table mempty ["effect", "cycles"] mempty mempty
        ((\(t,c) -> [t, formatI 3 c]) <$> Map.toList ms)
      ]

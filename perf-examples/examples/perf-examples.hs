{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import NumHask.Prelude
import Options.Generic
import Perf
import Readme.Format as F
import Readme.Lhs as Lhs

data Opts
  = Opts
      { runs :: Maybe Int, -- <?> "number of runs"
        sumTo :: Maybe Int, -- <?> "sum to this number"
        sumsTo :: Maybe [Int] -- <?> "sum to these numbers"
      }
  deriving (Generic, Show)

instance ParseField [Int]

instance ParseRecord Opts

-- | useful as a benchmark versus the `ticks` functions being imported
ticksInFile :: (NFData b) => Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInFile n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
        (t, _) <- tick f a
        go f a (n - 1) (t : ts)

-- | interposition of a lambda
fLambda :: Int -> Int
fLambda = \x -> foldl' (+) 0 [1 .. x]

-- | monomorphic version
fMono :: Int -> Int
fMono x = foldl' (+) 0 [1 .. x]

-- | polymorphic function version
fPoly :: (Integral b, Enum b, FromInteger b) => b -> b
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
  manyticks <- fmap fromIntegral <$> replicateM 1000000 tick_
  let avticks = average manyticks
  let qticks = deciles 5 manyticks
  let tick999 = percentile 0.999 manyticks
  let tick99999 = percentile 0.99999 manyticks
  let tick99 = percentile 0.99 manyticks
  let tick40 = percentile 0.4 manyticks
  -- perf basics
  (result, cs) <-
    runPerfT $
      perf "sum" cycles (pure $ foldl' (+) 0 [0 .. a])
  putStrLn (show (result, cs) :: Text)
  -- PerfT example
  -- prior to Perfification
  result <- do
    txt <- readFile "perf-examples/examples/perf-examples.hs"
    let n = Text.length txt
    let x = foldl' (+) 0 [1 .. n]
    putStrLn $
      "sum of one to number of characters is: "
        <> (show x :: Text)
    pure (n, x)
  -- post-Perfification
  (result', ms) <- runPerfT $ do
    txt <- perf "file read" cycles $ readFile "perf-examples/examples/perf-examples.hs"
    n <- perf "length" cycles $ pure (Text.length txt)
    x <- perf "sum" cycles $ pure (foldl' (+) 0 [1 .. n])
    perf "print to screen" cycles
      $ putStrLn
      $ "sum of one to number of characters is: "
        <> (show x :: Text)
    pure (n, x)
  when (result == result') $ print "PerfT has no effect on original computation"
  -- tick tests
  (t, resultPrime) <- tick fMono a
  print resultPrime
  (t2, _) <- tick fMono a
  (t3, _) <- tick fMono a
  (t', _) <- tick' fMono a
  (tio, _) <- tickIO (pure $ fMono a)
  -- mystery: if tickNoinline is used at all, all tick computations slow down
  -- by a factor of 2 (or by a function call)
  -- (tn,_) <- tickNoinline fMono a
  t10 <- sequence $ replicate 10 (tick fMono a)
  t10io <- sequence $ replicate 10 (tickIO $ pure (fMono a))
  -- t10n <- sequence $ replicate 10 (tickNoinline fMono a)
  t10' <- sequence $ replicate 10 (tick fMono a)
  -- ticks and ticksIO
  (rpure, _) <- ticks n fMono a
  (rpurePoly, _) <- ticks n fPoly a
  (rpureLambda, _) <- ticks n fLambda a
  (rio, _) <- ticksIO n (pure $ fMono a)
  (rioPoly, _) <- ticksIO n (pure $ fPoly a)
  (rioLambda, _) <- ticksIO n (pure $ fLambda a)
  -- gaps
  gaps <- sequence $ (tickIO . ticks n fPoly) <$> as
  -- ns testing
  css <-
    fmap (fmap fst)
      <$> sequence ((replicateM n . tick fMono) <$> as)
  (ts, _) <- ns (ticks n fMono) as
  -- vectors
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
  let makeStatsTable t hs rs = Lhs.toList $ defaultTextTable bootTableAttr t ((,ColWidthDefault) <$> [AlignLeft, AlignRight]) hs rs
  void
    $ runOutput
      ("perf-examples/examples/perf-examples.md", GitHubMarkdown)
      ("other/perf-examples.md", GitHubMarkdown)
    $ do
      output "run" $ Native $
        makeStatsTable
          "run details"
          []
          [ ["number of runs", prec 2 (fromIntegral n)],
            ["accumulate to", prec 2 (fromIntegral a)],
            ["function", "foldl' (+) 0"]
          ]
      output "tick_" $ Native $
        makeStatsTable
          ""
          (["stat", "cycles"])
          [ ["pre warmup", show prewarmup],
            ["one tick_", show onetick],
            ["next 10", show ticks'],
            ["average over one million", fixed 2 avticks],
            ["99.999% perc", commas 0 tick99999],
            ["99.9% perc", fixed 2 tick999],
            ["99th perc", fixed 2 tick99],
            ["40th perc", fixed 2 tick40],
            [ "[min, 20th, .. 80th, max]",
              Text.intercalate " " (prec 4 <$> qticks)
            ]
          ]
      output "tick" $ Native $
        (Lhs.toList $ plain (str $ "sum to " <> show a))
          <> makeStatsTable
            ""
            ["stat", "cycles"]
            [ ["first measure", show t],
              ["second measure", show t2],
              ["third measure", show t3],
              ["tick'", show t'],
              ["tickIO", show tio],
              ["tick * 10", show (fst <$> t10)],
              ["tickIO * 10", show (fst <$> t10io)],
              ["tick' * 10", show (fst <$> t10')]
            ]
      output "ticks"
        $ Native
        $ Lhs.toList
        $ formatRuns
          3
          2
          [ ("monomorphic", rpure),
            ("includes lambda", rpureLambda),
            ("polymorphic", rpurePoly),
            ("ticksIO mono", rio),
            ("ticksIO lambda", rioLambda),
            ("ticksIO poly", rioPoly)
          ]
      output "gaps" $ Native $
        makeStatsTable
          ""
          ["number runs", "outside cycles", "inside cycles", "gap"]
          ( zipWith
              ( \a (co, (ci, _)) ->
                  [ prec 1 a,
                    prec 3 (fromIntegral co),
                    prec 3 (sum (fromIntegral <$> ci)),
                    prec 3 (fromIntegral co - sum (fromIntegral <$> ci))
                  ]
              )
              (fromIntegral <$> as :: [Double])
              gaps
          )
      output "tickns"
        $ Native
        $ makeStatsTable
          ""
          (["sum to:"] <> (show <$> as))
          [ ["(replicateM n . tick fMono) <$> as"]
              <> (prec 3 . percentile 0.5 <$> (fmap fromIntegral <$> css)),
            ["ns (ticks n fMono) as"]
              <> (prec 3 . percentile 0.5 <$> (fmap fromIntegral <$> ts))
          ]
      output "vector" $ Native $
        (Lhs.toList $ plain (str $ "sum to " <> show a))
          <> ( Lhs.toList $
                 formatRuns
                   3
                   2
                   [ ("ticks list", rlist),
                     ("ticks boxed", rboxed),
                     ("ticks storable", rstorable),
                     ("ticks unboxed", runboxed)
                   ]
             )
      output "whnf" $ Native $
        (Lhs.toList $ plain (str $ "sum to " <> show a))
          <> makeStatsTable
            ""
            ["function", "cycles"]
            [ ["tick", prec 3 (fromIntegral rnf)],
              ["tickWHNF", prec 3 (fromIntegral rwhnf)],
              formatRun "ticks" 3 3 (fromIntegral <$> rnfs),
              formatRun "ticksWHNF" 3 3 rwhnfs,
              ["tickIO", prec 3 (fromIntegral rnfio)],
              ["tickWHNFIO", prec 3 (fromIntegral rwhnfio)],
              formatRun "ticksIO" 3 3 (fromIntegral <$> rnfsio),
              formatRun "ticksWHNFIO" 3 3 (fromIntegral <$> rwhnfsio)
            ]
      output "perf" $ Native $
        (Lhs.toList $ plain "perf cycle measurements")
          <> ( makeStatsTable "" ["effect", "cycles"] $
                 (\(t, c) -> [t, prec 3 (fromIntegral c)]) <$> Map.toList ms
             )

-- | format the first few results, the median and average
formatRun :: Text -> Int -> Int -> [Cycle] -> [Text]
formatRun label n p xs =
  [label]
    <> (prec p . fromIntegral <$> take n xs)
    <> [ prec p $ average (fromIntegral <$> xs),
         prec p $ percentile 0.5 (fromIntegral <$> xs)
       ]

-- | average and time
formatRunTime :: Text -> Double -> Int -> [Cycle] -> [Text]
formatRunTime label npc p xs =
  [label]
    <> [ prec p $ npc * average (fromIntegral <$> xs),
         prec p $ average (fromIntegral <$> xs)
       ]

formatRuns :: Int -> Int -> [(Text, [Cycle])] -> Blocks
formatRuns n p runs =
  defaultTextTable
    bootTableAttr
    mempty
    ( zip
        ([AlignLeft] <> replicate (n + 2) AlignRight)
        (replicate (n + 3) ColWidthDefault)
    )
    ( ["run"]
        <> take n (["first", "second", "third"] <> ((<> "th") . show <$> [4 :: Int ..]))
        <> ["average", "median"]
    )
    (fmap (\(l, as) -> formatRun l n p as) runs)

formatRunsTime :: Double -> Int -> [(Text, [Cycle])] -> Blocks
formatRunsTime npc p runs =
  defaultTextTable
    bootTableAttr
    mempty
    ((,ColWidthDefault) <$> [AlignLeft, AlignRight, AlignRight])
    ["run", "cputime", "cycles"]
    (fmap (\(l, as) -> formatRunTime l npc p as) runs)

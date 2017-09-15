{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Main where

import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Formatting
import Options.Generic
import NumHask.Prelude hiding ((%))
-- import Perf
import Perf.Cycle
import System.CPUTime.Rdtsc

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Double -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

ticksTest :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksTest n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t,_) <- tick f a
          go f a (n - 1) (t:ts)

ticksInFile :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInFile n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t,_) <- tick f a
          go f a (n - 1) (t:ts)

ticksRep :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksRep n f a = do
    ts <- replicateM n (tick f a)
    pure (fst <$> ts, List.last $ snd <$> ts)

-- | lazy version of tick
tickLazy :: (NFData b) => (a -> b) -> a -> IO (Cycle, b)
tickLazy f a = do
  !t <- rdtsc
  a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')

ticksLazy :: (NFData b) => Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksLazy n f a = go f a n []
  where
      go :: (NFData b) => (a->b) -> a -> Int -> [Cycle] -> IO ([Cycle], b)
      go f a n ts
        | n <= 0 = pure (ts, f a)
        | otherwise = do
              (t,_) <- tickLazy f a
              go f a (n - 1) (t:ts)

fLambda :: Int -> Int
fLambda = \x -> foldl' (+) 0 $! [1 .. x]

fMono :: Int -> Int
fMono x = foldl' (+) 0 [1 .. x]

fPoly :: (Enum b, Num b, Additive b) => b -> b
fPoly x = foldl' (+) 0 [1 .. x]

main :: IO ()
main = do
  o :: Opts <- getRecord "a random bit of text"
  let n = fromMaybe 100 (runs o)
  let a = fromMaybe 100000 (sumTo o)
  let !a' = fromIntegral $ floor a :: Int
  let as = [1, 10, 100, 1000, 10000, 100000 :: Int]

  let formatRun :: [Cycle] -> Text -> Text
      formatRun cs label =
        sformat
          ((right 24 ' ' %. stext) % stext %
           (left 7 ' ' %. prec 3) % " cycles")
          label
          (Text.intercalate " " $ sformat (left 7 ' ' %. prec 3) <$>
           take 5 cs)
          (percentile 0.4 cs)

  let formatRunHeader =
        sformat
          ((right 24 ' ' %. stext) %
           (left 7 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext))
          "run"
          "first"
          "2nd"
          "3rd"
          "4th"
          "5th"
          "40th %"

  let run label t = (`formatRun` label) . fst <$> t

  _ <- warmup 100

  -- | tick_
  onetick <- tick_
  ticks' <- replicateM 10 tick_
  manyticks <- replicateM 1000000 tick_
  let avticks = average manyticks
  let qticks = deciles 10 manyticks
  let tick999 = percentile 0.999 manyticks
  let tick99999 = percentile 0.99999 manyticks
  let tick99 = percentile 0.99 manyticks
  let tick40 = percentile 0.4 manyticks
  writeFile "other/tick_.md" $
    code
      [ "one tick_: " <> show onetick <> " cycles"
      , "next 10: " <> show ticks'
      , "average over 1m: " <> sformat (fixed 2) avticks <> " cycles"
      , "99.999% perc: " <> sformat commas (floor tick99999 :: Integer)
      , "99.9% perc: " <> sformat (fixed 2) tick999
      , "99th perc:  " <> sformat (fixed 2) tick99
      , "40th perc:  " <> sformat (fixed 2) tick40
      , "[min, 10th, 20th, .. 90th, max]:"
      , mconcat (sformat (" " % prec 4) <$> qticks)
      ]

  -- tick
  (t, _) <- tick fMono a'
  (t2,_) <- tick fMono a'
  (ts, _) <- ticks n fMono a'
  let qt x = (`percentile` x) <$> [0, 0.3, 0.5, 0.9, 0.99, 1]
  writeFile "other/tick.md" $
    code
      [ "sum to " <> show a
      , "first measure: " <> show t <> " cycles"
      , "second measure: " <> show t2 <> " cycles"
      , "average over next " <> show n <> ": " <> sformat (fixed 2) (average ts) <>
        " cycles"
      , "[min, 30th, median, 90th, 99th, max]:"
      , mconcat (sformat (" " % prec 4) <$> qt ts)
      ]

  -- | various versions of tick
 
  rio <- run "ticksIO" $ ticksIO n (pure $ fMono a')
  rpure <- run "ticksPure" $ ticks n fMono a'
  rinline <- run "ticksInline" $ ticksInline n fMono a'
  rnoinline <- run "ticksNoinline" $ ticksNoinline n fMono a'
  rinfile <- run "ticksInFile" $ ticksInFile n fMono a'
  rrep <- run "ticksRep" $ ticksRep n fMono a'
  rioLambda <- run "ticksIO Lambda" $ ticksIO n (pure $ fLambda a')
  rpureLambda <- run "ticksPure Lambda" $ ticks n fLambda a'
  rinlineLambda <- run "ticksInline Lambda" $ ticksInline n fLambda a'
  rioPoly <- run "ticksIO Poly" $ ticksIO n (pure $ fPoly a')
  rpurePoly <- run "ticksPure Poly" $ ticks n fPoly a'
  rinlinePoly <- run "ticksInline Poly" $ ticksInline n fPoly a'

  writeFile "other/ticks.md" $
    code [ "sum to " <> show a <> " n = " <> show n <> " prime run: " <>
           sformat (prec 3) t
         , formatRunHeader
         , rio
         , rpure
         , rinline
         , rnoinline
         , rinfile
         , rrep
         , rioLambda
         , rpureLambda
         , rinlineLambda
         , rioPoly
         , rpurePoly
         , rinlinePoly
         ]

  -- | overall ticks cost
  let formatGap :: Int -> (Cycle, ([Cycle],b)) -> Text
      formatGap a (co, (ci, _)) =
          sformat
          ("n = " %(left 7 ' ' %. prec 3)%" outside: "%(left 7 ' ' %. prec 3)%" inside: "%(left 7 ' ' %. prec 3)%" gap: "%(left 7 ' ' %. prec 3))
          a co (sum ci) (co - sum ci)
  let runGap t a = formatGap a <$> tickIO t
  gaps <- sequence $ (\a -> runGap (ticks n fPoly a) a) <$> as
  writeFile "other/ticksCost.md" $ code gaps

  -- | tickns
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick fMono) <$> as)
  let r12 =
        "(replicateM n . tick fMono) <$> as: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> css))
  (ts, _) <- tickns n fMono as
  let r13 =
        "Perf.tickns n fMono as: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> ts))
  writeFile "other/tickns.md" $
    code ["sum to's " <> show as, "n = " <> show n, r12, r13]

  -- | vectors
  let asl :: [Double]
      asl = [1 .. a]

  let suml :: [Double] -> Double
      suml = foldl' (+) 0
  
  rlist <- run "ticksInFile list" $ ticksInFile n suml asl
  
  let sumv :: V.Vector Double -> Double
      sumv = V.foldl (+) 0

  let asv :: V.Vector Double =
        (\x -> V.generate (fromIntegral $ floor x) fromIntegral) a

  rboxed <- run "ticksInFile boxed" $ ticksInFile n sumv asv

  let sums :: S.Vector Double -> Double
      sums = S.foldl (+) 0

  let ass :: S.Vector Double =
        (\x -> S.generate (fromIntegral $ floor x) fromIntegral) a

  rstorable <- run "ticksInFile storable" $ ticksInFile n sums ass

  let sumu :: U.Vector Double -> Double
      sumu = U.foldl (+) 0

  let asu :: U.Vector Double =
        (\x -> U.generate (fromIntegral $ floor x) fromIntegral) a

  runboxed <- run "ticksInFile unboxed" $ ticksInFile n sumu asu


  writeFile "other/vector.md" $
    code ["sum to " <> show a, rlist, rboxed, rstorable, runboxed]


{-
  -- perf basics
  (result, cs) <- runPerfT $
      perf "sum" cycles (pure $ foldl' (+) 0 [0 .. floor a :: Integer])
  putStrLn (show (result, cs) :: Text)

  -- PerfT example
  -- prior to Perfification
  result <- do
      txt <- readFile "examples/examples.hs"
      let n = Text.length txt
      let x = foldl' (+) 0 [1..n]
      putStrLn $ "sum of one to number of characters is: " <>
          (show x :: Text)
      pure (n, x)

  -- post-Perfification
  (result', ms) <- runPerfT $ do
          txt <- perf "file read" cycles $ readFile "examples/examples.hs"
          n <- perf "length" cycles $ pure (Text.length txt)
          x <- perf "sum" cycles $ pure (foldl' (+) 0 [1..n])
          perf "print to screen" cycles $
              putStrLn $ "sum of one to number of characters is: " <>
              (show x :: Text)
          pure (n, x)

  when (result == result') $ print "PerfT preserving computations"

  let fmt = sformat ((right 40 ' ' %. stext) %prec 3 % " " % stext)
  writeFile "other/perf.md" $
    "\nperf cycle measurements\n---\n" <>
    code ((\(t,c) -> fmt t c "cycles") <$> Map.toList ms)
-}


code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

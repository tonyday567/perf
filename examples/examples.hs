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

import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text.IO (writeFile, readFile)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Formatting
import Options.Generic
import NumHask.Prelude hiding ((%))
import Perf
import System.CPUTime.Rdtsc

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Double -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

-- | needs more testing
app :: t -> () -> t
app e () = e
{-# NOINLINE app #-}

tick' :: (a -> b) -> a -> IO (Cycle, b)
tick' !f !a = do
  !t <- rdtsc
  !a' <- pure (f a)
  !t' <- rdtsc
  pure (t' - t, a')

ticksTest :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksTest n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t,_) <- Perf.tick' f a
          go f a (n - 1) (t:ts)

ticksNonMemoising :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksNonMemoising n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t,_) <- tick f a
          go f a (n - 1) (t:ts)

ticksMemoising :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksMemoising n f a = do
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

f :: Int -> Int
f = \x -> foldl' (+) 0 $! [1 .. x]

fFloat :: Int -> Int
fFloat x = foldl' (+) 0 [1 .. x]

fPoly x = foldl' (+) 0 [1 .. x]

main :: IO ()
main = do
  o :: Opts <- getRecord "a random bit of text"
  let n = fromMaybe 1000 (runs o)
  let a = fromMaybe 1000 (sumTo o)
  let !a' = fromIntegral $ floor a :: Int


  (cprime, _) <- Perf.ticks n f a'
  let ft cs t =
        sformat
          ((right 40 ' ' %. stext) % prec 3 % " cycles")
          t
          (percentile 0.4 cs)
  let rprime = ft cprime "ticks n f a"

  -- | ttick
  let ft' :: [Cycle] -> Text -> Text
      ft' cs label =
        sformat
          ((right 30 ' ' %. stext) % (right 6 ' ' %. stext) % (right 36 ' ' %. stext) % (right 6 ' ' %. prec 3) % " cycles")
          label
          (mconcat $ sformat (right 6 ' ' %. prec 3 % " ") <$> take 1 cs)
          (mconcat $ sformat (right 6 ' ' %. prec 3 % " ") <$> take 5 cs)
          (percentile 0.4 cs)
  (c1, _) <- ticksNonMemoising n f a'
  let r1 = ft' c1 "ticksNonMemoising n f a"
  (cs, _) <- ticksTest n f a'
  let r2 = ft' cs "ticksTest n f a"
  (cs, _) <- ticksLazy n f a'
  let r3 = ft' cs "ticksLazy n f a"
  (cs, _) <- ticksMemoising n f a'
  let r4 = ft' cs "ticksMemoising n f a"
  writeFile "other/tticks.md" $
    code ["sum to " <> show a, "n = " <> show n, r1, r2, r3, r4]

  
  -- perf
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
  _ <- warmup 100

  (t, _) <- tick f a'
  (ts, _) <- ticks n f a'
  let qt x = (`percentile` x) <$> [0, 0.3, 0.5, 0.9, 0.99, 1]
  writeFile "other/tick.md" $
    code
      [ "sum to " <> show a
      , "first measure: " <> show t <> " cycles"
      , "average over next " <> show n <> ": " <> sformat (fixed 2) (average ts) <>
        " cycles"
      , "[min, 30th, median, 90th, 99th, max]:"
      , mconcat (sformat (" " % prec 4) <$> qt ts)
      ]

  -- | ticks & friends
  (cs, _) <- ticksIO n (pure $ f a')
  let r3 = ft cs "ticksIO n (pure $ f a)"
  (c, _) <- tickq n f a'
  let fq c t = sformat ((right 40 ' ' %. stext) %prec 3 % " cycles") t c
  let r4 = fq c "tickq n f a"
  cs <- fmap fst <$> replicateM n (tick f a')
  let r6 = ft cs "replicateM n (tick f a)"
  cs <- fmap fst <$> replicateM n (tickIO (pure (f a')))
  let r8 = ft cs "replicateM n (tickIO (pure (f a)))"
  cs <- fmap fst <$> replicateM n (tick (app (f a')) ())
  let r9 = ft cs "replicateM n (tick (app (f a)) ())"
  cs <- fmap fst <$> replicateM n (tick identity (f a'))
  let r10 = ft cs "replicateM n (tick identity (f a))"
  cs <- fmap fst <$> replicateM n (tick (const (f a')) ())
  let r11 = ft cs "replicateM n (tick (const (f a)) ())"
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick f) <$> [1, 10, 100, 1000 :: Int])
  let r12 =
        "(replicateM n . tick f) <$> [1,10,100,1000]: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> css))
  (ts, _) <- Perf.tickns n f [1, 10, 100, 1000 :: Int]
  let r13 =
        "Perf.tickns n f [1,10,100,1000]: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> ts))
  writeFile "other/ticks.md" $
    code ["sum to " <> show a, "n = " <> show n, rprime, r3, r4, r6, r8, r9, r10, r11, r12, r13]


  -- vectors
  let sumv :: V.Vector Double -> Double
      sumv = V.foldl (+) 0

  let asv :: V.Vector Double =
        (\x -> V.generate (fromIntegral $ floor x) fromIntegral) a

  (t, _) <- Perf.ticks n sumv asv
  let rboxed = sformat ("boxed: " %prec 3) (percentile 0.4 t)

  let sums :: S.Vector Double -> Double
      sums = S.foldl (+) 0

  let ass :: S.Vector Double =
        (\x -> S.generate (fromIntegral $ floor x) fromIntegral) a

  (t, _) <- Perf.ticks n sums ass
  let rstorable = sformat ("storable: " %prec 3) (percentile 0.4 t)

  let sumu :: U.Vector Double -> Double
      sumu = U.foldl (+) 0

  let asu :: U.Vector Double =
        (\x -> U.generate (fromIntegral $ floor x) fromIntegral) a

  (t, _) <- Perf.ticks n sumu asu
  let runboxed = sformat ("unboxed: " %prec 3) (percentile 0.4 t)

  writeFile "other/vector.md" $
    code ["sum to " <> show a, rboxed, rstorable, runboxed]

  (t, _) <- Perf.ticks n f a'
  putStrLn $ sformat ("Perf.Cycle.ticks n f a: " %prec 3) (percentile 0.4 t)

  -- perf basics
  (result, cs) <- runPerfT $
      perf "sum" cycles (pure $ foldl' (+) 0 [0 .. floor a :: Integer])
  putStrLn (show (result, cs) :: Text)


code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

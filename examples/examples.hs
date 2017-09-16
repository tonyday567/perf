{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Data.Text as Text
import Data.Text.IO (writeFile)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Formatting
import Options.Generic
import NumHask.Prelude hiding ((%))
import Perf

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  , sumsTo :: Maybe [Int] -- <?> "sum to these numbers"
  , flipMaybe :: Maybe Bool
  } deriving (Generic, Show)

instance ParseField [Int]

instance ParseRecord Opts

ticksInFile :: (NFData b) => Int -> (a -> b) -> a -> IO ([Cycle], b)
ticksInFile n0 f a = go f a n0 []
  where
    go f a n ts
      | n <= 0 = pure (ts, f a)
      | otherwise = do
          (t,_) <- tick f a
          go f a (n - 1) (t:ts)

fLambda :: Int -> Int
fLambda = \x -> foldl' (+) 0 [1 .. x]

fMono :: Int -> Int
fMono x = foldl' (+) 0 [1 .. x]

fPoly :: (Enum b, Num b, Additive b) => b -> b
fPoly x = foldl' (+) 0 [1 .. x]


formatone :: (Real a) => Text -> a -> Text
formatone =
        sformat
          ((right 24 ' ' %. stext) %
           (left 7 ' ' %. prec 3) % " cycles")

runone :: (Real a) => Text -> IO (a, b) -> IO Text
runone label t = formatone label . fst <$> t

main :: IO ()
main = do
  o :: Opts <- getRecord "a random bit of text"
  let n = fromMaybe 1000 (runs o)
  let a = fromMaybe 1000 (sumTo o)
  let !a' = fromIntegral a
  let !a'' = a'+1
  let !a''' = a'+1
  let as = [1, 10, 100, 1000 :: Int]
  let fm = fromMaybe True (flipMaybe o)

  writeFile "other/run.md" $
    code
      [ sformat ((right 24 ' ' %. stext)%prec 3)  "number of runs:" n
      , sformat ((right 24 ' ' %. stext)%prec 3)  "accumulate to:" a
      , sformat ((right 24 ' ' %. stext)%stext) "function:" "foldl' (+) 0"
      ]

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
  (t, resultPrime) <- tick fMono a'
  print resultPrime
  (t2,_) <- tick fMono a'
  writeFile "other/tick.md" $
    code
      [ "sum to " <> show a
      , "first measure: " <> show t <> " cycles"
      , "second measure: " <> show t2 <> " cycles"
      ]

  -- | various versions of tick
  rpure <- run "ticks" $ ticks n fMono a'
  rpurePoly <- run "ticks (poly)" $ ticks n fPoly a
  rpureLambda <- run "ticks (lambda)" $ ticks n fLambda a'
  rio <- run "ticksIO" $ ticksIO n (pure $ fMono a'')
  rioPoly <- run "ticksIO (poly)" $ ticksIO n (pure $ fPoly a)
  rioLambda <- run "ticksIO (lambda)" $ ticksIO n (pure $ fLambda a')

  writeFile "other/ticks.md" $
    code [ "sum to " <> show a <> " n = " <> show n <> " prime run: " <>
           sformat (prec 3) t
         , formatRunHeader
         , rpure
         , rpureLambda
         , rpurePoly
         , rio
         , rioLambda
         , rioPoly
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

  -- | ns
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick fMono) <$> as)
  let r12 =
        "(replicateM n . tick fMono) <$> as: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> css))
  (ts, _) <-ns (ticks n fMono) as
  let r13 =
        "ns (ticks n fMono) as: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> ts))
  writeFile "other/tickns.md" $
    code ["sum to's " <> show as, r13, r12]

  -- | vectors
  let asl :: [Int]
      asl = [1 .. a]
  let suml :: [Int] -> Int
      suml = foldl' (+) 0
  rlist <- run "ticks list" $ ticks n suml asl
  let sumv :: V.Vector Int -> Int
      sumv = V.foldl (+) 0
  let asv :: V.Vector Int =
        V.generate a identity
  rboxed <- run "ticks boxed" $ ticks n sumv asv
  let sums :: S.Vector Int -> Int
      sums = S.foldl (+) 0
  let ass :: S.Vector Int =
        S.generate a identity
  rstorable <- run "ticks storable" $ ticks n sums ass
  let sumu :: U.Vector Int -> Int
      sumu = U.foldl (+) 0
  let asu :: U.Vector Int =
        U.generate a identity
  runboxed <- run "ticks unboxed" $ ticks n sumu asu
  writeFile "other/vector.md" $
    code ["sum to " <> show a, rlist, rboxed, rstorable, runboxed]


  -- WHNF
  let just' x
        | fm = Just x
        | otherwise = Nothing

  rnf <- runone "tick" $ tick (fmap fMono) (just' a')
  rwhnf <- runone "tickWHNF" $ tick (fmap fMono) (just' a')
  rnfs <- run "ticks" $ ticks n (fmap fMono) (just' a')
  rwhnfs <- run "ticksWHNF" $ ticksWHNF n (fmap fMono) (just' a')

  rnfio <- runone "tickIO" $ tickIO (pure $ fmap fMono (just' a'))
  rwhnfio <- runone "tickWHNFIO" $ tickWHNFIO (pure $ fmap fMono (just' a'))
  rnfsio <- run "ticksIO" $ ticksIO n (pure $ fmap fMono (just' a'''))
  rwhnfsio <- run "ticksWHNFIO" $ ticksWHNFIO n (pure $ fmap fMono (just' a'))

  writeFile "other/whnf.md" $
    code ["sum to " <> show a,
          rnf, rwhnf, rnfs, rwhnfs, rnfio, rwhnfio, rnfsio, rwhnfsio]

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

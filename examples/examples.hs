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
import Data.Scientific

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

prec :: Int -> Format r (Scientific -> r)
prec n = scifmt Exponent (Just n)

int2Sci :: ToInteger a => a -> Scientific
int2Sci n = scientific (fromIntegral n) 0

formatone :: (ToInteger a) => Text -> Int -> a -> Text
formatone t p x =
        sformat
          ((right 24 ' ' %. stext) %
           (left 8 ' ' %. prec p) % " cycles") t (int2Sci x)

runone :: (ToInteger a) => Text -> Int -> IO (a, b) -> IO Text
runone label p t = formatone label p . fst <$> t

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
      [ sformat ((right 24 ' ' %. stext)%prec 1)  "number of runs:" (int2Sci n)
      , sformat ((right 24 ' ' %. stext)%prec 1)  "accumulate to:" (int2Sci a)
      , sformat ((right 24 ' ' %. stext)%stext) "function:" "foldl' (+) 0"
      ]

  let formatRun :: [Cycle] -> Text -> Int -> Text
      formatRun cs label p =
        sformat
          ((right 24 ' ' %. stext) % stext %
           (left 7 ' ' %. prec p) % " cycles")
          label
          (Text.intercalate " " $ sformat (left 7 ' ' %. prec p) .
           int2Sci <$>
           take 5 cs)
          (fromFloatDigits $ percentile 0.4 cs)

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

  let run label p t = (\x -> formatRun x label p) . fst <$> t

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
      , mconcat (sformat (" " % prec 4) . fromFloatDigits <$> qticks)
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
  rpure <- run "ticks" 2 $ ticks n fMono a'
  rpurePoly <- run "ticks (poly)" 2 $ ticks n fPoly a
  rpureLambda <- run "ticks (lambda)" 2 $ ticks n fLambda a'
  rio <- run "ticksIO" 2 $ ticksIO n (pure $ fMono a'')
  rioPoly <- run "ticksIO (poly)" 2 $ ticksIO n (pure $ fPoly a)
  rioLambda <- run "ticksIO (lambda)" 2 $ ticksIO n (pure $ fLambda a')

  writeFile "other/ticks.md" $
    code [ "sum to " <> show a <> " n = " <> show n <> " prime run: " <>
           sformat (prec 3) (int2Sci t)
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
          (int2Sci a) (int2Sci co) (int2Sci $ sum ci) (int2Sci $ co - sum ci)
  let runGap t a = formatGap a <$> tickIO t
  gaps <- sequence $ (\a -> runGap (ticks n fPoly a) a) <$> as
  writeFile "other/ticksCost.md" $ code gaps

  -- | ns
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick fMono) <$> as)
  let r12 =
        "(replicateM n . tick fMono) <$> as: " <>
        mconcat (sformat (" " %prec 3) <$> (fromFloatDigits . percentile 0.4 <$> css))
  (ts, _) <-ns (ticks n fMono) as
  let r13 =
        "ns (ticks n fMono) as: " <>
        mconcat (sformat (" " %prec 3) <$> (fromFloatDigits . percentile 0.4 <$> ts))
  writeFile "other/tickns.md" $
    code ["sum to's " <> show as, r13, r12]

  -- | vectors
  let asl :: [Int]
      asl = [1 .. a]
  let suml :: [Int] -> Int
      suml = foldl' (+) 0
  rlist <- run "ticks list" 2 $ ticks n suml asl
  let sumv :: V.Vector Int -> Int
      sumv = V.foldl (+) 0
  let asv :: V.Vector Int =
        V.generate a identity
  rboxed <- run "ticks boxed" 2 $ ticks n sumv asv
  let sums :: S.Vector Int -> Int
      sums = S.foldl (+) 0
  let ass :: S.Vector Int =
        S.generate a identity
  rstorable <- run "ticks storable" 2 $ ticks n sums ass
  let sumu :: U.Vector Int -> Int
      sumu = U.foldl (+) 0
  let asu :: U.Vector Int =
        U.generate a identity
  runboxed <- run "ticks unboxed" 2 $ ticks n sumu asu
  writeFile "other/vector.md" $
    code ["sum to " <> show a, rlist, rboxed, rstorable, runboxed]


  -- WHNF
  let just' x
        | fm = Just x
        | otherwise = Nothing

  rnf <- runone "tick" 2 $ tick (fmap fMono) (just' a')
  rwhnf <- runone "tickWHNF" 2 $ tick (fmap fMono) (just' a')
  rnfs <- run "ticks" 2 $ ticks n (fmap fMono) (just' a')
  rwhnfs <- run "ticksWHNF" 2 $ ticksWHNF n (fmap fMono) (just' a')

  rnfio <- runone "tickIO" 2 $ tickIO (pure $ fmap fMono (just' a'))
  rwhnfio <- runone "tickWHNFIO" 2 $ tickWHNFIO (pure $ fmap fMono (just' a'))
  rnfsio <- run "ticksIO" 2 $ ticksIO n (pure $ fmap fMono (just' a'''))
  rwhnfsio <- run "ticksWHNFIO" 2 $ ticksWHNFIO n (pure $ fmap fMono (just' a'))

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

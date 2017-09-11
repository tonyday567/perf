{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Chart
import Data.List (last)
import Data.Text (intercalate)
import Data.Text.IO (writeFile)

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import Diagrams.Prelude
import Formatting
import Options.Generic
import Protolude hiding ((%), intercalate)

import Perf

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Double -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

ticks :: Int -> (a -> b) -> a -> IO ([Cycle], b)
ticks n f a = do
  ts <- replicateM' n (tick f a)
  pure (fst <$> ts, snd $ last ts)

qtick :: Int -> (a -> b) -> a -> IO (Double, b)
qtick n f a = do
  ts <- replicateM' n (tick f a)
  pure (percentile 0.4 $ fst <$> ts, snd $ last ts)

main :: IO ()
main = do
  o :: Opts <- getRecord "a random bit of text"
  let n = fromMaybe 1000 (runs o)
  let a = fromMaybe 10000 (sumTo o)


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

{-

  -- tick
  _ <- warmup 100
  let f x = foldl' (+) 0 [1 .. x]
  (t, _) <- tick f a
  (ts, _) <- Main.ticks n f a
  let qt x = (`percentile` x) <$> [0, 0.3, 0.5, 0.9, 0.99, 1]
  writeFile "other/tick.md" $
    code
      [ "one tick: " <> show t <> " cycles"
      , "average over " <> show n <> ": " <> sformat (fixed 2) (average ts) <>
        " cycles"
      , "[min, 30th, median, 90th, 99th, max]:"
      , mconcat (sformat (" " % prec 4) <$> qt ts)
      ]

  -- | ticks & friends
  (cs, _) <- Perf.ticks n f a
  let ft cs t =
        sformat
          ((right 40 ' ' %. stext) % prec 3 % " cycles")
          t
          (percentile 0.4 cs)
  let r1 = ft cs "Perf.ticks n f a"
  (cs, _) <- Main.ticks n f a
  let r2 = ft cs "Main.ticks n f a"
  (cs, _) <- Perf.ticksIO n (pure $ f a)
  let r3 = ft cs "Perf.ticksIO n (pure $ f a)"
  (c, _) <- Perf.qtick n f a
  let fq c t = sformat ((right 40 ' ' %. stext) %prec 3 % " cycles") t c
  let r4 = fq c "Perf.qtick n f a"
  (c, _) <- Main.qtick n f a
  let r5 = fq c "Main.qtick n f a"
  cs <- fmap fst <$> replicateM n (tick f a)
  let r6 = ft cs "replicateM n (tick f a)"
  cs <- fmap fst <$> replicateM' n (tick f a)
  let r7 = ft cs "replicateM' n (tick f a)"
  cs <- fmap fst <$> replicateM n (tickIO (pure (f a)))
  let r8 = ft cs "replicateM n (tickIO (pure (f a)))"
  cs <- fmap fst <$> replicateM n (tick (app (f a)) ())
  let r9 = ft cs "replicateM n (tick (app (f a)) ())"
  cs <- fmap fst <$> replicateM n (tick identity (f n))
  let r10 = ft cs "replicateM n (tick identity (f n))"
  cs <- fmap fst <$> replicateM n (tick (const (f a)) ())
  let r11 = ft cs "replicateM n (tick (const (f a)) ())"
  css <-
    fmap (fmap fst) <$>
    sequence ((replicateM n . tick f) <$> [1, 10, 100, 1000, 10000])
  let r12 =
        "(replicateM n . tick f) <$> [1,10,100,1000,10000]: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> css))
  (ts, _) <- Perf.tickns n f [1, 10, 100, 1000, 10000]
  let r13 =
        "Perf.tickns n f [1,10,100,1000,10000]: " <>
        mconcat (sformat (" " %prec 3) <$> (percentile 0.4 <$> ts))
  writeFile "other/ticks.md" $
    code ["sum to " <> show a, r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13]
-}
  -- vectors

{-
  let sumv :: V.Vector Double -> Double
      sumv = V.foldl (+) 0

  let asv :: V.Vector Double =
        (\x -> V.generate (floor x) fromIntegral) a

  (t, _) <- Perf.ticks n sumv asv
  let rboxed = sformat ("boxed: " %prec 3) (percentile 0.4 t)

  let sums :: S.Vector Double -> Double
      sums = S.foldl (+) 0

  let ass :: S.Vector Double =
        (\x -> S.generate (floor x) fromIntegral) a

  (t, _) <- Perf.ticks n sums ass
  let rstorable = sformat ("storable: " %prec 3) (percentile 0.4 t)

  let sumu :: U.Vector Double -> Double
      sumu = U.foldl (+) 0

  let asu :: U.Vector Double =
        (\x -> U.generate (floor x) fromIntegral) a

  (t, _) <- Perf.ticks n sumu asu
  let runboxed = sformat ("unboxed: " %prec 3) (percentile 0.4 t)

  writeFile "other/vector.md" $
    code ["sum to " <> show a, rboxed, rstorable, runboxed]
-}

{-
  (t, _) <- Perf.ticks n f a
  putStrLn $ sformat ("Perf.Cycle.ticks n f a: " %prec 3) (percentile 0.4 t)

-}  

  pure ()

{-
  res <-
    execPerfT $ do
      xs <- perf "sum1" cycles (sum1 a n)
      d1 <- perf "decile calc" cycles (pure $ Main.deciles xs)
      xs1 <-
        perf "truncation" cycles $ pure $ (\x -> min x ((d1 !! 5) * tr)) <$> xs
      _ <-
        perf "prints to stdout" cycles $ do
          putStrLn $ "inner loop quantiles: " <> (show d1 :: Text)
          putStrLn $
            "inner loop total cycles: " <> (show (foldl' (+) 0 xs1) :: Text)
      _ <-
        perf "chart creation" cycles $
          if chart o
            then let name = fromMaybe "other/summing.svg" (chartName o)
                 in fileSvg (unpack name) (750, 250) $ pad 1.1 $ histLine xs1
            else pure ()
      pure ()
  putStrLn $ showPerf res

-}


code :: [Text] -> Text
code cs = "\n```\n" <> intercalate "\n" cs <> "\n```\n"

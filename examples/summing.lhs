./examples/summing +RTS -s -H32 -RTS --loops 10000 --sumTo 100 --truncAt 10 --chart
https://www.cheatography.com/nash/cheat-sheets/ghc-and-rts-options/
http://ghc.readthedocs.io/en/8.0.2/sooner.html

> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric     #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE TypeOperators     #-}
> import Data.Primitive.MutVar
> import Data.Text (pack, unpack, intercalate)
> import Data.Text.IO (writeFile)
> import Formatting
> import Protolude hiding ((%), intercalate)
> import Data.List.NonEmpty (NonEmpty(..))
> 
> import qualified Control.Foldl as L
> import qualified Data.Vector as V
> import Perf
> import Perf.Measure
> import Perf.Cycles
> import Data.TDigest

All the imports that are needed for charts

> import Chart.Unit
> import Chart.Types
> import Chart.Range
> import Data.Default
> import Diagrams.Prelude
> import Linear.V4
> import Data.List ((!!), zipWith4)
> 

imports for random numbers

> import System.Random.MWC.Probability

command line options parsing

> import Options.Generic

printing of performance output

> import qualified Data.Map.Strict as Map

> data Opts = Opts
>   { runs :: Maybe Int             -- <?> "number of runs"
>   , sumTo :: Maybe Double         -- <?> "sum to this number"
>   , chart :: Bool           -- <?> create chart
>   , truncAt :: Maybe Double -- <?> truncate chart values above median*truncAt
>   , chartName :: Maybe Text -- <?> name of chart
>   }
>   deriving (Generic, Show)
> instance ParseRecord Opts
>

> main :: IO ()
> main = do
>     o :: Opts <- getRecord "tester"
>     let n = fromMaybe 10000 (runs o)
>     let a = fromMaybe 1000 (sumTo o)
>     let tr = fromMaybe 100 (truncAt o)
>     res <- execPerfT $ do
>         xs <- perf "sum1" cycles (sum1 a n)
>         d1 <- perf "decile calc" cycles (pure $ deciles xs)
>         xs1 <- perf "truncation" cycles $ pure $ (\x -> min x ((d1!!5)*tr)) <$> xs
>         _ <- perf "prints to stdout" cycles $ do
>           putStrLn $ "inner loop quantiles: " <> (show $ d1 :: Text)
>           putStrLn $ "inner loop total cycles: " <> (show (foldl' (+) 0 xs1) :: Text)
>         _ <- perf "chart creation" cycles $ do
>           if (chart o) then
>             let name = fromMaybe "other/summing.svg" (chartName o) in
>             fileSvg (unpack name) (750,250) $ pad 1.1 $
>             ((hist [def] wideScreen
>             [zipWith4 V4 [0..] (repeat 0) [1..] xs1]) <>
>             (axes def wideScreen
>             [ toCorners (V2
>              (Range (0.0,(fromIntegral $ length xs1)))
>              (Range (0,(L.fold (L.Fold max 0 identity) xs1))))]))
>           else (pure ())
>         pure ()
>     putStrLn $ showPerf res

> sum1 :: Double -> Int -> IO [Double]
> sum1 a n = do
>     _ <- warmup 100
>     let f :: Double -> Double; f x = foldl' (+) 0 [1..x]
>     (cs,_) <- spin n tick f a
>     let cs1 = fromIntegral <$> cs :: [Double]
>     pure cs1

> deciles :: [Double] -> [Double]
> deciles xs =
>   ((\x -> fromMaybe 0 $ quantile x (tdigest xs :: TDigest 25)) <$> ((0.1*) <$> [0..10]))

> truncOver tr m xs = (\x -> min x (m*tr)) <$> xs

> showPerf :: Map Text Cycles -> Text
> showPerf ps = Map.foldrWithKey (\k a b -> b <> "\n" <>
>     sformat ((right 20 ' ' %. Formatting.stext) % ": " % Formatting.expt 3) k a) mempty ps

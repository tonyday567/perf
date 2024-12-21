{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Test.Tasty.Bench
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Options.Applicative
import Perf.Report
import GHC.Generics
import Data.Bifunctor
import Perf.Algos
import Data.List (intercalate)
import Perf
import Control.Monad
import Data.Bool

partial :: [a] -> Maybe ([a], a)
partial [] = Nothing
partial s = Just (init s, last s)

naive ::  [a] -> Maybe ([a], a)
naive [] = Nothing
naive (x : xs) = let l = x :| xs in Just ( NE.init l, NE.last l)

-- The existing unsnoc, copied from the base package:
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
{-# INLINABLE unsnoc #-}

-- The existing unsnoc, copied from the base package, but with the tilde removed:
unsnocNoTilde :: [a] -> Maybe ([a], a)
unsnocNoTilde = foldr (\x -> Just . maybe ([], x) (Data.Bifunctor.first (x :))) Nothing
{-# INLINABLE unsnocNoTilde #-}

unsnocGo :: [a] -> Maybe ([a], a)
unsnocGo [] = Nothing
unsnocGo (x : xs) = Just $ go (x :| xs)
 where
  go (y :| []) = ([], y)
  go (y1 :| (y2 : ys)) = Data.Bifunctor.first (y1 :) (go (y2 :| ys))
{-# INLINABLE unsnocGo #-}

unsnocMerge :: [a] -> Maybe ([a], a)
-- The lazy pattern ~(a, b) is important to be productive on infinite lists
-- and not to be prone to stack overflows.
-- Expressing the recursion via 'foldr' provides for list fusion.
unsnocMerge [] = Nothing
unsnocMerge (x : xs) = Just $ go (x :| xs)
 where
  go (y :| []) = ([], y)
  go (y1 :| (y2 : ys)) = (\(~(a, b)) -> (y1 : a, b)) (go (y2 :| ys))
{-# INLINABLE unsnocMerge #-}


data Run = RunUnsnoc | RunUnsnocAll deriving (Eq, Show)

data Algo = AlgoMerge | AlgoPartial | AlgoNaive | AlgoUnsnoc | AlgoUnsnocNoTilde | AlgoUnsnocGo deriving (Show, Eq, Generic)

data AppConfig = AppConfig
  {
    appRun :: Run,
    appAlgo :: Algo,
    appFusion :: Bool,
    appReportOptions :: ReportOptions
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig RunUnsnoc AlgoPartial False defaultReportOptions

parseRun :: Parser Run
parseRun =
  flag' RunUnsnoc (long "single" <> help "run a single algo") <|>
  flag' RunUnsnocAll (long "all" <> help "run all algorithms")
    <|> pure RunUnsnoc

parseAlgos :: Parser Algo
parseAlgos =
  flag' AlgoNaive (long "naive") <|>
  flag' AlgoPartial (long "partial") <|>
  flag' AlgoUnsnoc (long "unsnoc") <|>
  flag' AlgoUnsnocNoTilde (long "unsnocnotilde") <|>
  flag' AlgoUnsnocGo (long "unsnocgo") <|>
  flag' AlgoMerge (long "merge") <|>
  pure AlgoNaive

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseRun
    <*> parseAlgos
    <*> switch (long "fusion")
    <*> parseReportOptions (appReportOptions def)

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> header "unsnoc testing")

perfAlgosFusion :: Algo -> Int -> PerfT IO [[Double]] ()
perfAlgosFusion AlgoNaive l = void $ ffap "naive" (\x -> naive [1 ..x]) l
perfAlgosFusion AlgoPartial l = void $ ffap "partial" (\x -> partial [1 ..x]) l
perfAlgosFusion AlgoUnsnoc l = void $ ffap "unsnoc" (\x -> unsnoc [1 ..x]) l
perfAlgosFusion AlgoUnsnocNoTilde l = void $ ffap "unsnocNoTilde" (\x -> unsnocNoTilde [1 ..x]) l
perfAlgosFusion AlgoUnsnocGo l = void $ ffap "unsnocGo" (\x -> unsnocGo [1 ..x]) l
perfAlgosFusion AlgoMerge l = void $ ffap "unsnocMerge" (\x -> unsnocMerge [1 ..x]) l

perfAlgos :: Algo -> Int -> PerfT IO [[Double]] ()
perfAlgos AlgoNaive l = void $ ffap "naive" naive [1 .. l]
perfAlgos AlgoPartial l = void $ ffap "partial" partial [1 ..l]
perfAlgos AlgoUnsnoc l = void $ ffap "unsnoc" unsnoc [1 ..l]
perfAlgos AlgoUnsnocNoTilde l = void $ ffap "unsnocNoTilde" unsnocNoTilde [1 ..l]
perfAlgos AlgoUnsnocGo l = void $ ffap "unsnocGo" unsnocGo [1 ..l]
perfAlgos AlgoMerge l = void $ ffap "unsnocMerge" unsnocMerge [1 ..l]

perfAlgosAll :: Int -> PerfT IO [[Double]] ()
perfAlgosAll l = do
  void $ ffap "naive" (\x -> naive [1 ..x]) l
  void $ ffap "partial" (\x -> partial [1 ..x]) l
  void $ ffap "unsnoc" (\x -> unsnoc [1 ..x]) l
  void $ ffap "unsnocNoTilde" (\x -> unsnocNoTilde [1 ..x]) l
  void $ ffap "unsnocGo" (\x -> unsnocGo [1 ..x]) l
  void $ ffap "unsnocMerge" (\x -> unsnocMerge [1 ..x]) l

perfAlgosAllFusion :: Int -> PerfT IO [[Double]] ()
perfAlgosAllFusion l = do
  void $ ffap "naive" naive [1 ..l]
  void $ ffap "partial" partial [1 ..l]
  void $ ffap "unsnoc" unsnoc [1 ..l]
  void $ ffap "unsnocNoTilde" unsnocNoTilde [1 ..l]
  void $ ffap "unsnocGo" unsnocGo [1 ..l]
  void $ ffap "unsnocMerge" unsnocMerge [1 ..l]

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let repOptions = appReportOptions o
  let !l = reportLength repOptions
  let a = appAlgo o
  let r = appRun o

  case r of
    RunUnsnoc -> do
      reportMain ExampleSum repOptions (intercalate "-" [show r, show a, show l]) (bool (perfAlgos a) (perfAlgosFusion a) (appFusion o))
    RunUnsnocAll -> do
      reportMain ExampleSum repOptions (intercalate "-" [show r, show a, show l]) (bool perfAlgosAll perfAlgosAllFusion (appFusion o))

{-
tasty :: IO ()
tasty = defaultMain
  [ bgroup "unsnoc" [ bench "partial" $ nf partial list
                    , bench "naive" $ nf naive list
                    , bench "unsnoc" $ nf unsnoc list
                    , bench "unsnocNoTilde" $ nf unsnocNoTilde list
                    , bench "unsnocGo" $ nf unsnocGo list
                    ]
  ]
-}

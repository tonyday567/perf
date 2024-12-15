{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.DeepSeq
import Control.Monad
import Control.Monad.State.Lazy
import Data.FormatN
import Data.List (intercalate, nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Exts
import GHC.Generics
import Optics.Core
import Options.Applicative
import Perf
import Prelude

data Run = RunExample | RunExamples | RunClocks | RunNub | RunExampleIO | RunSums | RunLengths | RunNoOps | RunTicks deriving (Eq, Show)

data AppConfig = AppConfig
  { appReportOptions :: ReportOptions,
    appRun :: Run,
    appLength :: Int,
    appExample :: Example
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig defaultReportOptions RunExample 1000 ExampleSum

parseRun :: Parser Run
parseRun =
  flag' RunSums (long "sums" <> help "run on sum algorithms")
    <|> flag' RunLengths (long "lengths" <> help "run on length algorithms")
    <|> flag' RunNub (long "nub" <> help "nub test")
    <|> flag' RunClocks (long "clocks" <> help "clock test")
    <|> flag' RunExamples (long "examples" <> help "run on example algorithms")
    <|> flag' RunExample (long "example" <> help "run on the example algorithm")
    <|> flag' RunExampleIO (long "exampleIO" <> help "exampleIO test")
    <|> flag' RunNoOps (long "noops" <> help "noops test")
    <|> flag' RunTicks (long "ticks" <> help "tick test")
    <|> pure RunExample

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseReportOptions (view #appReportOptions def)
    <*> parseRun
    <*> option auto (value (view #appLength def) <> long "length" <> short 'l' <> help "length of list")
    <*> parseExample

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "perf exploration" <> header "examples of perf usage")

-- | * exampleIO
exampleIO :: (Semigroup t) => PerfT IO t ()
exampleIO = do
  txt <- fam "file-read" (Text.readFile "src/Perf.hs")
  n <- ffap "length" Text.length txt
  fam "print-result" (Text.putStrLn $ "length of file is: " <> Text.pack (show n))

-- | * sums
-- | measure the various versions of a tick.
statTicks :: (NFData t, NFData b) => Text -> (t -> b) -> t -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
statTicks l f a n s = do
  addStat [l, "tick"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tick n f a)
  addStat [l, "tickWHNF"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickWHNF n f a)
  addStat [l, "tickLazy"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickLazy n f a)
  addStat [l, "tickForce"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickForce n f a)
  addStat [l, "tickForceArgs"] . statD s . fmap fromIntegral =<< lift (fst <$> multi tickForceArgs n f a)
  addStat [l, "stepTime"] . statD s . fmap fromIntegral =<< lift (mconcat . fmap snd . take 1 . Map.toList <$> execPerfT (toMeasureN n stepTime) (f |$| a))
  addStat [l, "times"] . statD s . fmap fromIntegral =<< lift (mconcat . fmap snd . take 1 . Map.toList <$> execPerfT (times n) (f |$| a))
  addStat [l, "timesn"] . statD s . fmap fromIntegral =<< lift (mconcat . fmap snd . Map.toList <$> execPerfT (pure <$> timesN n) (f |$| a))

statTicksSum :: (NFData b, Enum b, Num b) => SumPattern b -> Int -> StatDType -> StateT (Map.Map [Text] Double) IO ()
statTicksSum (SumFuse label f a) n s = statTicks label f a n s
statTicksSum (SumFusePoly label f a) n s = statTicks label f a n s
statTicksSum (SumPoly label f a) n s = statTicks label f a n s
statTicksSum (SumMono label f a) n s = statTicks label f a n s

statTicksSums :: Int -> Int -> StatDType -> IO (Map.Map [Text] Double)
statTicksSums n l s = flip execStateT Map.empty $ mapM_ (\x -> statTicksSum x n s) (allSums l)

-- * no-op testing

perfNoOps :: (Semigroup a) => Measure IO a -> IO (Map.Map Text a)
perfNoOps meas =
  execPerfT meas $ do
    liftIO $ warmup 1000
    fap "const" (const ()) ()
    fam "pure" (pure ())

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let repOptions = appReportOptions o
  let n = reportN repOptions
  let s = reportStatDType repOptions
  let mt = reportMeasureType repOptions
  let c = reportClock repOptions
  let !l = appLength o
  let a = appExample o
  let r = appRun o

  case r of
    RunNub -> do
      reportMain repOptions (show r) (void $ ffap "nub" nub [1 .. l])
    RunExample -> do
      reportMain repOptions (intercalate "-" [show r, show a, show l]) $
        testExample (examplePattern a l)
    RunExamples -> do
      reportMain
        repOptions
        (intercalate "-" [show r, show l])
        (statExamples l)
    RunClocks -> do
      reportMain
        repOptions
        (intercalate "-" [show r, show a, show l, show c])
        (testExample (examplePattern a l))
    RunExampleIO -> do
      m1 <- execPerfT (measureDs mt c 1) exampleIO
      (_, (m', m2)) <- outer "outer-total" (measureDs mt c 1) (measureDs mt c 1) exampleIO
      let ms = mconcat [Map.mapKeys (\x -> ["normal", x]) m1, Map.mapKeys (\x -> ["outer", x]) (m2 <> m')]
      putStrLn ""
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show s]) repOptions
      report o' (fmap (statD s) <$> ms)
    RunSums -> do
      m <- statSums n l (measureDs mt c)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n, show l, show s]) repOptions
      report o' (statify s m)
    RunLengths -> do
      m <- statLengths n l (measureDs mt c)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n, show l, show s]) repOptions
      report o' (statify s m)
    RunNoOps -> do
      m <- perfNoOps (measureDs mt c n)
      let o' = replaceDefaultFilePath (intercalate "-" [show r, show mt, show n]) repOptions
      report o' (allStats 4 (Map.mapKeys (: []) m))
    RunTicks -> do
      m <- statTicksSums n l s
      reportOrg2D (fmap (expt (Just 3)) m)

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Perf.Space
  ( ghcStats,
    SpaceStats (..),
    space,
    space2,
    prettyOrgSpace,
    prettyOrgHeaderSpace,
    printOrgSpaceTime,
    spaceTime,
    diffSpace,
    addSpace,
  )
where

import Perf.Types
import Perf.Time
import Control.Monad.State.Lazy
import Prelude hiding (cycle)
import Data.String
import Data.Text (Text)
import GHC.Stats
import Data.Bool
import Data.Word
import System.Mem
import Data.FormatN
import qualified Data.Text.IO as Text
import qualified Data.Text as Text

ghcStats :: StepMeasure IO (Maybe (RTSStats, RTSStats))
ghcStats = StepMeasure start stop
  where
    start = do
      p <- getRTSStatsEnabled
      bool (pure Nothing) (Just <$> getRTSStats) p
    stop s = do
      case s of
        Nothing -> pure Nothing
        Just s' -> do
          s'' <- getRTSStats
          pure $ Just (s',s'')
{-# INLINEABLE ghcStats #-}

data SpaceStats = SpaceStats { allocated :: Word64, gcollects :: Word32, maxLiveBytes :: Word64, gcLiveBytes :: Word64, maxMem :: Word64 } deriving (Read, Show, Eq)

prettyOrgSpace :: SpaceStats -> Text
prettyOrgSpace (SpaceStats x1 x2 x3 x4 x5) = expt (Just 3) (fromIntegral x1) <> "|" <> fixed (Just 0) (fromIntegral x2) <> "|" <> expt (Just 3) (fromIntegral x3)  <> "|" <> expt (Just 3) (fromIntegral x4) <> "|" <> expt (Just 3) (fromIntegral x5)

prettyOrgHeaderSpace :: Text
prettyOrgHeaderSpace = Text.intercalate "|" ["allocated","gcollects","maxLiveBytes","gcLiveBytes","MaxMem"]

printOrgSpaceTime :: [(Word64, (SpaceStats, SpaceStats))] -> IO ()
printOrgSpaceTime xs = do
  Text.putStrLn $ "|" <> "cycles" <> "|" <> prettyOrgHeaderSpace <> "|" <> prettyOrgHeaderSpace <> "|"
  Text.putStrLn "|---"
  mapM_ Text.putStrLn $ (\(c,(s,s')) -> "|" <> expt (Just 3) (fromIntegral c) <> "|" <> prettyOrgSpace s <> "|" <> prettyOrgSpace s' <> "|") <$> xs

diffSpace :: SpaceStats -> SpaceStats -> SpaceStats
diffSpace (SpaceStats x1 x2 x3 x4 x5) (SpaceStats x1' x2' x3' x4' x5') = SpaceStats (x1' - x1) (x2' - x2) (x3' - x3) (x4' - x4) (x5' - x5)

addSpace :: SpaceStats -> SpaceStats -> SpaceStats
addSpace (SpaceStats x1 x2 x3 x4 x5) (SpaceStats x1' x2' x3' x4' x5') = SpaceStats (x1' + x1) (x2' + x2) (x3' + x3) (x4' + x4) (x5' + x5)

getSpace :: RTSStats -> SpaceStats
getSpace s = SpaceStats (allocated_bytes s) (gcs s) (max_live_bytes s) (gcdetails_live_bytes (gc s)) (max_mem_in_use_bytes s)

space :: Bool -> StepMeasure IO SpaceStats
space p = StepMeasure (start p) stop
  where
    start p' = do
      when p' performGC
      getSpace <$> getRTSStats
    stop s = do
      s' <- getSpace <$> getRTSStats
      pure $ diffSpace s s'
{-# INLINEABLE space #-}

space2 :: StepMeasure IO (SpaceStats, SpaceStats)
space2 = StepMeasure start stop
  where
    start = do
      performGC
      getSpace <$> getRTSStats
    stop s = do
      s' <- getSpace <$> getRTSStats
      pure $ (s, s')
{-# INLINEABLE space2 #-}


spaceTime :: StepMeasure IO (Word64, SpaceStats)
spaceTime = (,) <$> stepTime <*> space False

-- FIXME:
-- inners :: Int -> StepMeasure IO (SpaceStats, [SpaceStats])
-- inners n = (,) <$> space <*> multi n space

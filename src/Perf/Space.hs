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
    printOrgSpace,
    printOrgSpaceTime,
    unlistify,
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
import GHC.Stats
import Data.Bool
import Data.Word
import System.Mem
import Data.FormatN
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Perf.Stats

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
prettyOrgSpace (SpaceStats x1 x2 x3 x4 x5) =
  Text.intercalate "|"
  [expt (Just 3) (fromIntegral x1),
   fixed (Just 0) (fromIntegral x2),
   expt (Just 3) (fromIntegral x3),
   expt (Just 3) (fromIntegral x4),
   expt (Just 3) (fromIntegral x5)]

spaceLabels :: [Text]
spaceLabels = ["allocated","gcollects","maxLiveBytes","gcLiveBytes","MaxMem"]

printOrgSpace :: Map.Map [Text] SpaceStats -> IO ()
printOrgSpace m = do
  printOrgHeader m spaceLabels
  void $ Map.traverseWithKey (\k a -> Text.putStrLn ("|" <> Text.intercalate "|" k <> "|" <> prettyOrgSpace a <> "|")) m

printOrgSpaceTime :: Map.Map [Text] (Cycles, SpaceStats) -> IO ()
printOrgSpaceTime m = do
  printOrgHeader m ("time":spaceLabels)
  void $ Map.traverseWithKey (\k (c,s) -> Text.putStrLn (outercalate "|"  (k <> [expt (Just 3) (fromIntegral c), prettyOrgSpace s]))) m

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

space2 :: Bool -> StepMeasure IO (SpaceStats, SpaceStats)
space2 p = StepMeasure start stop
  where
    start = do
      when p performGC
      getSpace <$> getRTSStats
    stop s = do
      s' <- getSpace <$> getRTSStats
      pure (s, s')
{-# INLINEABLE space2 #-}


spaceTime :: StepMeasure IO (Cycles, SpaceStats)
spaceTime = (,) <$> stepTime <*> space False

-- FIXME:
-- inners :: Int -> StepMeasure IO (SpaceStats, [SpaceStats])
-- inners n = (,) <$> space <*> multi n space

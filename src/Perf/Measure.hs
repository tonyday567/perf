{-# LANGUAGE OverloadedStrings #-}

-- | Unification of the various different performance measure types, mostly to unify reporting and data management.
module Perf.Measure
  ( MeasureType (..),
    parseMeasure,
    measureDs,
    measureLabels,
  )
where

import Data.Text (Text)
import Options.Applicative
import Perf.Space
import Perf.Time
import Perf.Types
import Prelude hiding (cycle)

-- | Command-line measurement options.
data MeasureType = MeasureTime | MeasureSpace | MeasureSpaceTime | MeasureAllocation deriving (Eq, Show)

-- | Parse command-line 'MeasureType' options.
parseMeasure :: Parser MeasureType
parseMeasure =
  flag' MeasureTime (long "time" <> help "measure time performance")
    <|> flag' MeasureSpace (long "space" <> help "measure space performance")
    <|> flag' MeasureSpaceTime (long "spacetime" <> help "measure both space and time performance")
    <|> flag' MeasureAllocation (long "allocation" <> help "measure bytes allocated")
    <|> pure MeasureTime

-- | unification of the different measurements to being a list of doubles.
measureDs :: MeasureType -> Int -> Measure IO [[Double]]
measureDs mt n =
  case mt of
    MeasureTime -> fmap ((: []) . fromIntegral) <$> times n
    MeasureSpace -> toMeasureN n (ssToList <$> space False)
    MeasureSpaceTime -> toMeasureN n ((\x y -> ssToList x <> [fromIntegral y]) <$> space False <*> stepTime)
    MeasureAllocation -> fmap ((: []) . fromIntegral) <$> toMeasureN n (allocation False)

-- | unification of the different measurements to being a list of doubles.
measureLabels :: MeasureType -> [Text]
measureLabels mt =
  case mt of
    MeasureTime -> ["time"]
    MeasureSpace -> spaceLabels
    MeasureSpaceTime -> spaceLabels <> ["time"]
    MeasureAllocation -> ["allocation"]

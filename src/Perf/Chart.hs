{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Perf.Chart where

import Chart
import Data.List qualified as List
import Data.Text qualified as Text
import Perf.Stats as Perf
import Optics.Core
import NumHask.Space
import Prettychart
import Control.Category ((>>>))
import Options.Applicative
import GHC.Generics
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Perf.Measure (MeasureType, measureLabels)
import Data.Bifunctor
import Data.Bool
import Data.Maybe

--     m <- fromDump defaultPerfDumpOptions
data PerfChartOptions = PerfChartOptions { doChart :: Bool, chartFilepath :: FilePath, truncateAt :: Double, doScatterChart :: Bool, doTruncateChart :: Bool, doHistChart :: Bool, chartAveragesSize :: Double, chartSmallSize :: Double, chartSmallOpac :: Double, chartBigSize :: Double, chartBigOpac :: Double, chartTitleSize :: Double } deriving (Eq, Show, Generic)

defaultPerfChartOptions :: PerfChartOptions
defaultPerfChartOptions = PerfChartOptions False "other/perf.svg" 10 True True True 0.05 0.01 0.3 0.06 1 0.06

-- | Parse charting options.
parsePerfChartOptions :: PerfChartOptions -> Parser PerfChartOptions
parsePerfChartOptions def =
  (\c fp trunAt scat trun hist -> PerfChartOptions c fp trunAt scat trun hist (view #chartAveragesSize def) (view #chartSmallSize def) (view #chartSmallOpac def) (view #chartBigSize def) (view #chartBigOpac def) (view #chartTitleSize def))
    <$> switch (long "chart" <> short 'c' <> help "chart the raw the result to the chartpath")
    <*> option str (value (view #chartFilepath def) <> long "chartpath" <> help "chart file name")
    <*> option auto (value (view #truncateAt def) <> long "truncateat" <> help "truncate chart data (multiple of median)")
    <*> switch (long "scatter")
    <*> switch (long "truncate")
    <*> switch (long "histogram")

perfCharts :: PerfChartOptions -> Maybe MeasureType -> Map.Map Text [[Double]] -> ChartOptions
perfCharts cfg mt m = bool (head cs) (stackCO stackn NoAlign NoAlign 0 cs) (length cs == 1)
  where
    stackn = length cs & fromIntegral & sqrt @Double & ceiling
    cs = uncurry (perfChart cfg) <$> ps
    ps = mconcat $ fmap (uncurry zip . bimap (\t -> fmap ((t <> ": ") <>) (maybe (Text.pack . show @Int <$> [0..]) measureLabels mt)) List.transpose) (Map.toList m)

perfChart :: PerfChartOptions -> Text -> [Double] -> ChartOptions
perfChart cfg t xs = finalChart
  where
    xsSmall = xs & xify & filter (_y >>> (<upperCutoff))
    xsBig = xs & xify & filter (_y >>> (>=upperCutoff))
    med = median xs
    best = tenth xs
    av = Perf.average xs
    upperCutoff = view #truncateAt cfg * med

    labels =
      [ "average: " <> comma (Just 3) av
      , "median: " <> comma (Just 3) med
      , "best: " <> comma (Just 3) best]

    averagesCT = named "averages" $ zipWith (\x i -> GlyphChart (defaultGlyphStyle & set #size (view #chartAveragesSize cfg) & set #color (palette i) & set #borderColor (palette i) & set #glyphShape (gpalette i)) [Point zero x]) [av,med,best] [2..]

    averagesLegend = defaultLegendOptions & set #legendCharts (zipWith (\t' c -> (t', [c])) labels (toListOf chart' averagesCT)) & set #place PlaceBottom & set #numStacks 3 & set #scaleChartsBy 0.2 & set #legendSize 0.3 & set #alignCharts AlignLeft & set #hgap (-0.2) & set #vgap (-0.1)

    smallChart = mempty @ChartOptions & set #hudOptions defaultHudOptions & set #chartTree (averagesCT <> named "small" [GlyphChart (defaultGlyphStyle & set #size (view #chartSmallSize cfg) & over #color (rgb (palette 0)) & set (#color % opac') (view #chartSmallOpac cfg) & set (#borderColor % opac') (view #chartSmallOpac cfg) & set #glyphShape (gpalette 0)) xsSmall])

    (Rect _ _ y w) = unsafeSpace1 xsBig
    bigRect = Rect 0 (fromIntegral $ length xs) y w

    bigChart = mempty @ChartOptions & set #hudOptions (defaultHudOptions & over #axes (drop 1) & set (#axes % each % #item % #ticks % #tick % numTicks') (Just 2) & set (#axes % each % #item % #ticks % #textTick %? #style % #size) 0.07) & set #chartTree (named "blankrect" [BlankChart defaultStyle [bigRect]] <> named "big" [GlyphChart (defaultGlyphStyle & set #size (view #chartBigSize cfg) & over #color (rgb (palette 0)) & set #glyphShape (gpalette 0) & set (#color % opac') (view #chartBigOpac cfg) & set (#borderColor % opac') (view #chartBigOpac cfg)) xsBig])

    rawYSmall = fmap _y xsSmall
    smallR = computeRangeTick (unsafeSpace1 rawYSmall) (fromMaybe defaultTick (smallChart & preview (#hudOptions % #axes % ix 1 % #item % #ticks % #tick)))
    (Range smallY smallW) = smallR

    smallHist = hhistChart smallR 100 rawYSmall & set (#markupOptions % #chartAspect) (CanvasAspect 0.3) & over #chartTree (<> unnamed [BlankChart defaultStyle [Rect 0 0 smallY smallW]])
    smallChartwHist = horiCO NoAlign 0 [smallHist, smallChart]

    rawYBig = fmap _y xsBig
    bigR = computeRangeTick (unsafeSpace1 rawYBig) (fromMaybe defaultTick (bigChart & preview (#hudOptions % #axes % ix 0 % #item % #ticks % #tick)))
    (Range bigY bigW) = bigR
    bigHist = hhistChart bigR 100 rawYBig & set (#markupOptions % #chartAspect) (CanvasAspect 0.3) & over #chartTree (<> unnamed [BlankChart defaultStyle [Rect 0 0 bigY bigW]])
    bigChartwHist = horiCO NoAlign 0 [bigHist, bigChart]

    smallCT = view #chartTree smallChartwHist
    bigCT = set styleBox' (Just (Rect (-2.01) 0.14 (-0.2) 0.2)) $ view #chartTree bigChartwHist

    finalChart = mempty @ChartOptions & set #chartTree (vert NoAlign 0.1 [smallCT, bigCT]) & set (#hudOptions % #legends) [Priority 10 averagesLegend] & set (#hudOptions % #titles) [Priority 5 (defaultTitleOptions t & set (#style % #size) (view #chartTitleSize cfg))]

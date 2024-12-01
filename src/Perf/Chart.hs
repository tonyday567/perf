{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Perf.Chart where

import Chart
import Data.List qualified as List
import Data.Text qualified as Text
import Perf.Stats as Perf
import Optics.Core
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
data PerfChartOptions =
  PerfChartOptions
    { doChart :: Bool
    , chartFilepath :: FilePath
    , truncateAt :: Double
    , doSmallChart :: Bool
    , doBigChart :: Bool
    , doHistChart :: Bool
    , doAveragesLegend :: Bool
    , averagesStyle :: Style
    , averagesPaletteStart :: Int
    , averagesLegend :: LegendOptions
    , smallStyle :: Style
    , smallHud :: HudOptions
    , bigStyle :: Style
    , bigHud :: HudOptions
    , titleSize :: Double
    , histGrain :: Int
    } deriving (Eq, Show, Generic)

defaultPerfChartOptions :: PerfChartOptions
defaultPerfChartOptions = PerfChartOptions False "other/perf.svg" 10 True True True True (defaultGlyphStyle & set #size 0.05) 2 (defaultLegendOptions & set #place PlaceBottom & set #numStacks 3 & set #scaleChartsBy 0.2 & set #legendSize 0.3 & set #alignCharts AlignLeft & set #hgap (-0.2) & set #vgap (-0.1)) (defaultGlyphStyle & set #size 0.01 & over #color (rgb (palette 0)) & set (#color % opac') 0.3 & set (#borderColor % opac') 0.3 & set #glyphShape (gpalette 0)) defaultHudOptions (defaultGlyphStyle & set #size 0.06 & over #color (rgb (palette 0)) & set #glyphShape (gpalette 0) & set (#color % opac') 0.3  & set (#borderColor % opac') 1) (defaultHudOptions & over #axes (drop 1) & set (#axes % each % #item % #ticks % #tick % numTicks') (Just 2) & set (#axes % each % #item % #ticks % #textTick %? #style % #size) 0.07) 0.08 100

-- | Parse charting options.
parsePerfChartOptions :: PerfChartOptions -> Parser PerfChartOptions
parsePerfChartOptions def =
  (\c fp trunAt small big hist avs -> PerfChartOptions c fp trunAt small big hist avs (view #averagesStyle def) (view #averagesPaletteStart def) (view #averagesLegend def) (view #smallStyle def) (view #smallHud def) (view #bigStyle def) (view #bigHud def) (view #titleSize def) (view #histGrain def))
  <$> switch (long "chart" <> short 'c' <> help "chart the result")
    <*> option str (value (view #chartFilepath def) <> long "chartpath" <> help "chart file name")
    <*> option auto (value (view #truncateAt def) <> long "truncateat" <> help "truncate chart data (multiple of median)")
    <*> switch (long "small")
    <*> switch (long "big")
    <*> switch (long "histogram")
    <*> switch (long "averages")

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

    (Range x' z') = Range zero (fromIntegral $ length xs)
    rectx = BlankChart defaultStyle [Rect x' z' zero zero]
    averagesCT = named "averages" $ zipWith (\x i -> GlyphChart (view #averagesStyle cfg & set #color (palette i) & set #borderColor (palette i) & set #glyphShape (gpalette i)) [Point zero x]) [av,med,best] [(view #averagesPaletteStart cfg)..]

    smallChart = dotHistChart (view #histGrain cfg) (view #smallStyle cfg) (mempty @ChartOptions & set #chartTree (averagesCT <> named "xrange" [rectx]) & set #hudOptions (view #smallHud cfg)) xsSmall

    minb = minimum (_y <$> xsBig)
    bigrange = Rect x' z' (bool minb zero (length xsBig == 1)) minb
    bigChart = dotHistChart (view #histGrain cfg) (view #bigStyle cfg) (mempty @ChartOptions & set #hudOptions (view #bigHud cfg) & set #chartTree (named "xrange" [BlankChart defaultStyle [bigrange]])) xsBig

    -- FIXME: get from style box
    bigCT = set styleBox' (Just (Rect (-2.01) 0.14 (-0.2) 0.2)) bigChart

    finalChart = mempty @ChartOptions & set #chartTree (vert NoAlign 0.1 [smallChart, bool bigCT mempty (null xsBig)]) & set (#hudOptions % #legends) [Priority 10 (view #averagesLegend cfg &  set #legendCharts (zipWith (\t' c -> (t', [c])) labels (toListOf chart' averagesCT))) ] & set (#hudOptions % #titles) [Priority 5 (defaultTitleOptions t & set (#style % #size) (view #titleSize cfg))]

dotHistChart :: Int -> Style -> ChartOptions -> [Point Double] -> ChartTree
dotHistChart grain gstyle co xs = view #chartTree dotHistCO
  where
    dotCT = named "dot" [GlyphChart gstyle xs]
    ys = fmap _y xs
    (Range l u) = fromMaybe one (space1 ys)
    r' = bool (Range l u) (Range 0 l) (l==u)
    r = computeRangeTick r' (fromMaybe defaultTick (co & preview (#hudOptions % #axes % ix 1 % #item % #ticks % #tick)))
    (y,w) = let (Range y' w') = r in bool (y',w') (y'-0.5, y'+0.5) (y' == w')

    histCO = hhistChart r grain ys & set (#markupOptions % #chartAspect) (CanvasAspect 0.3) & over #chartTree (<> unnamed [BlankChart defaultStyle [Rect 0 0 y w]])
    dotCO = co & over #chartTree (dotCT<>)
    dotHistCO = horiCO NoAlign 0 [histCO, dotCO]

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Perf.Chart where

import Chart
import Control.Category ((>>>))
import Data.Bifunctor
import Data.Bool
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics
import Optics.Core
import Options.Applicative
import Perf.Stats as Perf
import Prettychart

--     m <- fromDump defaultPerfDumpOptions
data PerfChartOptions
  = PerfChartOptions
  { doChart :: Bool,
    chartFilepath :: FilePath,
    truncateAt :: Double,
    doSmallChart :: Bool,
    doBigChart :: Bool,
    doHistChart :: Bool,
    doAveragesLegend :: Bool,
    averagesStyle :: Style,
    averagesPaletteStart :: Int,
    averagesLegend :: LegendOptions,
    smallStyle :: Style,
    smallHud :: HudOptions,
    bigStyle :: Style,
    bigHud :: HudOptions,
    titleSize :: Double,
    histGrain :: Int,
    bigWidth :: Double,
    excludeZeros :: Bool
  }
  deriving (Eq, Show, Generic)

defaultPerfChartOptions :: PerfChartOptions
defaultPerfChartOptions = PerfChartOptions False "other/perf.svg" 10 True True True True (defaultGlyphStyle & set #size 0.05) 2 (defaultLegendOptions & set #place PlaceBottom & set #numStacks 3 & set #scaleChartsBy 0.2 & set #legendSize 0.3 & set #alignCharts AlignLeft & set #hgap (-0.2) & set #vgap (-0.1)) (defaultGlyphStyle & set #size 0.01 & over #color (rgb (palette 0)) & set (#color % opac') 0.3 & set (#borderColor % opac') 0.3 & set #glyphShape (gpalette 0)) defaultHudOptions (defaultGlyphStyle & set #size 0.06 & over #color (rgb (palette 0)) & set #glyphShape (gpalette 0) & set (#color % opac') 0.3 & set (#borderColor % opac') 1) (defaultHudOptions & set (#axes % each % #item % #ticks % #textTick %? #style % #size) 0.07 & over #axes (drop 1) & set (#axes % each % #item % #ticks % #tick % numTicks') (Just 2)) 0.08 100 0.2 True

-- | Parse charting options.
parsePerfChartOptions :: PerfChartOptions -> Parser PerfChartOptions
parsePerfChartOptions def =
  (\c fp trunAt small big hist avs -> PerfChartOptions c fp trunAt small big hist avs (view #averagesStyle def) (view #averagesPaletteStart def) (view #averagesLegend def) (view #smallStyle def) (view #smallHud def) (view #bigStyle def) (view #bigHud def) (view #titleSize def) (view #histGrain def) (view #bigWidth def) (view #excludeZeros def))
    <$> switch (long "chart" <> short 'c' <> help "chart the result")
    <*> option str (value (view #chartFilepath def) <> showDefault <> long "chartpath" <> metavar "FILE" <> help "chart file name")
    <*> option auto (value (view #truncateAt def) <> showDefaultWith show <> long "truncateat" <> help "truncate chart data (multiple of median)")
    <*> switch (long "small")
    <*> switch (long "big")
    <*> switch (long "histogram")
    <*> switch (long "averages")

perfCharts :: PerfChartOptions -> Maybe [Text] -> Map.Map Text [[Double]] -> ChartOptions
perfCharts cfg labels m = case cs of
  [c] -> c
  _ -> stackCO stackn AlignLeft NoAlign 0.1 cs
  where
    stackn = length cs & fromIntegral & sqrt @Double & ceiling
    cs = uncurry (perfChart cfg) <$> ps'
    ps = mconcat $ fmap (uncurry zip . bimap (\t -> fmap ((t <> ": ") <>) (fromMaybe (Text.pack . show @Int <$> [0 ..]) labels)) List.transpose) (Map.toList m)
    ps' = filter ((> 0) . sum . snd) ps

perfChart :: PerfChartOptions -> Text -> [Double] -> ChartOptions
perfChart cfg t xs = finalChart
  where
    xsSmall = xs & xify & filter (_y >>> (< upperCutoff)) & filter (\x -> view #excludeZeros cfg && (_y x > 0))
    xsBig = xs & xify & filter (_y >>> (>= upperCutoff))
    med = median xs
    best = tenth xs
    av = Perf.average xs
    upperCutoff = view #truncateAt cfg * med

    labels =
      [ "average: " <> comma (Just 3) av,
        "median: " <> comma (Just 3) med,
        "best: " <> comma (Just 3) best
      ]
    (Rect _ _ y' w') = fromMaybe one $ space1 xsSmall
    (Range x' z') = Range zero (fromIntegral $ length xs)
    rectx = BlankChart defaultStyle [Rect x' z' y' w']
    averagesCT = named "averages" $ zipWith (\x i -> GlyphChart (view #averagesStyle cfg & set #color (palette i) & set #borderColor (palette i) & set #glyphShape (gpalette i)) [Point zero x]) [av, med, best] [(view #averagesPaletteStart cfg) ..]

    (smallDot, smallHist) = dotHistChart (view #histGrain cfg) (view #smallStyle cfg) (mempty @ChartOptions & set #chartTree (averagesCT <> named "xrange" [rectx]) & set #hudOptions (view #smallHud cfg)) xsSmall

    minb = minimum (_y <$> xsBig)
    bigrange = Rect x' z' (bool minb zero (length xsBig == 1)) minb
    (bigDot, bigHist) = dotHistChart (view #histGrain cfg) (view #bigStyle cfg) (mempty @ChartOptions & set #hudOptions (view #bigHud cfg) & set #chartTree (named "xrange" [BlankChart defaultStyle [bigrange]])) xsBig

    (Rect bdX bdW _ _) = fromMaybe one $ view styleBox' (asChartTree bigDot)
    bdr = Just $ Rect bdX bdW (-(view #bigWidth cfg)) (view #bigWidth cfg)
    (Rect bdhX bdhW _ _) = fromMaybe one $ view styleBox' (asChartTree bigHist)
    bhr = Just $ Rect bdhX bdhW (-(view #bigWidth cfg)) (view #bigWidth cfg)

    finalChart =
      mempty @ChartOptions
        & set
          #chartTree
          ( stack
              2
              NoAlign
              NoAlign
              0
              ( bool (asChartTree bigDot & set styleBox' bdr & pure) mempty (null xsBig)
                  <> bool (asChartTree bigHist & set styleBox' bhr & pure) mempty (null xsBig)
                  <> [ asChartTree smallDot,
                       asChartTree smallHist
                     ]
              )
          )
        & set
          (#hudOptions % #legends)
          [Priority 10 (view #averagesLegend cfg & set #legendCharts (zipWith (\t' c -> (t', [c])) labels (toListOf chart' averagesCT)))]
        & set
          (#hudOptions % #titles)
          [Priority 5 (defaultTitleOptions t & set (#style % #size) (view #titleSize cfg))]

dotHistChart :: Int -> Style -> ChartOptions -> [Point Double] -> (ChartOptions, ChartOptions)
dotHistChart grain gstyle co xs = (dotCO, histCO)
  where
    dotCT = named "dot" [GlyphChart gstyle xs]
    ys = fmap _y xs
    (Range l u) = fromMaybe one (space1 ys)
    r' = bool (Range l u) (Range 0 l) (l == u)
    r = computeRangeTick r' (fromMaybe defaultTick (co & preview (#hudOptions % #axes % ix 1 % #item % #ticks % #tick)))
    (y, w) = let (Range y' w') = r in bool (y', w') (y' - 0.5, y' + 0.5) (y' == w')

    histCO = hhistChart r grain ys & set (#markupOptions % #chartAspect) (CanvasAspect 0.3) & over #chartTree (<> unnamed [BlankChart defaultStyle [Rect 0 0 y w]])
    dotCO = co & over #chartTree (dotCT <>)

compareCharts :: [(PerfChartOptions, Text, [Double])] -> ChartOptions
compareCharts xs = finalChart
  where
    xs' = xs & fmap (\(_, _, x) -> x)
    cfg' = xs & fmap (\(x, _, _) -> x)
    t' = xs & fmap (\(_, x, _) -> x)
    cfg = case cfg' of
      (c : _) -> c
      [] -> defaultPerfChartOptions
    xsSmall = xs' & fmap (xify >>> filter (_y >>> (< upperCutoff)) >>> filter (\x -> view #excludeZeros cfg && (_y x > 0)))
    xsBig = xs' & fmap (xify >>> filter (_y >>> (>= upperCutoff)))
    med = median <$> xs'
    upperCutoff = view #truncateAt cfg * maximum med

    (Rect _ _ y' w') = fromMaybe one $ space1 $ mconcat xsSmall
    (Range x' z') = Range zero (fromIntegral $ maximum (length <$> xs'))
    rectx = BlankChart defaultStyle [Rect x' z' y' w']

    (smallDot, smallHist) = dotHistCharts (view #histGrain cfg) (mempty @ChartOptions & set #hudOptions (view #smallHud cfg) & set #chartTree (unnamed [rectx])) (zip (view #smallStyle <$> cfg') xsSmall)

    minb = minimum (_y <$> mconcat xsBig)
    bigrange = Rect x' z' (bool minb zero (length (mconcat xsBig) == 1)) minb
    (bigDot, bigHist) = dotHistCharts (view #histGrain cfg) (mempty @ChartOptions & set #hudOptions (view #bigHud cfg) & set #chartTree (named "xrange" [BlankChart defaultStyle [bigrange]])) (zip (view #bigStyle <$> cfg') xsBig)

    (Rect bdX bdW _ _) = fromMaybe one $ view styleBox' (asChartTree bigDot)
    bdr = Just $ Rect bdX bdW (-(view #bigWidth cfg)) (view #bigWidth cfg)
    (Rect bdhX bdhW _ _) = fromMaybe one $ view styleBox' (asChartTree bigHist)
    bhr = Just $ Rect bdhX bdhW (-(view #bigWidth cfg)) (view #bigWidth cfg)

    finalChart =
      mempty @ChartOptions
        & set
          #chartTree
          ( stack
              2
              NoAlign
              NoAlign
              0
              ( bool (asChartTree bigDot & set styleBox' bdr & pure) mempty (null xsBig)
                  <> bool (asChartTree bigHist & set styleBox' bhr & pure) mempty (null xsBig)
                  <> [ asChartTree smallDot,
                       asChartTree smallHist
                     ]
              )
          )
        & set
          (#hudOptions % #legends)
          [Priority 10 (view #averagesLegend cfg & set #legendCharts (zipWith (\t'' c -> (t'', [c])) t' (toListOf (#chartTree % chart') smallDot)))]

dotHistCharts :: Int -> ChartOptions -> [(Style, [Point Double])] -> (ChartOptions, ChartOptions)
dotHistCharts grain co xs = (dotCO, histCO)
  where
    dotCTs = named "dot" (uncurry GlyphChart <$> xs)
    ys = fmap _y . snd <$> xs
    (Range l u) = fromMaybe one (space1 (mconcat ys))
    r' = bool (Range l u) (Range 0 l) (l == u)
    r = computeRangeTick r' (fromMaybe defaultTick (co & preview (#hudOptions % #axes % ix 1 % #item % #ticks % #tick)))
    (y, w) = let (Range y' w') = r in bool (y', w') (y' - 0.5, y' + 0.5) (y' == w')

    histCO = hhistCharts r grain (zip (fst <$> xs) ys) & set (#markupOptions % #chartAspect) (CanvasAspect 0.3) & over #chartTree (<> unnamed [BlankChart defaultStyle [Rect 0 0 y w]])
    dotCO = co & over #chartTree (dotCTs <>)

module Visualization
  ( Plot(..)
  , scatterDataset
  , regressionLine
  , mseCurve
  , renderASCII
  ) where

import qualified Data.Vector as V
import Scratch.LinearRegression

------------------------------------------------------------
-- Plot types
------------------------------------------------------------

data Plot
  = Scatter [(Double, Double)]
  | Line    [(Double, Double)]
  deriving (Show)

------------------------------------------------------------
-- Dataset → Scatter plot
------------------------------------------------------------

scatterDataset :: Dataset -> Plot
scatterDataset dataset =
  Scatter
    [ (x V.! 0, y)
    | (x, y) <- V.toList dataset
    ]

------------------------------------------------------------
-- Model → Regression line
------------------------------------------------------------

regressionLine :: Model
  -> Double   -- xmin
  -> Double   -- xmax
  -> Int      -- number of points
  -> Plot
regressionLine model xmin xmax n =
  let step = (xmax - xmin) / fromIntegral (n - 1)
      pts =
        [ let x = xmin + step * fromIntegral i
          in (x, predict model (V.singleton x))
        | i <- [0 .. n-1]
        ]
  in Line pts

------------------------------------------------------------
-- MSE → Curve
------------------------------------------------------------

mseCurve :: MSE -> Plot
mseCurve (MSE es) =
  Scatter $ zip [1..] es

------------------------------------------------------------
-- ASCII renderer
------------------------------------------------------------

renderASCII :: Int -> Int -> [Plot] -> String
renderASCII width height plots =

  let points = concatMap extract plots

      xs = map (\(x,_,_) -> x) points
      ys = map (\(_,y,_) -> y) points

      xmin = minimum xs
      xmax = maximum xs 
      ymin = minimum ys
      ymax = maximum ys

      scaleX x =
        round $ (x - xmin) / (xmax - xmin) * fromIntegral (width - 1)

      scaleY y =
        round $ (y - ymin) / (ymax - ymin) * fromIntegral (height - 1)

      coords =
        [ (scaleX x, scaleY y, c)
        | (x, y, c) <- points
        ]

      emptyGrid =
        replicate height (replicate width ' ')

      gridWithPoints =
        placePoints emptyGrid coords
  in unlines (reverse gridWithPoints)

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

extract :: Plot -> [(Double, Double, Char)]
extract (Scatter ps) = addChar '*' ps
extract (Line ps)    = addChar '-' ps

addChar :: Char -> [(Double, Double)] -> [(Double, Double, Char)]
addChar c = map (\(x,y) -> (x,y,c))

placePoints :: [[Char]] -> [(Int, Int, Char)] -> [[Char]]
placePoints grid points =
  [ [ case lookupChar x y of 
        Just ch -> ch 
        Nothing -> c 
      | (x, c) <- zip [0..] row ]
  | (y, row) <- zip [0..] grid 
  ]
  where
    lookupChar x y =
      case [ch | (px,py,ch) <- points, px == x, py == y] of
        (ch:_) -> Just ch 
        []     -> Nothing
  
data Bounds = Bounds
  { xmin :: Double
  , xmax :: Double
  , ymin :: Double
  , ymax :: Double
  }

boundsFromPlots :: [Plot] -> Bounds
boundsFromPlots plots =
  let pts = concatMap plotPoints plots
      xs  = map fst pts
      ys  = map snd pts
  in Bounds
      (minimum xs)
      (maximum xs)
      (minimum ys)
      (maximum ys)

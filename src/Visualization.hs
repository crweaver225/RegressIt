module Visualization
  ( Plot(..)
  , renderASCII
  , scatterPoints
  , linePoints
  , msePoints
  ) where

import qualified Data.Vector as V
import Scratch.LinearRegression

-- Simple plot types
data Plot
  = Scatter [(Double, Double)]
  | Line [(Double, Double)]
  deriving (Show)

-- Convert your dataset (V.Vector of Vectors) to scatter points (1D only)
scatterPoints :: V.Vector (V.Vector Double, Double) -> [(Double, Double)]
scatterPoints dataset =
  [ (x V.! 0, y) | (x, y) <- V.toList dataset ]

-- Convert your model to a line for plotting
linePoints :: Model -> Double -> Double -> Int -> [(Double, Double)]
linePoints (Model w b) xmin xmax n =
  let step = (xmax - xmin) / fromIntegral (n-1)
  in [ (x, b + (w V.! 0) * x) | i <- [0..n-1], let x = xmin + step * fromIntegral i ]

-- Convert MSE history to points
msePoints :: MSE -> [(Double, Double)]
msePoints (HistoryMSE errs) = zip [1..] errs
msePoints (ScalarMSE e)     = [(1, e)]

-- Render multiple plots as ASCII
renderASCII :: Int -> Int -> [Plot] -> String
renderASCII width height plots =
  let points = concatMap extract plots
      xs = map fst points
      ys = map snd points
      xmin = minimum xs
      xmax = maximum xs
      ymin = minimum ys
      ymax = maximum ys

      scaleX x = round $ (x - xmin) / (xmax - xmin) * fromIntegral (width - 1)
      scaleY y = round $ (y - ymin) / (ymax - ymin) * fromIntegral (height - 1)

      -- Initialize empty grid
      grid = replicate height (replicate width ' ')

      -- Place points on grid
      grid' = placePoints grid (map (\(x,y) -> (scaleX x, scaleY y)) points)
  in unlines $ reverse grid'  -- Reverse so row 0 is at the bottom

-- Flatten Plot to points
extract :: Plot -> [(Double, Double)]
extract (Scatter ps) = ps
extract (Line ps)    = ps

-- Place points onto a character grid
placePoints :: [[Char]] -> [(Int, Int)] -> [[Char]]
placePoints grid points =
  [ [ if (x, y') `elem` points then '*' else c
    | (x, c) <- zip [0..] row ]
  | (y', row) <- zip [0..] grid ]

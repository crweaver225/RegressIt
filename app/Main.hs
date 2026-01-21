import qualified Data.Vector              as V
import qualified Numeric.LinearAlgebra    as N
import qualified Scratch.LinearRegression as SLR
import qualified HMatrix.LinearRegression as HLR
import qualified Visualization            as Viz

main :: IO ()
main = do
  let dataset = V.fromList
        [ (V.fromList [2,4], 3)
        , (V.fromList [5,10], 6)
        , (V.fromList [4,8], 6)
        ]
      (model, mse) = SLR.trainLinear dataset 10 0.01

  mapM_ (\(i, e) -> putStrLn $ show i ++ ": " ++ show e)
      (zip [1 :: Int ..] (SLR.err mse))
  
  print model
  print $ SLR.predict model (V.fromList [3,9])

  -- Create ASCII plot for dataset + regression line
  let scatterPts = Viz.scatterPoints dataset
      linePts    = Viz.linePoints model (minimum (map fst scatterPts))
                                         (maximum (map fst scatterPts))
                                         50
      asciiPlot  = Viz.renderASCII 60 20 [Scatter scatterPts, Line linePts]

  putStrLn asciiPlot

  -- let dataset =
  --       [ (N.fromList [2,4], 3)
  --       , (N.fromList [5,10], 6)
  --       , (N.fromList [4,8], 6)
  --       ]

  --     (model, mseVal) = HLR.trainLinear dataset

  -- putStrLn $ "MSE: " ++ show mseVal
  -- print model
  -- print $ HLR.predict model (N.fromList [3,9])

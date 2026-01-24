import qualified Data.Vector              as V
import qualified Numeric.LinearAlgebra    as N
import qualified Scratch.LinearRegression as SLR
import qualified HMatrix.LinearRegression as HLR
import qualified Visualization            as Viz

generateHardcodedDataset :: SLR.Dataset
generateHardcodedDataset = V.fromList
         [ (V.fromList [2,4], 3)
         , (V.fromList [5,10], 6)
         , (V.fromList [4,8], 6)
         ]

generateLinearDataset :: Int -> SLR.Dataset
generateLinearDataset n =
  V.fromList
    [ let x1 = 2 * fromIntegral i
          y  = 0.5 * x1 + 0.25 * x1
      in (V.fromList [x1], y)
    | i <- [1..n]
    ]

main :: IO ()
main = do

    let dataset = generateLinearDataset 10
        (model, mse) = SLR.trainLinear dataset 25 0.001

    mapM_ (\(i, e) -> putStrLn $ show i ++ ": " ++ show e)
        (zip [1 :: Int ..] (SLR.err mse))

    print model

    -- Create ASCII plot for dataset + regression line
    putStrLn "\nRegression plot:"
    let scatter = Viz.scatterDataset dataset
        line    = Viz.regressionLine model 0 20 50
    putStrLn $ Viz.renderASCII 50 10 [scatter, line]

    putStrLn "\nMSE curve:"
    putStrLn $ Viz.renderASCII 60 15 [Viz.mseCurve mse]

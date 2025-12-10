import qualified Data.Vector as V
import Scratch.LinearRegression

main :: IO ()
main = do
  let dataset = V.fromList
        [ (V.fromList [2,4], 3)
        , (V.fromList [5,10], 6)
        , (V.fromList [4,8], 6)
        ]
      (model, mse) = trainLinear dataset 10 0.01

  mapM_ (\(i, e) -> putStrLn $ show i ++ ": " ++ show e)
      (zip [1 :: Int ..] (err mse))
  
  print model
  print $ predict model (V.fromList [3,9])

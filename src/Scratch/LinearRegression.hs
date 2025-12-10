{-# LANGUAGE BangPatterns #-}

module Scratch.LinearRegression
  ( Dataset
  , Model
  , trainLinear
  , predict
  , MSE (..)
  ) where

import qualified Data.Vector as V 
import Data.List (foldl')

-- [(inputs),target]
type Dataset = V.Vector (V.Vector Double, Double)

data Model = Model
  { weights :: V.Vector Double
  , bias    :: Double
  } deriving (Show)

data MSE = MSE 
  { err :: [Double] 
  } deriving (Show)

dot :: V.Vector Double -> V.Vector Double -> Double
dot v1 v2 = V.sum $ V.zipWith (*) v1 v2

scale :: Double -> V.Vector Double -> V.Vector Double 
scale s = V.map (* s)

vAdd :: V.Vector Double -> V.Vector Double -> V.Vector Double 
vAdd = V.zipWith (+)

vSubtract :: V.Vector Double -> V.Vector Double -> V.Vector Double 
vSubtract = V.zipWith (-)

predict :: Model -> V.Vector Double -> Double
predict (Model w b) x = dot w x + b

stepMSE :: [Double] -> Double -> MSE -> MSE
stepMSE e s (MSE es) =
    let !computedMSE = foldl' (\acc x -> acc + x*x) 0 e / s
        !es' = computedMSE : es
    in MSE es'

reverseMSE :: MSE -> MSE 
reverseMSE mse = MSE (reverse $ err mse)

--------------------------------------------------------------------
-- BACKPROPOGATION:
-- J(w,b) = 1/2m ∑ ​(yi​ − y^​i​)^2
-- m = number of training examples
-- yi = true label
-- y^​i = w⋅xi + b = predicted value
-- w = vector of weights
-- b = bias
-- We divide by 2m for convenience (derivatives simplify nicely).
--------------------------------------------------------------------

-- Train using gradient descent
trainLinear :: Dataset -> Int -> Double -> (Model, MSE)
trainLinear dataset epochs lr =
  let nFeatures = V.length . fst $ dataset V.! 0
      initialModel = Model (V.replicate nFeatures 0) 0 -- Initialize weights to 0
      mseInitial = MSE []
  in regress epochs initialModel mseInitial
  where
    m = fromIntegral $ V.length dataset

    regress :: Int -> Model -> MSE -> (Model, MSE)
    regress 0 model mse = (model, reverseMSE mse)
    regress t (Model w b) mse =
      let (dwA, dbA, errsA) = V.foldl' accumulate (V.replicate (V.length w) 0, 0, []) dataset
      
          accumulate :: (V.Vector Double, Double, [Double])
                     -> (V.Vector Double, Double)
                     -> (V.Vector Double, Double, [Double])
          accumulate (dwAcc', dbAcc', errsAcc') (x, y) =
            let yPred   = dot w x + b
                !err'   = yPred - y
                !dwNew  = vAdd dwAcc' (scale err' x)
                !dbNew  = dbAcc' + err'
                !errsNew = err' : errsAcc'
            in (dwNew, dbNew, errsNew)

          -- Averaging the gradients
          dwAvg = scale (1/m) dwA
          dbAvg = dbA / m

          -- Update the model
          w' = vSubtract w (scale lr dwAvg)
          b' = b - lr * dbAvg

          -- Compute MSE for this epoch
          mse' = stepMSE errsA m mse

      in regress (t-1) (Model w' b') mse'

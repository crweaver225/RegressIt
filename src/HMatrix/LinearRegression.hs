module HMatrix.LinearRegression 
  ( trainLinear
  , predict
  , mse
  , Model(..)
  ) where 

import qualified Numeric.LinearAlgebra as N

type Dataset = [(N.Vector Double, Double)]

data Model = Model
  { weights :: N.Vector Double
  , bias    :: Double
  } deriving (Show)

-- Convert dataset to matrix form
toMatrix :: Dataset -> (N.Matrix Double, N.Vector Double)
toMatrix dataset = 
    let xs = map fst dataset 
        ys = map snd dataset 
    in (N.fromRows xs, N.fromList ys)

-- Add bias column
addBias :: N.Matrix Double -> N.Matrix Double 
addBias x = 
    let ones = N.konst 1 (N.rows x, 1)
    in ones N.||| x 

-- Train using closed-form OLS
trainLinear :: Dataset -> (Model, Double)
trainLinear dataset =
  let (x0, y) = toMatrix dataset
      x       = addBias x0
      beta    = N.pinv x N.#> y
      model   = toModel beta
  in (model, mse dataset model)
  
-- Prediction
predict :: Model -> N.Vector Double -> Double
predict (Model w b) x =
  b + (w N.<.> x)

-- Mean squared error
mse :: Dataset -> Model -> Double
mse dataset model =
  let errors =
        [ let yHat = predict model x
          in (yHat - y) ^ 2
        | (x, y) <- dataset
        ]
  in sum errors / fromIntegral (length errors)

-- Convert beta vector into Model
toModel :: N.Vector Double -> Model
toModel beta =
  Model
    { bias    = N.atIndex beta 0
    , weights = N.subVector 1 (N.size beta - 1) beta
    }

<img width="609" height="444" alt="Screenshot 2026-01-24 at 2 01 21 PM" src="https://github.com/user-attachments/assets/e27fc9be-c0c4-418c-a602-f72c02eaca56" />

##Linear Regression in Haskell
Train a linear regression model to fit a datatset. Uses gradient descent.
```Haskell
(model, mse) = SLR.trainLinear dataset 25 0.001
```
What is returned when a model is fit to the training data is the model itself and information about the mean squared error tracked during training. Pass in how many times you want the model to iterate through the data and its learning rate. 

You can plot the model error during training using the visualizatsion tool
```Haskell
putStrLn "\nMSE curve:"
putStrLn $ Viz.renderASCII 60 15 [Viz.mseCurve mse]
```
You can also plot a regression line to the dataset
```Haskell
let scatter = Viz.scatterDataset dataset
    line    = Viz.regressionLine model 0 20 50
putStrLn $ Viz.renderASCII 50 10 [scatter, line]
```

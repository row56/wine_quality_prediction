# mltwo-project
Regression task on quality of red wine based on the physiochemical properties of the wine.

## Project report on overleaf:
Link to edit: https://www.overleaf.com/2127442822sbxtwttkycdr#5c3dcc 

## Red wine quality dataset:
### Source:

- Kaggle:  
    https://www.kaggle.com/datasets/uciml/red-wine-quality-cortez-et-al-2009 

- UCI machine learning repository: 

    https://archive.ics.uci.edu/dataset/186/wine+quality 

### Synopsis: 
The dataset contains physiochemical properties of red wine alongside with a quality score 
(target variable) based on sensory data. The dataset scores are not distributed equally, since 
there a lot more “normal” quality wines than “poor” or “excellent” quality wines. 

### Number of observations: 1599

### Variables:

| Variable Name         | Description       | Data type  |
| :---:                 | :---:             | :---:      |
| Fixed acidity         |                   | Continuous |
| Volatile acidity      |                   | Continuous |
| Citric acid           |                   | Continuous |
| Residual sugar        |                   | Continuous |
| Chlorides             |                   | Continuous |
| Free sulfur dioxide   |                   | Continuous |
| Total sulfur dioxide  |                   | Continuous |
| Density               |                   | Continuous |
| pH                    |                   | Continuous |
| Sulphates             |                   | Continuous |
| Alcohol               |                   | Continuous |
| Quality               | Target variable   | Ordinal    |

## ML Methods for the task:
- Non-linear models (Spline smoothing) 
- Pursuit projection regression 

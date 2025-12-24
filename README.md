## Comparative Machine Learning Models for ICO Success Prediction

## Project Overview 
This project applies supervised machine learning methods to predict the success of Initial Coin Offering (ICO) campaigns based on project-level characteristics

## Requirements
- R (version 4.0 or later)
- The following R packages:
  - dplyr
  - visdat
  - ggplot2
  - patchwork
  - VIM
  - stringr
  - missForest
  - caret
  - corrgram
  - randomForest
  - class
  - gmodels
  - naivebayes
  - C50
  - kernlab
  - tidyverse
  - pROC
  - irr
  
## Dataset
The dataset 'Data_202425.csv' contains information on over 6,000 ICO projects, including financial, temporal, organisational, and compliance-related attributes.

## Target variable
'success' is binary indicator of ICO outcome (success / failure)

## Methodology
- Explored variable distributions and missingness; removed observations and features with excessive missing values
- Engineered features from temporal variables and project attributes; encoded categorical predictors
- Imputed missing values using missForest and applied dummy encoding
- Assessed feature importance with Random Forests and excluded low-impact predictors
- Trained and compared k-NN, Naive Bayes, C5.0 decision trees (base and boosted), and RBF-SVM, with cross-validated tuning where applicable

## Evaluation and Results
- Models were evaluated using an 80/20 train–test split and accuracy, sensitivity, specificity, and ROC–AUC
- Boosted C5.0 achieved the best overall performance
- Key predictors included expert ratings, ICO timing, team size, and token distribution characteristics

## License
This project is released under the MIT License. See `LICENSE`

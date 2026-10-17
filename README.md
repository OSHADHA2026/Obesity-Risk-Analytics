# Obesity Risk Analytics Pipeline

## Overview

This project explores obesity-related data using descriptive statistics, statistical inference, and machine learning. It evaluates different classification models to predict binary obesity status and the specific obesity category.

The project is implemented in R and organized in a Jupyter Notebook.

## Objectives

- Explore and clean the obesity dataset.
- Perform descriptive and statistical analyses.
- Build and compare machine learning models.
- Tune model hyperparameters using cross-validation.
- Select an optimal probability threshold for binary classification.
- Evaluate models using accuracy, precision, recall, F1-score, and confusion matrices.
- Apply Principal Component Analysis (PCA) and Naive Bayes analysis.

## Models

- Binary Logistic Regression
- Multinomial Logistic Regression
- Ridge Regression
- LASSO Regression
- Decision Tree
- Random Forest
- Naive Bayes

## Hyperparameter Tuning

Cross-validation is used to select model settings. The binary Random Forest extension searches candidate `mtry` values and selects a probability threshold using out-of-fold training predictions.

The selected threshold is evaluated on a separate test set. The F1 score is used as the threshold-selection criterion in this implementation.

## Project Structure

- `Obesity_Risk_Analytics_Pipeline.ipynb` — main analysis notebook
- `scripts/` — R analysis scripts
- `outputs/` — selected model results and plots

## Requirements

- R
- Jupyter Notebook with an R kernel
- Required R packages: `nnet`, `glmnet`, `rpart`, `randomForest`, `e1071`, and `caret`

## How to Run

1. Clone or download this repository.
2. Place the required dataset in the expected `data/` directory.
3. Install the required R packages.
4. Open the notebook in Jupyter with an R kernel.
5. Run the notebook cells in order.

The notebook expects the cleaned dataset at `data/obesity_clean.rds` and the original dataset at `data/obesity_level.csv`. Follow the setup steps to generate the cleaned file before running downstream analyses.

## Evaluation

Model performance is evaluated using a held-out test set. The optimal threshold for binary classification is selected from cross-validation predictions rather than from the test labels.

Results may vary if the dataset, preprocessing, random seed, or model settings change.

## Limitations

This project demonstrates predictive analysis, not clinical diagnosis. Model performance depends on the dataset and its representativeness. Predictions should not be treated as medical advice or proof of causal relationships.

## Dataset

Provide the original dataset source and its applicable license or terms of use here.

## License

Add a license only after deciding how the project code and included materials may be reused.

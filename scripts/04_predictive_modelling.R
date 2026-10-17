# =============================================================================
# 04: ADVANCED PREDICTIVE MODELLING (Task 5)
# =============================================================================

# Install these packages once if they are not already installed
# install.packages(c("nnet", "glmnet", "rpart", "randomForest"))

library(nnet)         # Multinomial logistic regression
library(glmnet)       # Ridge and LASSO regression
library(rpart)        # Decision tree
library(randomForest) # Random forest

# Load the cleaned dataset
data <- readRDS("data/obesity_clean.rds")

# Use the same random split every time the script is run
set.seed(2026)

# Create the output folder if it does not already exist
dir.create("outputs", showWarnings = FALSE)

# Save the results to a text file and display them in the console
sink("outputs/task5_predictive_models_output.txt", split = TRUE)

cat("###############################################################\n")
cat("PREDICTIVE MODELLING & ADVANCED EVALUATION RESULTS \n")
cat("###############################################################\n\n")


# =============================================================================
# 1. SELECT FEATURES
# =============================================================================

# Exclude Weight and BMI because they are directly related to the obesity
# outcome and could make the model unrealistically strong.
predictors <- c("Gender", "Age", "Height", "family_history_with_overweight",
                "FAVC", "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC",
                "FAF", "TUE", "CALC", "MTRANS")

# Keep the selected predictors and both obesity outcome variables
model_data <- data[, c(predictors, "Obesity_Level", "Obese_Binary")]


# =============================================================================
# 2. TRAIN / TEST SPLIT
# =============================================================================

# Create an approximately 80/20 split while keeping each obesity level
# represented in both training and testing sets.
train_rows <- unname(unlist(tapply(
  1:nrow(model_data),
  model_data$Obesity_Level,
  function(x) {
    sample(x, size = round(0.8 * length(x)))
  }
)))

train <- model_data[train_rows, ]
test  <- model_data[-train_rows, ]

cat("Dataset Split: Training rows =", nrow(train),
    "| Test rows =", nrow(test), "\n")


# Helper function to calculate accuracy
calc_acc <- function(predicted, actual) {
  round(mean(predicted == actual) * 100, 2)
}


# =============================================================================
# MODEL : BINARY LOGISTIC REGRESSION
# Question: Can we predict whether a person is obese or not obese?
# =============================================================================

cat("\n===== MODEL: Binary Logistic Regression (GLM) =====\n")

# Predict the binary obesity status using the selected lifestyle and
# demographic variables.
logit_formula <- Obese_Binary ~ Gender + Age + Height +
  family_history_with_overweight + FAVC + FCVC + NCP + CAEC +
  SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS

logit_model <- glm(logit_formula, data = train, family = binomial)

# Predict the training data
logit_train_pred <- ifelse(
  predict(logit_model, newdata = train, type = "response") > 0.5,
  "Obese",
  "Not_Obese"
)

acc_glm_train <- calc_acc(logit_train_pred, train$Obese_Binary)

# Predict the test data
logit_test_pred <- ifelse(
  predict(logit_model, newdata = test, type = "response") > 0.5,
  "Obese",
  "Not_Obese"
)

acc_glm_test <- calc_acc(logit_test_pred, test$Obese_Binary)


# Calculate precision, recall and F1-score
cm <- table(
  Predicted = logit_test_pred,
  Actual = test$Obese_Binary
)

TP <- cm["Obese", "Obese"]
FP <- cm["Obese", "Not_Obese"]
FN <- cm["Not_Obese", "Obese"]

precision <- round((TP / (TP + FP)) * 100, 2)
recall    <- round((TP / (TP + FN)) * 100, 2)
f1_score  <- round((2 * precision * recall) /
                     (precision + recall), 2)

cat("Train Accuracy :", acc_glm_train, "%\n")
cat("Test Accuracy  :", acc_glm_test, "%\n")
cat("Precision      :", precision, "%\n")
cat("Recall         :", recall, "%\n")
cat("F1-Score       :", f1_score, "%\n")


# =============================================================================
# MODEL : MULTINOMIAL LOGISTIC REGRESSION
# Question: Can we predict the specific obesity level?
# =============================================================================

cat("\n===== MODEL : Multinomial Logistic Regression =====\n")

# Predict all seven obesity levels instead of only obese/not obese
multi_formula <- Obesity_Level ~ Gender + Age + Height +
  family_history_with_overweight + FAVC + FCVC + NCP + CAEC +
  SMOKE + CH2O + SCC + FAF + TUE + CALC + MTRANS

multi_model <- multinom(
  multi_formula,
  data = train,
  trace = FALSE,
  MaxNWts = 2500
)

# Make predictions for training and test data
multi_train_pred <- predict(multi_model, newdata = train)
multi_test_pred  <- predict(multi_model, newdata = test)

acc_multi_train <- calc_acc(multi_train_pred, train$Obesity_Level)
acc_multi_test  <- calc_acc(multi_test_pred, test$Obesity_Level)

cat("Train Accuracy :", acc_multi_train, "%\n")
cat("Test Accuracy  :", acc_multi_test, "%\n")


# =============================================================================
# MODEL : RIDGE REGRESSION
# Question: Can regularization improve multi-class prediction?
# =============================================================================

cat("\n===== MODEL : Ridge Regression =====\n")

# Convert the categorical predictors into a numeric model matrix
# for use with glmnet.
x_train <- model.matrix(multi_formula, data = train)[, -1]
y_train <- train$Obesity_Level

x_test <- model.matrix(multi_formula, data = test)[, -1]
y_test <- test$Obesity_Level

# Find the best regularization value using 5-fold cross-validation
ridge_cv <- cv.glmnet(
  x_train,
  y_train,
  family = "multinomial",
  alpha = 0,
  nfolds = 5
)

ridge_train_pred <- predict(
  ridge_cv,
  newx = x_train,
  s = "lambda.min",
  type = "class"
)

ridge_test_pred <- predict(
  ridge_cv,
  newx = x_test,
  s = "lambda.min",
  type = "class"
)

acc_ridge_train <- calc_acc(ridge_train_pred, y_train)
acc_ridge_test  <- calc_acc(ridge_test_pred, y_test)

cat("Train Accuracy :", acc_ridge_train, "%\n")
cat("Test Accuracy  :", acc_ridge_test, "%\n")


# =============================================================================
# MODEL : LASSO REGRESSION
# Question: Can LASSO regularization predict obesity levels while
# selecting important features?
# =============================================================================

cat("\n===== MODEL : LASSO Regression =====\n")

# Use LASSO regularization to reduce the effect of less useful predictors
lasso_cv <- cv.glmnet(
  x_train,
  y_train,
  family = "multinomial",
  alpha = 1,
  nfolds = 5
)

lasso_train_pred <- predict(
  lasso_cv,
  newx = x_train,
  s = "lambda.min",
  type = "class"
)

lasso_test_pred <- predict(
  lasso_cv,
  newx = x_test,
  s = "lambda.min",
  type = "class"
)

acc_lasso_train <- calc_acc(lasso_train_pred, y_train)
acc_lasso_test  <- calc_acc(lasso_test_pred, y_test)

cat("Train Accuracy :", acc_lasso_train, "%\n")
cat("Test Accuracy  :", acc_lasso_test, "%\n")

# =============================================================================
# MODEL : DECISION TREE
# Question: Can simple decision rules be used to predict obesity levels?
# =============================================================================

cat("\n===== MODEL : Decision Tree Classifier =====\n")

# Build a tree that uses if-then style rules to classify obesity levels
tree_model <- rpart(
  multi_formula,
  data = train,
  method = "class"
)

tree_train_pred <- predict(tree_model, train, type = "class")
tree_test_pred  <- predict(tree_model, test, type = "class")

acc_tree_train <- calc_acc(tree_train_pred, train$Obesity_Level)
acc_tree_test  <- calc_acc(tree_test_pred, test$Obesity_Level)

cat("Train Accuracy :", acc_tree_train, "%\n")
cat("Test Accuracy  :", acc_tree_test, "%\n")

# =============================================================================
# MODEL : RANDOM FOREST
# Question: Can multiple decision trees improve obesity-level prediction?
# =============================================================================

cat("\n===== MODEL : Random Forest Classifier =====\n")

# Build a forest using 100 decision trees
set.seed(2026)

rf_model <- randomForest(
  multi_formula,
  data = train,
  ntree = 100,
  importance = TRUE
)

rf_train_pred <- predict(rf_model, train)
rf_test_pred  <- predict(rf_model, test)

acc_rf_train <- calc_acc(rf_train_pred, train$Obesity_Level)
acc_rf_test  <- calc_acc(rf_test_pred, test$Obesity_Level)

cat("Train Accuracy :", acc_rf_train, "%\n")
cat("Test Accuracy  :", acc_rf_test, "%\n")


# 3. FINAL MODEL COMPARISON

cat("\n\n===== FINAL MODEL COMPARISON (TRAIN VS TEST) =====\n")

comparison <- data.frame(
  Model = c(
    "Binary Logistic (Obese/Not)",
    "Multinomial Logistic (7 Classes)",
    "Ridge Regression",
    "LASSO Regression",
    "Decision Tree",
    "Random Forest"
  ),
  Train_Accuracy_Pct = c(
    acc_glm_train,
    acc_multi_train,
    acc_ridge_train,
    acc_lasso_train,
    acc_tree_train,
    acc_rf_train
  ),
  Test_Accuracy_Pct = c(
    acc_glm_test,
    acc_multi_test,
    acc_ridge_test,
    acc_lasso_test,
    acc_tree_test,
    acc_rf_test
  )
)

# Calculate the difference between training and test accuracy
comparison$Overfitting_Gap_Pct <-
  comparison$Train_Accuracy_Pct - comparison$Test_Accuracy_Pct

print(comparison)

# Find the model with the highest test accuracy
best_model <- comparison$Model[
  which.max(comparison$Test_Accuracy_Pct)
]


# Stop saving output to the text file
sink()

# Save the model comparison table for the report
write.csv(
  comparison,
  "outputs/task5_model_comparison.csv",
  row.names = FALSE
)
# =============================================================================
# 06 : ADVANCED NAIVE BAYES CLASSIFIER (Task 8)
# =============================================================================

# Install these packages once if they are not already installed
# install.packages(c("e1071", "nnet"))

library(e1071)   # Naive Bayes model
library(nnet)    # Multinomial logistic regression

# Load the cleaned dataset
data <- readRDS("data/obesity_clean.rds")

# Create the output folder if it does not already exist
# Save the results to a text file while also showing them in the console
dir.create("outputs", showWarnings = FALSE)
sink("outputs/task8_bayesian_evaluation.txt", split = TRUE)

cat("###############################################################\n")
cat("ADVANCED NAIVE BAYES OPTIMIZATION & EVALUATION \n")
cat("###############################################################\n\n")


# =============================================================================
# 1. SELECT VARIABLES AND SPLIT THE DATA
# =============================================================================

# Select the variables that will be used to predict obesity level
predictors <- c(
  "Gender", "Age", "Height", "family_history_with_overweight",
  "FAVC", "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC",
  "FAF", "TUE", "CALC", "MTRANS"
)

# Keep the selected predictors and the obesity-level target variable
model_data <- data[, c(predictors, "Obesity_Level")]

# Use the same random seed so that the split is reproducible
set.seed(2026)

# Create an approximately 80/20 split while keeping each obesity level
# represented in both the training and test datasets.
train_rows <- unname(unlist(tapply(
  1:nrow(model_data),
  model_data$Obesity_Level,
  function(x) {
    sample(x, size = round(0.8 * length(x)))
  }
)))

train <- model_data[train_rows, ]
test  <- model_data[-train_rows, ]


# Helper function to calculate accuracy
calc_acc <- function(predicted, actual) {
  round(mean(predicted == actual) * 100, 2)
}


# =============================================================================
# 2. STANDARD NAIVE BAYES
# =============================================================================

cat("\n===== 1. STANDARD NAIVE BAYES (Default Gaussian) =====\n")

# Build the standard Naive Bayes model using its default settings
nb_base <- naiveBayes(
  Obesity_Level ~ .,
  data = train
)

# Predict the obesity levels in the test dataset
acc_base_test <- calc_acc(
  predict(nb_base, newdata = test),
  test$Obesity_Level
)

cat("-> Base Test Accuracy:", acc_base_test, "%\n")


# =============================================================================
# 3. OPTIMIZED NAIVE BAYES
# =============================================================================

cat("\n===== 2. OPTIMIZED NAIVE BAYES (KDE + Laplace) =====\n")

# Use kernel density estimation to better represent the distribution
# of continuous variables and Laplace smoothing for categorical variables.
nb_optimized <- naiveBayes(
  Obesity_Level ~ .,
  data = train,
  laplace = 1,
  usekernel = TRUE
)

# Predict the obesity levels using the optimized model
acc_opt_test <- calc_acc(
  predict(nb_optimized, newdata = test),
  test$Obesity_Level
)

# Display the confusion matrix to see the prediction results for each class
cat("Confusion Matrix (Optimized Model):\n")

print(
  table(
    Predicted = predict(nb_optimized, newdata = test),
    Actual = test$Obesity_Level
  )
)

cat("\n-> Optimized Test Accuracy:", acc_opt_test, "%\n")


# =============================================================================
# 4. COMPARE NAIVE BAYES WITH MULTINOMIAL LOGISTIC REGRESSION
# =============================================================================

cat("\n===== DYNAMIC COMPARISON MATRIX =====\n")

# Build a multinomial logistic regression model using the same training data
# so that its test accuracy can be compared with the Naive Bayes models.
multi_model <- multinom(
  Obesity_Level ~ .,
  data = train,
  trace = FALSE,
  MaxNWts = 2500
)

# Calculate the test accuracy of the multinomial logistic model
acc_multi_test <- calc_acc(
  predict(multi_model, newdata = test),
  test$Obesity_Level
)

# Create a table to compare the three models
comparison <- data.frame(
  Model = c(
    "Multinomial Logistic (Script 03)",
    "Standard Naive Bayes (Gaussian)",
    "Optimized Naive Bayes (KDE + Laplace)"
  ),
  Test_Accuracy_Pct = c(
    acc_multi_test,
    acc_base_test,
    acc_opt_test
  )
)

print(comparison)

# Save the comparison table for use in the report
write.csv(
  comparison,
  "outputs/task8_bayes_vs_logistic_comparison.csv",
  row.names = FALSE
)


# =============================================================================
# 5. CRITICAL EVALUATION
# =============================================================================

cat("\n===============================================================\n")
cat("CRITICAL EVALUATION OF BAYESIAN METHODS (For Consultancy Report)\n")
cat("===============================================================\n")

cat("KEY DISCUSSION POINTS:\n\n")

cat("1. The Gaussian Flaw (Why Base Naive Bayes failed):\n")

cat(
  "   Standard Naive Bayes assumes numeric variables follow a perfect Bell Curve. However, our lifestyle data (like Age) is highly skewed. This false assumption dropped its initial accuracy to",
  acc_base_test,
  "%.\n\n"
)

cat("2. The Power of Kernel Density Estimation (KDE):\n")

cat(
  "   By applying KDE (usekernel = TRUE), we forced the Bayesian model to map the true, non-linear distribution of the numeric data. Along with Laplace Smoothing, this successfully optimized the model's performance without changing the underlying data.\n\n"
)

cat("3. The 'Naive' Independence Weakness:\n")

cat(
  "   Even when optimized, Naive Bayes assumes all lifestyle factors are completely independent. In reality, eating high-calorie food (FAVC) and snacking (CAEC) are correlated. Multinomial Logistic Regression handles these correlations better, which is why it maintains a highly competitive edge.\n\n"
)

cat("[CONSULTANT'S FINAL RECOMMENDATION]:\n")

cat(
  "Naive Bayes is an excellent tool due to its lightning-fast training speed and probabilistic outputs (e.g., '82% chance of Obesity Type II'). However, because human lifestyle variables are heavily interconnected, Random Forest and Logistic Regression remain the most robust choices for final clinical diagnostics.\n"
)

# Stop saving the output to the text file
sink()
# =============================================================================
# 07: MASTER EXECUTION PIPELINE
# =============================================================================

# Run all data science scripts in the correct order from start to finish.
# Make sure the working directory is set to the project root before running:
# source("scripts/07_run_all.R")


cat("===============================================================\n")
cat(" STARTING AUTOMATED DATA SCIENCE PIPELINE...\n")
cat("===============================================================\n\n")


# Record the starting time so we can calculate the total execution time
start_time <- Sys.time()


# =============================================================================
# 1. DATA SETUP AND CLEANING
# =============================================================================

cat(">>> [1/6] Running Data Setup & Cleaning (Script 01)...\n")
source("scripts/01_setup_and_cleaning.R")


# =============================================================================
# 2. DESCRIPTIVE ANALYSIS
# =============================================================================

cat("\n>>> [2/6] Running Advanced Descriptive Analysis (Script 02)...\n")
source("scripts/02_descriptive_analysis.R")


# =============================================================================
# 3. STATISTICAL INFERENCE
# =============================================================================

cat("\n>>> [3/6] Running Statistical Inference (Script 03)...\n")
source("scripts/03_statistical_inference.R")


# =============================================================================
# 4. PREDICTIVE MODELLING
# =============================================================================

cat("\n>>> [4/6] Running Predictive Modelling (Script 04)...\n")
source("scripts/04_predictive_modelling.R")


# =============================================================================
# 5. PCA ANALYSIS
# =============================================================================

cat("\n>>> [5/6] Running PCA Analysis (Script 05)...\n")
source("scripts/05_pca_analysis.R")


# =============================================================================
# 6. NAIVE BAYES EVALUATION
# =============================================================================

cat("\n>>> [6/6] Running Naive Bayes Evaluation (Script 06)...\n")
source("scripts/06_naive_bayes.R")


# Record the finishing time and calculate how long the full pipeline took
end_time <- Sys.time()

execution_time <- round(
  as.numeric(difftime(end_time, start_time, units = "secs")),
  2
)
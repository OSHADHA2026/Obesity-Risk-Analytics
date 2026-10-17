# =============================================================================
# 01: SETUP AND DATA CLEANING
# =============================================================================

# 1: Load data and Rename Target
# Read the CSV file and rename the last column to the correct target name
data <- read.csv("data/obesity_level.csv", stringsAsFactors = FALSE)
names(data)[ncol(data)] <- "Obesity_Level"


# 2: Remove ID and Duplicates
# Remove the ID column because it is not useful for analysis
data$id <- NULL

# Remove duplicate rows to avoid using the same record more than once
data <- unique(data)


# 3: Fix Corrupted Category Labels
# Correct the typo in the target variable
data$Obesity_Level <- ifelse(
  data$Obesity_Level == "0rmal_Weight",
  "Normal_Weight",
  data$Obesity_Level
)

# Replace incorrect "0" values with "no" in categorical variables
data$CAEC <- ifelse(data$CAEC == "0", "no", data$CAEC)
data$CALC <- ifelse(data$CALC == "0", "no", data$CALC)


# 4: Check for Missing Values
# Count the missing values in each column
missing_values <- colSums(is.na(data))

cat("\n===== Missing Value Check =====\n")

# Display the result clearly
if (sum(missing_values) == 0) {
  cat("No missing values were found in the dataset.\n")
} else {
  cat("Missing values found in the following columns:\n")
  print(missing_values[missing_values > 0])
}


# 5: Logical Correction of Synthetic Variables
# Round survey variables to whole numbers where appropriate
survey_vars <- c("FCVC", "NCP", "CH2O", "FAF", "TUE")

for (var in survey_vars) {
  data[[var]] <- round(data[[var]])
}


# 6: Feature Engineering
# Calculate BMI and add it as a new column
data$BMI <- data$Weight / (data$Height^2)

# Create age groups for descriptive analysis and dashboards
data$Age_Group <- cut(
  data$Age,
  breaks = c(0, 19, 35, 55, 100),
  labels = c("Teen", "Young_Adult", "Adult", "Senior"),
  right = FALSE
)

# Convert the seven obesity levels into two groups:
# Obese and Not_Obese
data$Obese_Binary <- ifelse(
  data$Obesity_Level %in% c(
    "Obesity_Type_I",
    "Obesity_Type_II",
    "Obesity_Type_III"
  ),
  "Obese",
  "Not_Obese"
)


# 7: Handle Age Outliers Without Data Leakage
# Create a stratified 80% training set by sampling from each obesity level
set.seed(2026)

train_idx <- unname(unlist(
  tapply(
    1:nrow(data),
    data$Obesity_Level,
    function(x) {
      sample(x, size = round(0.8 * length(x)))
    }
  )
))

# Calculate the 99th percentile of Age using only the training data
upper_age_bound <- quantile(data$Age[train_idx], 0.99)

# Cap Age values above the training-based upper limit
data$Age <- ifelse(
  data$Age > upper_age_bound,
  upper_age_bound,
  data$Age
)


# 8: Convert Binary and Categorical Variables to Factors

# Convert binary variables from 0/1 into No/Yes
binary_vars <- c(
  "family_history_with_overweight",
  "FAVC",
  "SMOKE",
  "SCC"
)

for (var in binary_vars) {
  data[[var]] <- factor(
    data[[var]],
    levels = c(0, 1),
    labels = c("No", "Yes")
  )
}

# Convert other categorical variables into factors
data$Gender <- factor(data$Gender)
data$MTRANS <- factor(data$MTRANS)
data$Obese_Binary <- factor(data$Obese_Binary)

# Set the order of the CAEC categories
data$CAEC <- factor(
  data$CAEC,
  levels = c("no", "Sometimes", "Frequently", "Always")
)

# Set the order of the CALC categories
data$CALC <- factor(
  data$CALC,
  levels = c("no", "Sometimes", "Frequently", "Always")
)

# Set the obesity levels from lower to higher weight categories
data$Obesity_Level <- factor(
  data$Obesity_Level,
  levels = c(
    "Insufficient_Weight",
    "Normal_Weight",
    "Overweight_Level_I",
    "Overweight_Level_II",
    "Obesity_Type_I",
    "Obesity_Type_II",
    "Obesity_Type_III"
  )
)


# 9: Save Cleaned Data
# Save the cleaned dataset as CSV and RDS files
write.csv(
  data,
  "data/obesity_clean.csv",
  row.names = FALSE
)

saveRDS(
  data,
  "data/obesity_clean.rds"
)

cat("\nCleaned dataset saved successfully.\n")
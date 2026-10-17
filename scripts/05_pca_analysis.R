# =============================================================================
# 05 : ADVANCED PRINCIPAL COMPONENT ANALYSIS (Task 7)
# =============================================================================

# Create the output folders if they do not already exist
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

# Load the cleaned dataset
data <- readRDS("data/obesity_clean.rds")

# Save the results to a text file while also showing them in the console
sink("outputs/task7_pca_summary.txt", split = TRUE)

cat("###############################################################\n")
cat("PRINCIPAL COMPONENT ANALYSIS (PCA) & EVALUATION\n")
cat("###############################################################\n\n")


# =============================================================================
# 1. SELECT VARIABLES FOR PCA
# =============================================================================

# Exclude Weight and BMI so that the PCA focuses on the independent
# lifestyle and demographic variables rather than variables directly
# related to the obesity outcome.
pca_vars <- c("Age", "Height", "FCVC", "NCP", "CH2O", "FAF", "TUE")

# Keep only the selected variables for the PCA
pca_data <- data[, pca_vars]


# =============================================================================
# 2. CHECK CORRELATION BEFORE PCA
# =============================================================================

# Check how strongly the variables are correlated with each other.
# PCA is more useful when several variables contain overlapping information.
cat("[INFO] Checking average correlation among variables...\n")

cor_matrix <- cor(pca_data)

# Calculate the average absolute correlation between the variables
avg_cor <- mean(abs(cor_matrix[upper.tri(cor_matrix)]))

cat("Average absolute correlation:", round(avg_cor, 3), "\n")

cat("(Note: A low average correlation suggests PCA might struggle to reduce dimensions efficiently).\n\n")


# =============================================================================
# 3. RUN PCA
# =============================================================================

# Scale the variables so that variables with larger numerical ranges
# do not have more influence on the PCA than smaller-scale variables.
pca_result <- prcomp(
  pca_data,
  scale. = TRUE,
  center = TRUE
)


# =============================================================================
# 4. CALCULATE VARIANCE EXPLAINED
# =============================================================================

# Calculate the proportion of total variance explained by each component
var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)

# Calculate the cumulative variance explained by the components
cum_var <- cumsum(var_explained)

cat("Variance explained by each Principal Component (PC):\n")
print(round(var_explained, 3))

cat("\nCumulative variance explained:\n")
print(round(cum_var, 3))

# Find the number of components required to explain at least 80% of the variance
n_needed <- which(cum_var >= 0.80)[1]


# =============================================================================
# 5. CREATE THE SCREE PLOT
# =============================================================================

png(
  "outputs/plots/10_pca_scree_plot.png",
  width = 700,
  height = 500
)

# Show the percentage of variance explained by each principal component
barplot(
  var_explained * 100,
  names.arg = paste0("PC", 1:length(var_explained)),
  main = "Scree Plot - Variance Explained by Each Component",
  ylab = "% of Variance Explained",
  col = "steelblue",
  ylim = c(0, 100)
)

# Add the cumulative variance explained to the plot
lines(
  x = 1:length(cum_var) - 0.5 + 0.5,
  y = cum_var * 100,
  type = "b",
  col = "red",
  pch = 16,
  lwd = 2
)

# Mark the 80% cumulative variance level
abline(
  h = 80,
  col = "darkgreen",
  lty = 2,
  lwd = 2
)

text(
  x = 1,
  y = 84,
  labels = "80% Threshold",
  col = "darkgreen",
  font = 2
)

# Add a legend to explain the two parts of the plot
legend(
  "topleft",
  legend = c("Individual Variance", "Cumulative Variance"),
  fill = c("steelblue", NA),
  col = c(NA, "red"),
  lty = c(NA, 1),
  pch = c(NA, 16),
  bty = "n"
)

dev.off()

cat("\n Scree Plot (saved to outputs/plots/10_pca_scree_plot.png)\n")


# =============================================================================
# 6. CREATE THE PCA BIPLOT
# =============================================================================

png(
  "outputs/plots/11_pca_biplot.png",
  width = 800,
  height = 700
)

# Hide the individual observations so that the variable arrows
# are easier to see in the biplot.
biplot(
  pca_result,
  main = "PCA Biplot - Lifestyle Variables",
  cex = 0.8,
  xlabs = rep(".", nrow(pca_data)),
  col = c("gray", "red")
)

dev.off()

cat("PCA Biplot\n")


# =============================================================================
# 7. DISPLAY VARIABLE LOADINGS
# =============================================================================

cat("\n Variable loadings for PC1 and PC2 (How original variables form the PCs):\n")

# Display the contribution of each original variable to PC1 and PC2
print(round(pca_result$rotation[, 1:2], 3))


# =============================================================================
# 8. CRITICAL EVALUATION
# =============================================================================

cat("\n===============================================================\n")
cat("CRITICAL EVALUATION: Is PCA actually useful for this dataset?\n")
cat("===============================================================\n")

cat(
  "Components needed to reach 80% variance:",
  n_needed,
  "out of",
  length(pca_vars),
  "original variables.\n\n"
)

cat("KEY FINDINGS FOR THE CONSULTANCY REPORT:\n")

cat(
  "1. No Dominant Component: PC1 explains only",
  round(var_explained[1] * 100, 1),
  "% of the total variance.\n"
)

cat(
  "2. Poor Compression: We need",
  n_needed,
  "out of",
  length(pca_vars),
  "components to hit the 80% threshold. This means the lifestyle variables carry mostly independent, distinct information rather than overlapping.\n"
)

cat(
  "3. Loss of Interpretability: If we use PCA, we replace understandable features like 'Age' or 'Water intake' with complex mathematical components (PC1, PC2). This makes it impossible to give actionable advice to a client (e.g., 'reduce PC1' makes no business sense).\n"
)


# =============================================================================
# 9. RECOMMENDATION
# =============================================================================

cat("\n[CONSULTANT'S RECOMMENDATION]:\n")

cat(
  "Do NOT use PCA for the predictive models.\n"
)

cat(
  "Instead, rely on the LASSO Regression model (which we already built). LASSO automatically selects important variables while keeping their original, interpretable meanings intact.\n"
)

# Stop saving the output to the text file
sink()
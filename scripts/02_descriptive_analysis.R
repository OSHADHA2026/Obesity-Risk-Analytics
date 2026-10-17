# =============================================================================
# 02: ADVANCED DESCRIPTIVE ANALYSIS
# =============================================================================

# 1: Setup and Directory Creation
# Create output folders automatically so the script works on a new PC
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)

# Load the cleaned dataset
data <- readRDS("data/obesity_clean.rds")

cat("Data loaded successfully. Creating plots...\n")


# =============================================================================
# 2: Target Variable Distribution
# =============================================================================

# Show the distribution of the seven original obesity levels
png(
  "outputs/plots/01_target_distribution.png",
  width = 800,
  height = 600
)

# Increase the bottom margin so the long class names are not cut off
par(mar = c(11, 4, 4, 2) + 0.1)

counts <- table(data$Obesity_Level)
percentages <- round(prop.table(counts) * 100, 1)

# Add percentages to the class names
labels_with_pct <- paste0(
  names(counts),
  "\n(",
  percentages,
  "%)"
)

barplot(
  counts,
  main = "Distribution of Obesity Level (Target Variable)",
  col = "steelblue",
  las = 2,
  cex.names = 0.8,
  names.arg = labels_with_pct
)

# Reset the plot margins
par(mar = c(5, 4, 4, 2) + 0.1)

dev.off()

cat("Plot 1 generated: Target Distribution.\n")

# =============================================================================
# 2.1: Binary Obesity Class Distribution
# =============================================================================

# Count the number of Obese and Not Obese records
binary_counts <- table(data$Obese_Binary)

# Calculate the percentage of each group
binary_percentages <- round(
  prop.table(binary_counts) * 100,
  2
)

# Create a table with counts and percentages
binary_distribution <- data.frame(
  Category = names(binary_counts),
  Count = as.vector(binary_counts),
  Percentage = as.vector(binary_percentages)
)

# Display the binary class distribution in the console
cat("\n===== Binary Obesity Class Distribution =====\n")
print(binary_distribution)

# Save the distribution table for the report
write.csv(
  binary_distribution,
  "outputs/binary_obesity_distribution.csv",
  row.names = FALSE
)


# Create the binary obesity distribution plot
# The plot is displayed in RStudio and also saved as a PNG file
barplot(
  binary_counts,
  main = "Obese vs Not Obese Distribution",
  xlab = "Obesity Category",
  ylab = "Number of Records",
  col = c("lightblue", "lightcoral"),
  ylim = c(0, max(binary_counts) * 1.15)
)

# Add the percentage above each bar
text(
  x = seq_along(binary_counts),
  y = binary_counts,
  labels = paste0(binary_percentages, "%"),
  pos = 3
)

# Save the current plot as a PNG file
dev.copy(
  png,
  filename = "outputs/plots/01_binary_obesity_distribution.png",
  width = 800,
  height = 600
)

dev.off()

cat("Binary obesity distribution plot generated successfully.\n")


# =============================================================================
# 3: Histograms of Numeric Variables
# =============================================================================

# Analyze the shape and spread of continuous variables
num_vars <- c(
  "Age",
  "Height",
  "Weight",
  "BMI",
  "FCVC",
  "NCP",
  "CH2O",
  "FAF",
  "TUE"
)

png(
  "outputs/plots/02_numeric_histograms.png",
  width = 1000,
  height = 800
)

# Set up a 3x3 grid for the nine numeric variables
par(mfrow = c(3, 3))

for (v in num_vars) {
  hist(
    data[[v]],
    main = paste("Distribution of", v),
    xlab = v,
    col = "lightblue",
    border = "white",
    breaks = 20
  )
}

# Reset the plotting layout
dev.off()
par(mfrow = c(1, 1))

cat("Plot 2 generated: Numeric Histograms.\n")


# =============================================================================
# 4: Validating Data through Boxplots
# =============================================================================

# Check whether BMI aligns with the obesity level categories
png(
  "outputs/plots/03_boxplot_bmi_by_class.png",
  width = 900,
  height = 600
)

# Increase the bottom margin for the long class names
par(mar = c(12, 4, 4, 2) + 0.1)

boxplot(
  BMI ~ Obesity_Level,
  data = data,
  col = "lightgreen",
  main = "BMI Validation by Obesity Level",
  xlab = "",
  ylab = "BMI",
  las = 2,
  cex.axis = 0.8
)

# Reset the margins
par(mar = c(5, 4, 4, 2) + 0.1)

dev.off()


# Print outlier summary to the console
cat("\nOutliers Summary (1.5 * IQR rule):\n")

for (v in c("Weight", "BMI")) {
  
  q <- quantile(data[[v]], c(0.25, 0.75))
  iqr <- IQR(data[[v]])
  
  n_out <- sum(
    data[[v]] < (q[1] - 1.5 * iqr) |
      data[[v]] > (q[2] + 1.5 * iqr)
  )
  
  cat(
    "  ->",
    v,
    "Extreme Values:",
    n_out,
    "(Kept as they represent Obesity Type III)\n"
  )
}


# =============================================================================
# 5: Business Insight Visualizations
# =============================================================================

# Compare Age Group against obesity rates
png(
  "outputs/plots/04_obesity_by_age_group.png",
  width = 700,
  height = 500
)

tbl_age <- table(
  data$Obese_Binary,
  data$Age_Group
)

barplot(
  prop.table(tbl_age, margin = 2),
  main = "Obesity Rate by Age Group",
  col = c("lightgray", "salmon"),
  xlab = "Age Group",
  ylab = "Proportion",
  xlim = c(0, 6)
)

legend(
  "topright",
  legend = rownames(tbl_age),
  fill = c("lightgray", "salmon"),
  bty = "n"
)

dev.off()


# Compare Family History against obesity rates
png(
  "outputs/plots/05_obesity_by_family_history.png",
  width = 700,
  height = 500
)

tbl_fam <- table(
  data$Obese_Binary,
  data$family_history_with_overweight
)

barplot(
  prop.table(tbl_fam, margin = 2),
  main = "Obesity Risk based on Family History",
  col = c("lightgray", "salmon"),
  xlab = "Family History of Overweight",
  xlim = c(0, 4)
)

legend(
  "topright",
  legend = rownames(tbl_fam),
  fill = c("lightgray", "salmon"),
  bty = "n"
)

dev.off()

cat("\nPlots 4 & 5 generated: Business Insights (Age Group & Family History).\n")


# =============================================================================
# 5.1: Advanced Business Insights
# =============================================================================

cat("[INFO] Generating additional strategic business plots...\n")


# Plot 7: Impact of Snacking on BMI
png(
  "outputs/plots/07_bmi_by_snacking_caec.png",
  width = 800,
  height = 500
)

boxplot(
  BMI ~ CAEC,
  data = data,
  col = c("white", "lightyellow", "orange", "red"),
  main = "Impact of Snacking (Eating Between Meals) on BMI",
  xlab = "Frequency of Snacking (CAEC)",
  ylab = "BMI",
  cex.axis = 1.2
)

dev.off()


# Plot 8: Calorie Monitoring vs Obesity Rate
png(
  "outputs/plots/08_obesity_by_calorie_monitoring.png",
  width = 700,
  height = 500
)

tbl_scc <- table(
  data$Obese_Binary,
  data$SCC
)

barplot(
  prop.table(tbl_scc, margin = 2),
  main = "Obesity Risk by Calorie Monitoring (SCC)",
  col = c("lightgray", "salmon"),
  xlab = "Monitors Calories?",
  ylab = "Proportion",
  xlim = c(0, 4)
)

legend(
  "topright",
  legend = rownames(tbl_scc),
  fill = c("lightgray", "salmon"),
  bty = "n"
)

dev.off()


# Plot 9: Obesity Level Distribution by Gender
png(
  "outputs/plots/09_obesity_by_gender.png",
  width = 900,
  height = 600
)

# Increase the bottom margin for long class names
par(mar = c(12, 4, 4, 2) + 0.1)

tbl_gender <- table(
  data$Gender,
  data$Obesity_Level
)

barplot(
  prop.table(tbl_gender, margin = 2),
  main = "Gender Distribution Across Obesity Levels",
  col = c("pink", "lightblue"),
  las = 2,
  ylab = "Proportion",
  xlim = c(0, 10)
)

legend(
  "topright",
  legend = rownames(tbl_gender),
  fill = c("pink", "lightblue"),
  bty = "n",
  title = "Gender"
)

# Reset the margins
par(mar = c(5, 4, 4, 2) + 0.1)

dev.off()

cat("Plots 7, 8 & 9 generated: Advanced Business Insights.\n")


# =============================================================================
# 6: Correlation Matrix
# =============================================================================

# Identify highly correlated numeric variables
cor_matrix <- cor(data[, num_vars])

png(
  "outputs/plots/06_correlation_matrix.png",
  width = 800,
  height = 800
)

# Draw the correlation heatmap
image(
  1:ncol(cor_matrix),
  1:ncol(cor_matrix),
  cor_matrix,
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "Correlation Matrix (Values > 0.7 indicate high multicollinearity)",
  col = colorRampPalette(c("blue", "white", "red"))(50),
  zlim = c(-1, 1)
)

# Add axis labels
axis(
  1,
  at = 1:ncol(cor_matrix),
  labels = colnames(cor_matrix),
  las = 2,
  cex.axis = 1.1
)

axis(
  2,
  at = 1:ncol(cor_matrix),
  labels = colnames(cor_matrix),
  las = 2,
  cex.axis = 1.1
)

# Add the correlation values inside the boxes
for (x in 1:ncol(cor_matrix)) {
  for (y in 1:ncol(cor_matrix)) {
    
    val <- round(cor_matrix[x, y], 2)
    
    # Use white text when the background is dark
    text_color <- ifelse(
      abs(val) > 0.6,
      "white",
      "black"
    )
    
    text(
      x,
      y,
      labels = val,
      col = text_color,
      cex = 1.2,
      font = 2
    )
  }
}

dev.off()

cat("Plot 6 generated: Correlation Matrix.\n")
cat("Weight and BMI show high multicollinearity. Avoid using both together in linear regression.\n")


# =============================================================================
# 7: Descriptive Statistics Summary Table
# =============================================================================

# Create a summary table for the numeric variables
desc_table <- data.frame(
  Variable = num_vars,
  Mean = round(
    sapply(num_vars, function(v) mean(data[[v]])),
    2
  ),
  Median = round(
    sapply(num_vars, function(v) median(data[[v]])),
    2
  ),
  SD = round(
    sapply(num_vars, function(v) sd(data[[v]])),
    2
  ),
  Min = round(
    sapply(num_vars, function(v) min(data[[v]])),
    2
  ),
  Max = round(
    sapply(num_vars, function(v) max(data[[v]])),
    2
  )
)

# Save the descriptive statistics for the final report
write.csv(
  desc_table,
  "outputs/descriptive_statistics_table.csv",
  row.names = FALSE
)

cat("Descriptive statistics table saved successfully.\n")
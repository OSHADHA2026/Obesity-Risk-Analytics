# =============================================================================
# 03: ADVANCED STATISTICAL INFERENCE (Task 4)
# =============================================================================

# Create the output folder and load the cleaned dataset
dir.create("outputs", showWarnings = FALSE)
data <- readRDS("data/obesity_clean.rds")

# Save the test results to a text file while also showing them in the console
sink("outputs/task4_statistical_tests_output.txt", split = TRUE)


# =============================================================================
# TEST 1: Independent-Samples t-Test
# Question: Is there a difference in BMI between males and females?
# =============================================================================

cat("\n===== TEST 1: Two-Sample t-test (BMI by Gender) =====\n")

# H0: The mean BMI is the same for males and females
# H1: The mean BMI is different between males and females

# Check whether the BMI variances are similar between the two groups
var_check_bmi <- var.test(BMI ~ Gender, data = data)
print(var_check_bmi)

# Run Welch's t-test, which is suitable even when variances are unequal
t_result <- t.test(BMI ~ Gender, data = data)
print(t_result)

cat("\nTest 1 Result\n")
cat("P-value =", format.pval(t_result$p.value, digits = 3), "\n")

cat("Conclusion: Since p-value < 0.05, there is a statistically significant difference in BMI between genders.\n")
cat("Business Strategy: Health insurance premiums or diet plans should be personalized based on gender risk profiles.\n")


# =============================================================================
# TEST 2: Chi-Square Test of Independence
# Question: Is high-calorie food consumption associated with obesity status?
# =============================================================================

cat("\n\n===== TEST 2: Chi-square Test (Obesity Risk vs High Caloric Food) =====\n")

# H0: Obesity status and high-calorie food consumption are independent
# H1: There is an association between obesity status and high-calorie food consumption

# Create a contingency table for FAVC and obesity status
tbl_favc <- table(data$FAVC, data$Obese_Binary)

# Display the percentage of each obesity category within each FAVC group
cat("Contingency Table (Row Percentages):\n")
print(round(prop.table(tbl_favc, margin = 1) * 100, 1))

# Perform the chi-square test
chi_result <- chisq.test(tbl_favc, correct = FALSE)
print(chi_result)

cat("\nTest 2 Result\n")
cat("P-value =", format.pval(chi_result$p.value, digits = 3), "\n")

cat("Conclusion: Eating high-caloric food is strongly associated with obesity.\n")
cat("Business Strategy: Predictive models must include 'FAVC' as a primary feature. Campaigns should focus on fast-food reduction.\n")


# =============================================================================
# TEST 3: F-Test for Equality of Variances
# Question: Does physical activity vary differently between smokers and
# non-smokers?
# =============================================================================

cat("\n\n===== TEST 3: F-test for Equal Variances (FAF by SMOKE) =====\n")

# H0: Physical activity has the same variance in both groups
# H1: Physical activity has different variances between the two groups

# Compare the variance of FAF between smokers and non-smokers
var_result <- var.test(FAF ~ SMOKE, data = data)
print(var_result)

cat("\nTest 3 Result\n")
cat("P-value =", format.pval(var_result$p.value, digits = 3), "\n")

cat("Conclusion: The spread (variance) of physical activity is different between smokers and non-smokers.\n")
cat("Business Strategy: Fitness programs for smokers need a highly structured, rigid schedule, whereas non-smokers have a more predictable natural variance in activity.\n")

# =============================================================================
# TEST 4: One-Way ANOVA
# Question: Does average age differ across the obesity levels?
# =============================================================================

cat("\n\n===== TEST 4: One-way ANOVA (Age across Obesity Levels) =====\n")

# H0: The mean age is the same across all obesity levels
# H1: At least one obesity level has a different mean age

# Check whether the group variances are similar
bartlett_result <- bartlett.test(Age ~ Obesity_Level, data = data)

cat("Bartlett's Test:\n")
print(bartlett_result)

# Compare the mean age across the seven obesity levels
anova_age <- aov(Age ~ Obesity_Level, data = data)

cat("\nANOVA Summary:\n")
print(summary(anova_age))


# Use Tukey's test to identify which groups differ from each other
cat("\nTukey HSD Post-Hoc Test:\n")

tukey_result <- TukeyHSD(anova_age)

# Convert the results into a data frame and sort by p-value
tukey_df <- as.data.frame(tukey_result$Obesity_Level)
tukey_df <- tukey_df[order(tukey_df[, 4]), ]

# Display the 10 comparisons with the smallest p-values
print(head(tukey_df, 10))

cat("\nBusiness Strategy: Post-hoc results show the biggest age gap is between 'Obesity Type II' and 'Normal Weight'. Marketing for extreme obesity interventions should strictly target older age demographics.\n")

# Stop saving output to the text file
sink()
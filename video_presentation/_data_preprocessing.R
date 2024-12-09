# Data Cleaning & Wrangling

library(naniar)
library(tidyverse)
library(caret)
library(plotly)
library(knitr)

stroke_tb <- read.csv("data/datasets/stroke_dataset.csv") 

# If you have a specific absolute path, you can uncomment the following line:
# stroke_tb <- read.csv("/Users/batta/Documents/University/dsiba_2024/DSIBA_StrokePredictionAnalysis/data/datasets/stroke_dataset.csv")

kable(stroke_tb)

str(stroke_tb)

colSums(stroke_tb == "Unknown" | stroke_tb == "N/A" | stroke_tb == "")

sum(duplicated(stroke_tb))

# The dataset seems to be fairly clean. Only bmi and smoking_status have missing values and no duplicated rows.
# We'll address these issues next.

# Identify categorical and numerical variables 
categorical_variables <- names(stroke_tb)[sapply(stroke_tb, function(x) is.character(x) || is.integer(x))]
numerical_variables <- names(stroke_tb)[sapply(stroke_tb, is.double)]

cat(sprintf("Categorical features: %s\nNumerical features: %s",
            paste(categorical_variables, collapse = ", "),
            paste(numerical_variables, collapse = ", ")))

# Here, some variables aren't recognized correctly (bmi should be numeric, hypertension & heart_disease should be categorical).

## Missing Values

# Replace missing values in bmi and smoking_status with NA.
# Convert bmi to numeric and remove id.
cleaned_stroke_tb <- replace_with_na(
  data = stroke_tb,
  replace = list(
    bmi = c("N/A", ""),  
    smoking_status = c("Unknown", "") 
  )
) %>%
  mutate(bmi = as.numeric(bmi)) %>%
  select(-id)

kable(cleaned_stroke_tb)

# Re-identify categorical and numerical variables after cleaning
categorical_variables <- names(cleaned_stroke_tb)[sapply(cleaned_stroke_tb, function(x) is.character(x) || is.integer(x))]
numerical_variables <- names(cleaned_stroke_tb)[sapply(cleaned_stroke_tb, is.double)]

categorical_vars <- categorical_variables
numerical_vars <- numerical_variables

cat("Categorical Variables:\n", paste(categorical_vars, collapse = ", "), "\n")
cat("Numerical Variables:\n", paste(numerical_vars, collapse = ", "), "\n")

# Save cleaned dataset
write.csv(cleaned_stroke_tb, "data/datasets/cleaned_stroke_tb.csv", row.names = FALSE)

# Replace NAs in bmi with median and in smoking_status with mode
bmi_median <- median(cleaned_stroke_tb$bmi, na.rm = TRUE)
smoking_status_mode <- names(which.max(table(cleaned_stroke_tb$smoking_status, useNA = "no")))

preprocessed_stroke_tb <- cleaned_stroke_tb %>%
  mutate(
    bmi = ifelse(is.na(bmi), bmi_median, bmi),
    smoking_status = ifelse(is.na(smoking_status), smoking_status_mode, smoking_status)
  )

kable(preprocessed_stroke_tb)

write.csv(preprocessed_stroke_tb, "data/datasets/preprocessed_stroke_tb.csv", row.names = FALSE)

## Outlier Analysis

variables <- c("age", "avg_glucose_level", "bmi")

# Boxplots for outlier visualization
for (var in variables) {
  p <- ggplot(preprocessed_stroke_tb, aes_string(y = var)) +
    geom_boxplot(outlier.color = "red", outlier.size = 2) +
    labs(title = paste("Boxplot of", var), y = var, x = "")
  
  print(ggplotly(p))
}

identify_outliers <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- column[column < lower_bound | column > upper_bound]
  return(outliers)
}

# Apply to variables
outliers_age <- identify_outliers(preprocessed_stroke_tb$age)
outliers_glucose <- identify_outliers(preprocessed_stroke_tb$avg_glucose_level)
outliers_bmi <- identify_outliers(preprocessed_stroke_tb$bmi)

count_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outlier_count <- sum(column < lower_bound | column > upper_bound, na.rm = TRUE)
  return(outlier_count)
}

outlier_counts <- sapply(preprocessed_stroke_tb[, c("age", "avg_glucose_level", "bmi")], count_outliers)
outlier_counts

# Examine categorical variable distributions
lapply(preprocessed_stroke_tb[categorical_vars], table) %>%
  lapply(as.data.frame) %>%
  bind_rows(.id = "Variable") %>%
  rename(Value = Var1, Count = Freq) %>%
  kable(col.names = c("Variable", "Category", "Count"))

# Remove less frequent categories in gender ("Other") and work_type ("Never_worked")
outlier_stroke_tb <- preprocessed_stroke_tb %>%
  filter(work_type != "Never_worked") %>%
  filter(gender != "Other")

write.csv(outlier_stroke_tb, "data/datasets/outlier_stroke_tb.csv", row.names = FALSE)
library(caret)
library(tidyverse)
library(vcd)
library(knitr)
library(janitor)
library(caret)
library(MLmetrics)
library(pROC)
library(kableExtra)

# Final Data Preparations
stroke_tb <- read.csv("data/datasets/stroke_dataset.csv")
outlier_stroke_tb <- read.csv("data/datasets/outlier_stroke_tb.csv")
str(outlier_stroke_tb)

### Feature Encoding
encoded_stroke_tb <- outlier_stroke_tb

# Encode binary variables
encoded_stroke_tb$gender <- factor(
  ifelse(encoded_stroke_tb$gender == "Female", "1",
         ifelse(encoded_stroke_tb$gender == "Male", "0", NA))
)

encoded_stroke_tb$ever_married <- factor(
  ifelse(encoded_stroke_tb$ever_married == "Yes", "1",
         ifelse(encoded_stroke_tb$ever_married == "No", "0", NA))
)

encoded_stroke_tb$Residence_type <- factor(
  ifelse(encoded_stroke_tb$Residence_type == "Urban", "1",
         ifelse(encoded_stroke_tb$Residence_type == "Rural", "0", NA))
)

# smoking_status
smoking_status_matrix <- model.matrix(~ smoking_status - 1, data = outlier_stroke_tb)
colnames(smoking_status_matrix) <- gsub("smoking_status", "smoking_status", colnames(smoking_status_matrix))
# Exclude reference level "never smoked"
smoking_status_matrix <- smoking_status_matrix[, -grep("never smoked", colnames(smoking_status_matrix))]

# work_type
work_type_matrix <- model.matrix(~ work_type - 1, data = outlier_stroke_tb)
colnames(work_type_matrix) <- gsub("work_type", "work_type", colnames(work_type_matrix))
# Exclude reference level "Private"
work_type_matrix <- work_type_matrix[, -grep("Private", colnames(work_type_matrix))]

# Bind matrices to dataset
encoded_stroke_tb <- cbind(
  encoded_stroke_tb[, !(names(outlier_stroke_tb) %in% c("smoking_status", "work_type"))],
  smoking_status_matrix,
  work_type_matrix
)

str(encoded_stroke_tb)

# Ensure binary vars are factors
binary_vars <- c("gender", "hypertension", "heart_disease", "ever_married", "Residence_type",
                 "work_typechildren", "work_typeGovt_job", "work_typeSelf-employed",
                 "stroke", "smoking_statusformerly smoked", "smoking_statussmokes")

encoded_stroke_tb[binary_vars] <- lapply(encoded_stroke_tb[binary_vars], function(col) {
  factor(col, levels = c("0", "1"))
})

# Clean column names
colnames(encoded_stroke_tb) <- make.names(colnames(encoded_stroke_tb))

# Display final dataset structure
kable(encoded_stroke_tb)
str(encoded_stroke_tb)

### Multicollinearity
cat_variables = c("gender", "hypertension", "heart_disease", "ever_married", "Residence_type",
                  "work_typechildren", "work_typeGovt_job", "work_typeSelf.employed",
                  "smoking_statusformerly.smoked", "smoking_statussmokes")

for (index1 in 1:(length(cat_variables) - 1)) {
  var1 <- cat_variables[index1]
  
  for (index2 in (index1 + 1):length(cat_variables)) {
    var2 <- cat_variables[index2]
    
    contingency_table <- table(encoded_stroke_tb[[var1]], encoded_stroke_tb[[var2]])
    
    if (any(contingency_table == 0)) {
      chi_square_result <- fisher.test(contingency_table, simulate.p.value = TRUE)
    } else {
      chi_square_result <- chisq.test(contingency_table)
    }
    
    cramers_v <- assocstats(contingency_table)$cramer
    
    if (cramers_v > 0.3 && chi_square_result$p.value < 0.05) {
      cat("Relationship between", var1, "and", var2, "\n")
      cat("P-value:", chi_square_result$p.value, "\n")
      cat("Cramér's V:", cramers_v, "\n")
      print(contingency_table)
      cat("\n")
    }
  }
}

# Drop work_typechildren
encoded_stroke_tb <- encoded_stroke_tb %>% 
  select(-work_typechildren)
str(encoded_stroke_tb)

### Dataset Balancing
low_risk_age_tb <- encoded_stroke_tb %>%
  filter(age < 60)

high_risk_age_tb <- encoded_stroke_tb %>%
  filter(age >= 60)

# Count stroke proportion
encoded_stroke_tb %>%
  group_by(stroke) %>%
  summarize(count = n()) %>%
  mutate(proportion = round(count / sum(count), 2))

low_risk_age_tb %>%
  group_by(stroke) %>%
  summarize(count = n()) %>%
  mutate(proportion = round(count / sum(count), 2))

high_risk_age_tb %>%
  group_by(stroke) %>%
  summarize(count = n()) %>%
  mutate(proportion = round(count / sum(count), 2))

# Balance dataset using upSample
balanced_stroke_tb <- upSample(
  x = encoded_stroke_tb[, names(encoded_stroke_tb) != "stroke"],
  y = encoded_stroke_tb$stroke,
  yname = "stroke"
)

high_risk_age_data <- upSample(
  x = high_risk_age_tb[, names(high_risk_age_tb) != "stroke"],
  y = high_risk_age_tb$stroke,
  yname = "stroke"
)

table(balanced_stroke_tb$stroke)

### Scaling
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

scaled_stroke_tb <- balanced_stroke_tb %>%
  mutate(across(where(is.double), ~ standardize(.)))

scaled_unbalanced_stroke_tb <- encoded_stroke_tb %>%
  mutate(across(where(is.double), ~ standardize(.)))

scaled_high_risk_age_tb <- high_risk_age_data %>%
  mutate(across(where(is.double), ~ standardize(.)))

kable(scaled_stroke_tb)

## Metrics and Terms - (no code here, just comments)

## Baseline Model
### Train-test split
unbalanced_original_data <- scaled_unbalanced_stroke_tb

cat_variables <- c("gender", "hypertension", "heart_disease", "ever_married",
                   "Residence_type", "smoking_statusformerly.smoked", 
                   "smoking_statussmokes", "work_typeGovt_job", "work_typeSelf.employed", "stroke")

unbalanced_original_data[cat_variables] <- lapply(unbalanced_original_data[cat_variables], function(col) {
  if (!is.factor(col)) col <- factor(col)
  levels(col) <- c("0", "1")
  return(col)
})

unbalanced_original_data$stroke <- factor(
  unbalanced_original_data$stroke,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

set.seed(42)
trainIndex <- createDataPartition(unbalanced_original_data$stroke, p = 0.8, list = FALSE, times = 1)
train_unbalanced <- unbalanced_original_data[trainIndex, ]
test_unbalanced <- unbalanced_original_data[-trainIndex, ]

colnames(test_unbalanced) <- make.names(colnames(test_unbalanced))
colnames(test_unbalanced) <- colnames(train_unbalanced)

### Model
custom_summary <- function(data, lev = NULL, model = NULL) {
  precision <- tryCatch(posPredValue(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  recall <- tryCatch(sensitivity(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  roc_auc <- tryCatch(pROC::auc(pROC::roc(data$obs, data[, lev[1]])), error = function(e) NA)
  c(Precision = precision, Recall = recall, ROC = roc_auc)
}

ctrl <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = custom_summary
)

set.seed(42)
logistic_model_1 <- train(
  stroke ~ ., 
  data = train_unbalanced,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

train_predictions_prob <- predict(logistic_model_1, newdata = train_unbalanced, type = "prob")
train_predictions_class <- ifelse(train_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
train_predictions_class <- factor(train_predictions_class, levels = levels(train_unbalanced$stroke))

conf_matrix_train <- confusionMatrix(train_predictions_class, train_unbalanced$stroke, positive = "Yes")

precision <- conf_matrix_train$byClass["Precision"]
recall <- conf_matrix_train$byClass["Recall"]
f1_score <- conf_matrix_train$byClass["F1"]
specificity <- conf_matrix_train$byClass["Specificity"]
accuracy <- conf_matrix_train$overall["Accuracy"]

classification_report_train <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(classification_report_train)
print(conf_matrix_train)

### Evaluation and Discussion
test_predictions_prob <- predict(logistic_model_1, newdata = test_unbalanced, type = "prob")
test_predictions_class <- ifelse(test_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
test_predictions_class <- factor(test_predictions_class, levels = c("No", "Yes"))
test_unbalanced$stroke <- factor(test_unbalanced$stroke, levels = c("No", "Yes"))

print(table(test_predictions_class, test_unbalanced$stroke))

conf_matrix_test <- confusionMatrix(
  data = test_predictions_class,
  reference = test_unbalanced$stroke,
  positive = "Yes"
)

precision <- ifelse(!is.na(conf_matrix_test$byClass["Precision"]), conf_matrix_test$byClass["Precision"], 0)
recall <- ifelse(!is.na(conf_matrix_test$byClass["Recall"]), conf_matrix_test$byClass["Recall"], 0)
f1_score <- ifelse(!is.na(conf_matrix_test$byClass["F1"]), conf_matrix_test$byClass["F1"], 0)
specificity <- conf_matrix_test$byClass["Specificity"]
accuracy <- conf_matrix_test$overall["Accuracy"]

classification_report_test <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(conf_matrix_test)
print(classification_report_test)

roc_curve <- roc(test_unbalanced$stroke, test_predictions_prob[, "Yes"], levels = c("No", "Yes"))
auc_value <- auc(roc_curve)
ggplot(data = data.frame(
  TPR = roc_curve$sensitivities,
  FPR = 1 - roc_curve$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "ROC Curve", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  annotate("text", x = 0.5, y = 0.05, label = paste("AUC =", round(auc_value, 3)), color = "black") +
  theme_minimal()

# Placeholder function call (assuming `generate_importance_table` is defined elsewhere)
# generate_importance_table(logistic_model_2, "Logistic Model 2")

## Balanced Baseline Model
### Train-test split
balanced_original_data <- scaled_stroke_tb
balanced_original_data[cat_variables] <- lapply(balanced_original_data[cat_variables], function(col) {
  if (!is.factor(col)) col <- factor(col)
  levels(col) <- c("0", "1")
  return(col)
})

balanced_original_data$stroke <- factor(
  balanced_original_data$stroke,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

set.seed(42)
trainIndex <- createDataPartition(balanced_original_data$stroke, p = 0.8, list = FALSE, times = 1)
train_balanced <- balanced_original_data[trainIndex, ]
test_balanced <- balanced_original_data[-trainIndex, ]

custom_summary <- function(data, lev = NULL, model = NULL) {
  precision <- tryCatch(posPredValue(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  recall <- tryCatch(sensitivity(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  roc_auc <- tryCatch(pROC::auc(pROC::roc(data$obs, data[, lev[1]])), error = function(e) NA)
  c(Precision = precision, Recall = recall, ROC = roc_auc)
}

ctrl <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = custom_summary
)

set.seed(42)
logistic_model_2 <- train(
  stroke ~ ., 
  data = train_balanced,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

train_predictions_prob <- predict(logistic_model_2, newdata = train_balanced, type = "prob")
train_predictions_class <- ifelse(train_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
train_predictions_class <- factor(train_predictions_class, levels = levels(train_balanced$stroke))

conf_matrix_train <- confusionMatrix(train_predictions_class, train_balanced$stroke, positive = "Yes")

precision <- conf_matrix_train$byClass["Precision"]
recall <- conf_matrix_train$byClass["Recall"]
f1_score <- conf_matrix_train$byClass["F1"]
specificity <- conf_matrix_train$byClass["Specificity"]
accuracy <- conf_matrix_train$overall["Accuracy"]

classification_report_train <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(classification_report_train)
print(conf_matrix_train)

### Evaluation and Discussion
test_predictions_prob <- predict(logistic_model_2, newdata = test_balanced, type = "prob")
test_predictions_class <- ifelse(test_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
test_predictions_class <- factor(test_predictions_class, levels = c("No", "Yes"))
test_balanced$stroke <- factor(test_balanced$stroke, levels = c("No", "Yes"))

print(table(test_predictions_class, test_balanced$stroke))

conf_matrix_test <- confusionMatrix(
  data = test_predictions_class,
  reference = test_balanced$stroke,
  positive = "Yes"
)

precision <- ifelse(!is.na(conf_matrix_test$byClass["Precision"]), conf_matrix_test$byClass["Precision"], 0)
recall <- ifelse(!is.na(conf_matrix_test$byClass["Recall"]), conf_matrix_test$byClass["Recall"], 0)
f1_score <- ifelse(!is.na(conf_matrix_test$byClass["F1"]), conf_matrix_test$byClass["F1"], 0)
specificity <- conf_matrix_test$byClass["Specificity"]
accuracy <- conf_matrix_test$overall["Accuracy"]

classification_report_test <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(conf_matrix_test)
print(classification_report_test)

roc_curve <- roc(test_balanced$stroke, test_predictions_prob[, "Yes"], levels = c("No", "Yes"))
auc_value <- auc(roc_curve)
ggplot(data = data.frame(
  TPR = roc_curve$sensitivities,
  FPR = 1 - roc_curve$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "ROC Curve", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  annotate("text", x = 0.5, y = 0.05, label = paste("AUC =", round(auc_value, 3)), color = "black") +
  theme_minimal()

# generate_importance_table(logistic_model_2, "Logistic Model 2")

## Low-Risk Age Model
low_risk_age_tb[cat_variables] <- lapply(low_risk_age_tb[cat_variables], function(col) {
  if (!is.factor(col)) col <- factor(col)
  levels(col) <- c("0", "1")
  return(col)
})

low_risk_age_tb$stroke <- factor(
  low_risk_age_tb$stroke,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

set.seed(42)
trainIndex <- createDataPartition(low_risk_age_tb$stroke, p = 0.8, list = FALSE, times = 1)
train_low_risk <- low_risk_age_tb[trainIndex, ]
test_low_risk <- low_risk_age_tb[-trainIndex, ]

train_low_risk <- upSample(
  x = train_low_risk[, names(train_low_risk) != "stroke"],
  y = train_low_risk$stroke,
  yname = "stroke"
)

train_low_risk <- train_low_risk %>%
  mutate(across(where(is.double), ~ standardize(.)))
test_low_risk <- test_low_risk %>%
  mutate(across(where(is.double), ~ standardize(.)))

custom_summary <- function(data, lev = NULL, model = NULL) {
  precision <- tryCatch(posPredValue(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  recall <- tryCatch(sensitivity(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  roc_auc <- tryCatch(pROC::auc(pROC::roc(data$obs, data[, lev[1]])), error = function(e) NA)
  c(Precision = precision, Recall = recall, ROC = roc_auc)
}

ctrl <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = custom_summary
)

set.seed(42)
logistic_model_3 <- train(
  stroke ~ ., 
  data = train_low_risk,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

train_predictions_prob <- predict(logistic_model_3, newdata = train_low_risk, type = "prob")
train_predictions_class <- ifelse(train_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
train_predictions_class <- factor(train_predictions_class, levels = levels(train_low_risk$stroke))

conf_matrix_train <- confusionMatrix(train_predictions_class, train_low_risk$stroke, positive = "Yes")

precision <- conf_matrix_train$byClass["Precision"]
recall <- conf_matrix_train$byClass["Recall"]
f1_score <- conf_matrix_train$byClass["F1"]
specificity <- conf_matrix_train$byClass["Specificity"]
accuracy <- conf_matrix_train$overall["Accuracy"]

classification_report_train <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(classification_report_train)
print(conf_matrix_train)

### Evaluation and Discussion
test_predictions_prob <- predict(logistic_model_3, newdata = test_low_risk, type = "prob")
test_predictions_class <- ifelse(test_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
test_predictions_class <- factor(test_predictions_class, levels = c("No", "Yes"))
test_low_risk$stroke <- factor(test_low_risk$stroke, levels = c("No", "Yes"))

print(table(test_predictions_class, test_low_risk$stroke))

conf_matrix_test <- confusionMatrix(
  data = test_predictions_class,
  reference = test_low_risk$stroke,
  positive = "Yes"
)

precision <- ifelse(!is.na(conf_matrix_test$byClass["Precision"]), conf_matrix_test$byClass["Precision"], 0)
recall <- ifelse(!is.na(conf_matrix_test$byClass["Recall"]), conf_matrix_test$byClass["Recall"], 0)
f1_score <- ifelse(!is.na(conf_matrix_test$byClass["F1"]), conf_matrix_test$byClass["F1"], 0)
specificity <- conf_matrix_test$byClass["Specificity"]
accuracy <- conf_matrix_test$overall["Accuracy"]

classification_report_test <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(conf_matrix_test)
print(classification_report_test)

roc_curve <- roc(test_low_risk$stroke, test_predictions_prob[, "Yes"], levels = c("No", "Yes"))
auc_value <- auc(roc_curve)
ggplot(data = data.frame(
  TPR = roc_curve$sensitivities,
  FPR = 1 - roc_curve$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "ROC Curve", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  annotate("text", x = 0.5, y = 0.05, label = paste("AUC =", round(auc_value, 3)), color = "black") +
  theme_minimal()

# generate_importance_table(logistic_model_3, "Logistic Model 3")

## High-Risk Age Model
high_risk_age_data <- scaled_high_risk_age_tb
high_risk_age_data[cat_variables] <- lapply(high_risk_age_data[cat_variables], function(col) {
  if (!is.factor(col)) col <- factor(col)
  levels(col) <- c("0", "1")
  return(col)
})

high_risk_age_data$stroke <- factor(
  high_risk_age_data$stroke,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

high_risk_age_ablation  <- high_risk_age_data %>% 
  select(-c("age"))

set.seed(42)
trainIndex <- createDataPartition(high_risk_age_data$stroke, p = 0.8, list = FALSE, times = 1)
train_high_risk <- high_risk_age_data[trainIndex, ]
test_high_risk <- high_risk_age_data[-trainIndex, ]

train_high_risk_ablated <- high_risk_age_ablation[trainIndex, ]
test_high_risk_ablated <- high_risk_age_ablation[-trainIndex, ]

custom_summary <- function(data, lev = NULL, model = NULL) {
  precision <- tryCatch(posPredValue(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  recall <- tryCatch(sensitivity(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  roc_auc <- tryCatch(pROC::auc(pROC::roc(data$obs, data[, lev[1]])), error = function(e) NA)
  c(Precision = precision, Recall = recall, ROC = roc_auc)
}

ctrl <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = custom_summary
)

set.seed(42)
logistic_model_4 <- train(
  stroke ~ ., 
  data = train_high_risk,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

train_predictions_prob <- predict(logistic_model_4, newdata = train_high_risk, type = "prob")
train_predictions_class <- ifelse(train_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
train_predictions_class <- factor(train_predictions_class, levels = levels(train_high_risk$stroke))

conf_matrix_train <- confusionMatrix(train_predictions_class, train_high_risk$stroke, positive = "Yes")

precision <- conf_matrix_train$byClass["Precision"]
recall <- conf_matrix_train$byClass["Recall"]
f1_score <- conf_matrix_train$byClass["F1"]
specificity <- conf_matrix_train$byClass["Specificity"]
accuracy <- conf_matrix_train$overall["Accuracy"]

classification_report_train <- data.frame(
  Metric = c("Precision", "Recall", "F1-Score", "Specificity", "Accuracy"),
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(classification_report_train)
print(conf_matrix_train)

### Evaluation and Discussion
test_predictions_prob <- predict(logistic_model_4, newdata = test_high_risk, type = "prob")
test_predictions_class <- ifelse(test_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
test_predictions_class <- factor(test_predictions_class, levels = c("No", "Yes"))
test_high_risk$stroke <- factor(test_high_risk$stroke, levels = c("No", "Yes"))

print(table(test_predictions_class, test_high_risk$stroke))

conf_matrix_test <- confusionMatrix(
  data = test_predictions_class,
  reference = test_high_risk$stroke,
  positive = "Yes"
)

precision <- ifelse(!is.na(conf_matrix_test$byClass["Precision"]), conf_matrix_test$byClass["Precision"], 0)
recall <- ifelse(!is.na(conf_matrix_test$byClass["Recall"]), conf_matrix_test$byClass["Recall"], 0)
f1_score <- ifelse(!is.na(conf_matrix_test$byClass["F1"]), conf_matrix_test$byClass["F1"], 0)
specificity <- conf_matrix_test$byClass["Specificity"]
accuracy <- conf_matrix_test$overall["Accuracy"]

classification_report_test <- data.frame(
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(conf_matrix_test)
print(classification_report_test)

roc_curve <- roc(test_high_risk$stroke, test_predictions_prob[, "Yes"], levels = c("No", "Yes"))
auc_value <- auc(roc_curve)
ggplot(data = data.frame(
  TPR = roc_curve$sensitivities,
  FPR = 1 - roc_curve$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "ROC Curve", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  annotate("text", x = 0.5, y = 0.05, label = paste("AUC =", round(auc_value, 3)), color = "black") +
  theme_minimal()

# generate_importance_table(logistic_model_4, "Logistic Model 4")

### Ablation Analysis on Age
custom_summary <- function(data, lev = NULL, model = NULL) {
  precision <- tryCatch(posPredValue(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  recall <- tryCatch(sensitivity(data$pred, data$obs, positive = lev[1]), error = function(e) NA)
  roc_auc <- tryCatch(pROC::auc(pROC::roc(data$obs, data[, lev[1]])), error = function(e) NA)
  c(Precision = precision, Recall = recall, ROC = roc_auc)
}

ctrl <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = custom_summary
)

set.seed(42)
logistic_model_5 <- train(
  stroke ~ ., 
  data = train_high_risk_ablated,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "ROC"
)

test_predictions_prob <- predict(logistic_model_5, newdata = test_high_risk_ablated, type = "prob")
test_predictions_class <- ifelse(test_predictions_prob[, "Yes"] > 0.5, "Yes", "No")
test_predictions_class <- factor(test_predictions_class, levels = c("No", "Yes"))
test_high_risk_ablated$stroke <- factor(test_high_risk_ablated$stroke, levels = c("No", "Yes"))

print(table(test_predictions_class, test_high_risk_ablated$stroke))

conf_matrix_test <- confusionMatrix(
  data = test_predictions_class,
  reference = test_high_risk_ablated$stroke,
  positive = "Yes"
)

precision <- ifelse(!is.na(conf_matrix_test$byClass["Precision"]), conf_matrix_test$byClass["Precision"], 0)
recall <- ifelse(!is.na(conf_matrix_test$byClass["Recall"]), conf_matrix_test$byClass["Recall"], 0)
f1_score <- ifelse(!is.na(conf_matrix_test$byClass["F1"]), conf_matrix_test$byClass["F1"], 0)
specificity <- conf_matrix_test$byClass["Specificity"]
accuracy <- conf_matrix_test$overall["Accuracy"]

classification_report_test <- data.frame(
  Value = c(precision, recall, f1_score, specificity, accuracy)
)

print(conf_matrix_test)
print(classification_report_test)

roc_curve <- roc(test_high_risk_ablated$stroke, test_predictions_prob[, "Yes"], levels = c("No", "Yes"))
auc_value <- auc(roc_curve)
ggplot(data = data.frame(
  TPR = roc_curve$sensitivities,
  FPR = 1 - roc_curve$specificities
), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "red") +
  labs(title = "ROC Curve", x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
  annotate("text", x = 0.5, y = 0.05, label = paste("AUC =", round(auc_value, 3)), color = "black") +
  theme_minimal()

# generate_importance_table(logistic_model_5, "Logistic Model 5")
# =========================================================
# SECTION 5 / INFERENTIAL STATISTICS
# Internet Advertisements Dataset
# =========================================================

# -------------------------
# 0. Packages
# -------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(caret)
  library(glmnet)
  library(pROC)
  library(randomForest)
})

# -------------------------
# 1. Configuration
# -------------------------
file_path <- "add.csv"    # Change if needed
seed_value <- 99
train_ratio <- 0.70

# =========================================================
# 2. Helper functions
# =========================================================

print_header <- function(title) {
  cat("\n", strrep("=", 70), "\n", title, "\n", strrep("=", 70), "\n", sep = "")
}

get_confusion_matrix <- function(actual, predicted) {
  actual_num <- as.numeric(as.character(actual))
  predicted_num <- as.numeric(predicted)

  TN <- sum(actual_num == 0 & predicted_num == 0)
  FP <- sum(actual_num == 0 & predicted_num == 1)
  FN <- sum(actual_num == 1 & predicted_num == 0)
  TP <- sum(actual_num == 1 & predicted_num == 1)

  matrix(
    c(TN, FP, FN, TP),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      Actual = c("0", "1"),
      Predicted = c("0", "1")
    )
  )
}

evaluate_model_clean <- function(prob, actual, threshold, label) {
  actual_num <- as.numeric(as.character(actual))
  pred_class <- ifelse(prob > threshold, 1, 0)
  cm <- get_confusion_matrix(actual, pred_class)

  TN <- cm[1, 1]
  FP <- cm[1, 2]
  FN <- cm[2, 1]
  TP <- cm[2, 2]

  accuracy <- (TP + TN) / sum(cm)
  sensitivity <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
  nir         <- max(prop.table(table(actual)))
  
  balanced_accuracy <- mean(c(sensitivity, specificity), na.rm = TRUE)

  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  f1_score <- ifelse(
    is.na(precision) || is.na(sensitivity) || (precision + sensitivity == 0),
    NA,
    2 * precision * sensitivity / (precision + sensitivity)
  )

  roc_obj <- roc(actual_num, prob, quiet = TRUE)
  auc_value <- as.numeric(auc(roc_obj))

  cat("\n", label, "\n", sep = "")
  cat("Threshold : 0.5","\n", sep = "")
  cat("Accuracy: ", round(accuracy, 4), "\n", sep = "")
  cat("Sensitivity: ", round(sensitivity, 4), "\n", sep = "")
  cat("Specificity: ", round(specificity, 4), "\n", sep = "")
  cat("NIR: ", round(nir, 4), "\n", sep = "")
  cat("AUC: ", round(auc_value, 4), "\n", sep = "")
  cat("Confusion Matrix:\n")
  print(cm)
  cat("\n")

  list(
    threshold = threshold,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = balanced_accuracy,
    auc = auc_value,
    f1 = f1_score,
    confusion_matrix = cm,
    pred_class = pred_class,
    prob = prob,
    roc = roc_obj
  )
}

plot_roc_curve <- function(roc_obj, title_text) {
  plot(roc_obj, main = title_text, col = "blue", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "gray")
}

# =========================================================
# 3. Read and preprocess data
# =========================================================
print_header("1. Read and preprocess data")

ad_data <- read.csv(
  file_path,
  header = FALSE,
  na.strings = "?",
  skip = 1,
  stringsAsFactors = FALSE
)

# Remove index column
ad_data <- ad_data[, -1]

# Assign column names
colnames(ad_data) <- c(
  "height", "width", "ratio", "local",
  paste0("url_", 1:457),
  paste0("origurl_", 1:495),
  paste0("ancurl_", 1:472),
  paste0("alt_", 1:111),
  paste0("caption_", 1:19),
  "Class"
)

# Trim whitespace
ad_data[] <- lapply(ad_data, function(x) {
  if (is.character(x)) trimws(x) else x
})

# Convert remaining question marks to NA
ad_data[ad_data == "?"] <- NA

# Convert predictors to numeric
ad_data[, 1:1558] <- lapply(ad_data[, 1:1558], as.numeric)

# Convert class label to binary factor
ad_data$Class <- gsub("\\.", "", ad_data$Class)
ad_data$Class <- ifelse(ad_data$Class == "ad", 1, 0)
ad_data$Class <- factor(ad_data$Class, levels = c(0, 1))

# Convert local to factor after imputation
ad_data$height[is.na(ad_data$height)] <- median(ad_data$height, na.rm = TRUE)
ad_data$width[is.na(ad_data$width)] <- median(ad_data$width, na.rm = TRUE)
ad_data$local[is.na(ad_data$local)] <- 1
ad_data$local <- factor(ad_data$local, levels = c(0, 1))

# Remove derived ratio to avoid redundancy in the full model
ad_data <- ad_data %>% select(-ratio)

# Remove duplicates
ad_data <- ad_data[!duplicated(ad_data), ]

cat("Data shape after preprocessing:", nrow(ad_data), "rows x", ncol(ad_data), "columns\n")
cat("Class distribution:\n")
print(table(ad_data$Class))

# =========================================================
# 4. Inferential analysis for width
# =========================================================
print_header("2. Inferential analysis for width")

set.seed(seed_value)
shapiro_df <- ad_data %>%
  group_by(Class) %>%
  summarise(
    shapiro_p = shapiro.test(sample(width, min(500, n())))$p.value,
    .groups = "drop"
  )

cat("Shapiro-Wilk test results:\n")
print(shapiro_df)

qq_plot <- ggplot(ad_data, aes(sample = width)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Class) +
  theme_minimal() +
  labs(
    title = "QQ Plot of Width by Class",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  geom_text(
    data = shapiro_df,
    aes(
      x = Inf,
      y = -Inf,
      label = paste0("Shapiro p = ", signif(shapiro_p, 4))
    ),
    hjust = 1.1,
    vjust = -0.5,
    inherit.aes = FALSE
  )
print(qq_plot)

width_summary <- ad_data %>%
  group_by(Class) %>%
  summarise(
    n = n(),
    mean_width = mean(width, na.rm = TRUE),
    sd_width = sd(width, na.rm = TRUE),
    median_width = median(width, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nWidth summary by class:\n")
print(width_summary)

width_ttest <- t.test(width ~ Class, data = ad_data)
width_wilcox <- wilcox.test(width ~ Class, data = ad_data)

cat("\nWelch two-sample t-test:\n")
print(width_ttest)

cat("\nWilcoxon rank-sum test:\n")
print(width_wilcox)

# =========================================================
# 5. Inferential analysis for local
# =========================================================
print_header("3. Inferential analysis for local")

tab_local <- table(ad_data$Class, ad_data$local)
cat("Contingency table:\n")
print(tab_local)

success <- c(tab_local["1", "1"], tab_local["0", "1"])
n_total <- c(sum(tab_local["1", ]), sum(tab_local["0", ]))

prop_check <- data.frame(
  Group = c("ad", "nonad"),
  n = n_total,
  p_hat = success / n_total,
  n_p = n_total * (success / n_total),
  n_1_minus_p = n_total * (1 - success / n_total)
)

cat("\nAssumption check for proportion test:\n")
print(prop_check)

local_prop_test <- prop.test(success, n_total, alternative = "greater")
cat("\nOne-sided two-sample proportion test:\n")
print(local_prop_test)

# =========================================================
# 6. Train-test split
# =========================================================
print_header("4. Train-test split")

set.seed(seed_value)
train_index <- createDataPartition(ad_data$Class, p = train_ratio, list = FALSE)
train_data <- ad_data[train_index, ]
test_data <- ad_data[-train_index, ]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# =========================================================
# 7. Lasso Logistic Regression
# =========================================================
print_header("5. Lasso Logistic Regression")

class_table <- table(train_data$Class)
cat("Training class distribution:\n")
print(class_table)

weights <- ifelse(
  train_data$Class == "1",
  1 / sum(train_data$Class == "1"),
  1 / sum(train_data$Class == "0")
)

x_train <- model.matrix(Class ~ . - 1, data = train_data)
y_train <- train_data$Class
x_test <- model.matrix(Class ~ . - 1, data = test_data)
y_test <- test_data$Class

set.seed(seed_value)
cv_model <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1,
  weights = weights
)

best_lambda <- cv_model$lambda.min
lambda_1se <- cv_model$lambda.1se

cat("Best lambda (lambda.min):", best_lambda, "\n")
cat("Conservative lambda (lambda.1se):", lambda_1se, "\n")

coef_lasso <- coef(cv_model, s = "lambda.min")
coef_df <- data.frame(
  feature = rownames(as.matrix(coef_lasso)),
  coefficient = as.numeric(as.matrix(coef_lasso)[, 1])
)

selected_features <- coef_df %>% filter(coefficient != 0)
cat("Number of selected features (excluding intercept):", nrow(selected_features) - 1, "\n")
cat("Top selected features:\n")
print(head(selected_features, 20))

lasso_prob_train <- as.vector(
  predict(cv_model, s = "lambda.min", newx = x_train, type = "response")
)
lasso_threshold <- 0.5

lasso_prob_test <- as.vector(
  predict(cv_model, s = "lambda.min", newx = x_test, type = "response")
)

lasso_res <- evaluate_model_clean(
  prob = lasso_prob_test,
  actual = y_test,
  threshold = lasso_threshold,
  label = "Model 1 - Lasso Logistic Regression"
)

plot_roc_curve(lasso_res$roc, "ROC Curve - Lasso Logistic Regression")

# =========================================================
# 8. Random Forest
# =========================================================
print_header("6. Random Forest")

set.seed(seed_value)
rf_model <- randomForest(
  Class ~ .,
  data = train_data,
  ntree = 500,
  mtry = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

print(rf_model)

rf_prob_train <- predict(rf_model, newdata = train_data, type = "prob")[, "1"]
rf_threshold <- 0.5

rf_prob_test <- predict(rf_model, newdata = test_data, type = "prob")[, "1"]
rf_res <- evaluate_model_clean(
  prob = rf_prob_test,
  actual = test_data$Class,
  threshold = rf_threshold,
  label = "Model 2 - Random Forest"
)

plot_roc_curve(rf_res$roc, "ROC Curve - Random Forest")
varImpPlot(rf_model)

# =========================================================
# 9. Comparison table
# =========================================================
print_header("7. Comparison table")

comparison_table <- data.frame(
  Model = c("Lasso Logistic Regression", "Random Forest"),
  Accuracy = c(lasso_res$accuracy, rf_res$accuracy),
  Sensitivity = c(lasso_res$sensitivity, rf_res$sensitivity),
  Specificity = c(lasso_res$specificity, rf_res$specificity),
  Balanced_Accuracy = c(lasso_res$balanced_accuracy, rf_res$balanced_accuracy),
  AUC = c(lasso_res$auc, rf_res$auc),
  F1 = c(lasso_res$f1, rf_res$f1)
)

comparison_table <- comparison_table %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

print(comparison_table)

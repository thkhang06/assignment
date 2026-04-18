# Probability and Statistics Project: Internet Advertisement Classification

This repository contains the R source code and dataset for the **Probability and Statistics Project (HCMUT)**. The objective of this project is to perform comprehensive statistical analysis and build machine learning models to classify whether an image on a webpage is an advertisement ("ad") or not ("nonad").

## 📁 Repository Structure

* **`add.csv`**: The original Internet Advertisements dataset obtained from the UCI Machine Learning Repository (also available on Kaggle). It contains 3,279 observations and 1,559 attributes.
* **`01_Data_Preprocessing.R`**: Script for initial dataset cleaning. It handles high-dimensionality, removes extraneous whitespace, maps the `Class` target to binary, performs Median imputation for skewed numeric features (`height`, `width`), handles outliers using the robust IQR method, and removes duplicate rows.
* **`02_Descriptive_Statistics.R`**: Contains the code to generate EDA (Exploratory Data Analysis) visualizations. Includes Histograms, Boxplots, Scatterplots, and Correlation Matrices leveraging `ggplot2` to evaluate feature distributions intuitively.
* **`03_Inferential_and_ML_Models.R`**: The core statistical script. It includes:
  * **Hypothesis Testing:** Welch's t-test, Wilcoxon rank-sum test, and 2-sample Proportion tests.
  * **Machine Learning Models:** Implementation of Lasso-penalized Logistic Regression (via `glmnet`) with Cross-Validation and class weighting to tackle severe Class Imbalance. It also implements Random Forest (`randomForest`) to capture non-linear relationships.
  * **Evaluation:** Confusion matrices, Accuracy, Sensitivity, Specificity, and ROC/AUC curves.

## 🚀 How to Run

1. Clone this repository to your local machine.
2. Ensure you have the following R packages installed:
   ```R
   install.packages(c("dplyr", "ggplot2", "caret", "glmnet", "pROC", "randomForest", "patchwork"))
   ```
3. Open any of the `.R` scripts in RStudio.
4. Set your working directory to the folder containing `add.csv` and run the scripts sequentially from `01` to `03`.

## 📊 Summary of Results

* **Descriptive Insights**: Significant visual distinction was found between dimensions (width/height) of ads vs. nonads. The dataset features over 1,500 sparse URL markers heavily skewed towards 0.
* **Inferential Statistics**: Both Parametric and Non-parametric tests successfully rejected the null hypotheses regarding image properties and URL domains, confirming their predictive power.
* **Model Performance**: 
  * The **Lasso Logistic Regression** performed variable selection, shrinking variables down to ~400 components while still achieving **96.28% Accuracy** and an **AUC of 0.9728**.
  * The **Random Forest** captured non-linear tendencies well, leading with **96.97% Accuracy** and an **AUC of 0.9874**.

---
*Developed for the Probability and Statistics Course Report - Faculty of Computer Science & Engineering, Ho Chi Minh City University of Technology.*

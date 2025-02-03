# Load required libraries
library(dplyr)
library(lubridate)
library(caret)
library(mltools)
library(ROSE)
library(glmnet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(ROCR)
library(pROC)
library(MLmetrics)
library(imbalance)
library(rpart)
library(e1071)
library(MLmetrics)
library(readr)
library(MASS)
# getwd()
# setwd('/Users/rashmiprasanna/Desktop/EMBAA_TERM1/PR/S1-S6/End_Term_project_ch23m542 2/')

# 1. Scaling Material: Big Data Preprocessing

# Train OLS model for predicting the weight column
train_OLS <- function(data) {
  X <- as.matrix(data %>% dplyr::select(-PackageWt))
  y <- as.numeric(data$PackageWt)
  X <- cbind(1, X)
  beta <- ginv(t(X) %*% X) %*% t(X) %*% y
  y_pred_ols <- X %*% beta
  return(beta)
}

# Predict the weight column using beta coefficients
predict_OLS <- function(beta, data) {
  X <- as.matrix(data %>% dplyr::select(PackageCirc, PackageCircLength, PackageHeight, PackageVol, PackageWidth))
  X <- cbind(1, X)
  y_pred_ols <- X %*% beta
  nan_indices <- which(is.na(data$PackageWt))
  data$PackageWt[nan_indices] <- y_pred_ols[nan_indices]
  return(data)
}

# Perform data cleaning (handling missing values)
fill_missing_values <- function(data) {
  data$RegAddedDate <- parse_date_time(data$RegAddedDate, orders = "ymd HMS", tz = "UTC")
  data$RegTimestamp <- parse_date_time(data$RegTimestamp, orders = "ymd HMS", tz = "UTC")

  for (col in colnames(data)) {
    if (is.character(data[[col]]) || is.logical(data[[col]])) {
      mode_value <- as.character(names(sort(table(data[[col]]), decreasing = TRUE)[1]))
      data[[col]][is.na(data[[col]])] <- mode_value
    } else if (is.numeric(data[[col]]) && col != "PackageWt") {
      mean_value <- mean(data[[col]], na.rm = TRUE)
      data[[col]][is.na(data[[col]])] <- mean_value
    }
  }
  return(data)
}


# Perform Outlier removal with Z-score threshold
outliers <- function(data) {
  numeric_cols <- select_if(data, is.numeric)
  for (col in colnames(numeric_cols)) {
    z_scores <- abs(scale(data[[col]]))
    data[[col]][z_scores > 3] <- NA
  }
  return(data)
}


missing_values_summary <- function(data) {
  missing_values <- colSums(is.na(data))
  total_values <- nrow(data)
  missing_percentage <- (missing_values / total_values) * 100
  missing_values_df <- data.frame(
    Column = names(missing_values),
    NaN_Values = missing_values,
    Missing_Percentage = missing_percentage
  )
  missing_values_df <- missing_values_df[missing_values_df$NaN_Values > 0, ]
  return(missing_values_df)
}


print("!!! Initial Missing values !!!")
print(missing_values_summary(data))

# Fill missing values
data <- fill_missing_values(data)

print("!!! Missing Values after replacing with Mean and Mode !!!")
print(missing_values_summary(data))
library(tidyr)
# Performing OLS for filling package weight column based on other dimensions of parcels
columns_to_check <- c('PackageCirc', 'PackageCircLength', 'PackageHeight', "PackageVol", "PackageWidth")
data_cleaned <- data %>% drop_na(all_of(columns_to_check))
sample_data <- data_cleaned  %>% dplyr::select(PackageCirc, PackageCircLength, PackageHeight, PackageVol, PackageWidth, PackageWt)
train_data <- sample_data %>% drop_na(PackageWt)
beta <- train_OLS(train_data)
data <- predict_OLS(beta, data)

print("!!!!After replacing remaining null Using Ordinary Least squares!!!!")
print(missing_values_summary(data))

data <- outliers(data)

# Filling missing values as we have removed outliers
print("!!!!After removing Outliers and replacing with NaN values!!!!")
print(missing_values_summary(data))

data <- fill_missing_values(data)

print("!!!!After replacing all null values!!!!")
print(missing_values_summary(data))
library(tidyr)
# Performing OLS for filling package weight column based on other dimensions of parcels
data_cleaned <- data %>% drop_na(all_of(columns_to_check))
sample_data <- data_cleaned  %>% dplyr::select(PackageCirc, PackageCircLength, PackageHeight, PackageVol, PackageWidth, PackageWt)
train_data <- sample_data %>% drop_na(PackageWt)
beta <- train_OLS(train_data)
data <- predict_OLS(beta, data)

print("!!!!Replace weight column outliers using Ordinary Least squares!!!!")
print(missing_values_summary(data))

# Transform data as needed (normalization, encoding categorical variables)
labelling <- function(data) {
  for (col in names(data)) {
    if (is.character(data[[col]]) || is.logical(data[[col]])) {
      data[[col]] <- as.numeric(factor(data[[col]]))
    }
  }
  return(data)
}

# # Perform OneHot encoding on specific columns

onehot_encoding <- function(data) {
    categorical_col <- c("ShiftType", "PackageOverlap")
    for (col in categorical_col) {
      data <- bind_cols(data, model.matrix(~ get(col) - 1, data = data))
      data <- data %>% dplyr::select(-all_of(col))
    }
    return(data)
  }

# Transform data as needed (adding new features based on existing features)
feature_engineering <- function(df_cleaned) {
  df_cleaned <- df_cleaned %>% 
    dplyr::select(-Barcode, -PackageAdviceSystem, -LocIDSecondary, -PackageType) %>%
    mutate(PackageHeight = PackageHeight * 1000,
           Package_size_width = case_when(
             PackageWidth < 0.3 ~ "Small",
             PackageWidth < 0.6 ~ "Medium",
             TRUE ~ "Large"),
           Package_size_height = case_when(
             PackageHeight < 150 ~ "Small",
             PackageHeight < 270 ~ "Medium",
             TRUE ~ "Large"),
           Package_size_weight = case_when(
             PackageWt < 5 ~ "Small",
             PackageWt < 25 ~ "Medium",
             TRUE ~ "Heavy"),
           day_of_week = wday(RegAddedDate, label = TRUE))
  return(df_cleaned)
}

# Get numeric columns
get_numeric_columns <- function(data) {
  numerical_columns <- names(data)[sapply(data, is.numeric)]
  return(numerical_columns)
}


# Perform Data Cleaning
library(dplyr)

library(tidyverse)
#library(dummies)
library(lubridate)

# Function to convert character and logical columns to numeric
label_encode <- function(data) {
  data %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.factor), as.numeric)) %>%
    mutate(across(where(is.logical), as.numeric))
}

# Assuming 'data' is your DataFrame
data <- label_encode(data)

# Ensure there are no NA values
data <- na.omit(data)

# Performing upsampling using SMOTE
library(smotefamily)
handle_imbalance <- function(data, numerical_columns) {
  # Ensure all columns are numeric
  numerical_columns <- names(data)[sapply(data, is.numeric)]
  
  # Separate features and target
  features <- data %>% dplyr::select(all_of(numerical_columns))
  label <- data$Rejected
  # Apply SMOTE
  smote_output <- SMOTE(features, label, K = 5)
  
  # Extract synthetic data
  smote_data <- smote_output$data
  X_smote <- smote_data[, 1:(length(numerical_columns) - 1)]
  y_smote <- smote_data[, length(numerical_columns)]
  
  # Combine features and labels
  combined_df <- cbind(X_smote, y_smote)
  
  return(combined_df)
}
# Read the data
file_path <- "small_data.csv"
data <- read.csv(file_path)
data <- data %>% dplyr::select(-X)
print(paste("Size of the dataset before processing =", nrow(data)))

data <- fill_missing_values(data)
library(tidyr)
columns_to_check <- c('PackageCirc', 'PackageCircLength', 'PackageHeight', "PackageVol", "PackageWidth")
data_cleaned <- data %>% drop_na(all_of(columns_to_check))
sample_data <- data_cleaned  %>% dplyr::select(PackageCirc, PackageCircLength, PackageHeight, PackageVol, PackageWidth, PackageWt)
train_data <- sample_data %>% drop_na(PackageWt)
beta <- train_OLS(train_data)
data <- predict_OLS(beta, data)

data <- outliers(data)
data <- fill_missing_values(data)
library(tidyr)
columns_to_check <- c('PackageCirc', 'PackageCircLength', 'PackageHeight', "PackageVol", "PackageWidth")
data_cleaned <- data %>% drop_na(all_of(columns_to_check))
sample_data <- data_cleaned  %>% dplyr::select(PackageCirc, PackageCircLength, PackageHeight, PackageVol, PackageWidth, PackageWt)
train_data <- sample_data %>% drop_na(PackageWt)
beta <- train_OLS(train_data)
data <- predict_OLS(beta, data)

# Perform Data Transformation
data <- feature_engineering(data)
data <- onehot_encoding(data)
data <- labelling(data)
numerical_columns <- get_numeric_columns(data)
# Load necessary libraries
library(ggplot2)

# Assuming your data is in a dataframe called 'df'



# Box Plot for Package Weight
print(ggplot(data, aes(y = PackageWt)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Package Weight", y = "Weight"))

# Box Plot for Package Height
print(ggplot(data, aes(y = PackageHeight)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Boxplot of Package Height", y = "Height"))

# Load necessary libraries
library(ggcorrplot)
#library(corrplot)

# Calculate correlation matrix
corr_matrix <- cor(data[, c("PackageWt", "PackageCirc", "PackageHeight", "PackageWidth", "PackageVol")], use = "complete.obs")

# Plot correlation heatmap
print(ggcorrplot(corr_matrix, method = "circle", lab = TRUE) +
  labs(title = "Correlation Heatmap of Parcel Dimensions"))

# Bar chart with hue (fill) for categorical variables
print(ggplot(data, aes(x = 'ShiftType', fill = 'PackageOverlap')) +
  geom_bar(position = "dodge") +  # 'dodge' separates the bars by the 'hue'
  theme_minimal() +
  labs(title = "Bar Chart of ShiftType with PackageOverlap as Hue", 
       x = "Shift Type", 
       y = "Count", 
       fill = "Package Overlap") ) # Label for the legend


# Handle Imbalance
combined_df <- handle_imbalance(data, numerical_columns)
print(paste("Size of the dataset After processing =", nrow(data)))

# Store the preprocessed data in a suitable format for machine learning/data analysis (Parquet)
library(arrow)
write_parquet(data, "combined_df.parquet")
print("Successfully exported!!!")

# Scaling Methods: Machine Learning
### Objective: Apply machine learning algorithms. 
###Students can select a dataset where not much preprocessing is required, 
##if the student is working alone. Training time as well as accuracy should be
###compared while using different data sizes

# Logistic Regression function
run_logistic_regression <- function(data) {
  # Print the dimensions of the dataset
  print(dim(data))
  
  # Separate features and labels
  X <- data %>% dplyr::select(-Rejected)
  Y <- data$Rejected
  
  # Split the data into training and testing sets
  set.seed(123)
  trainIndex <- createDataPartition(Y, p = .8, list = FALSE)
  X_train <- X[trainIndex, ]
  X_test <- X[-trainIndex, ]
  y_train <- Y[trainIndex]
  y_test <- Y[-trainIndex]
  
  # Train Logistic Regression model
  lr <- cv.glmnet(as.matrix(X_train), y_train, family = "binomial", alpha = 1, standardize = TRUE)
  
  # Make predictions on the test set
  y_pred <- predict(lr, newx = as.matrix(X_test), s = "lambda.min", type = "class")
  
  # Convert predictions to numeric if they are not already
  y_pred <- as.numeric(y_pred)
  y_test <- as.numeric(y_test)
  
  # Evaluate the model
  accuracy <- mean(y_pred == y_test)
  print(paste("Accuracy:", accuracy))
  print("---------------------------------------")
  
  f1 <- F1_Score(y_test, y_pred)
  print(paste("F1 Score:", f1))
  print("---------------------------------------")
  
  # Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
  mse <- mean((y_test - y_pred)^2)
  rmse <- sqrt(mse)
  print(paste("RMSE:", rmse))
  print("---------------------------------------")
  
  # Generate and print the confusion matrix
  conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_test))
  print("Confusion Matrix:")
  print(conf_matrix)
}

# Load necessary libraries
library(pROC)
library(caret)

#getwd()
run_logistic_regression(data)
#setwd('/Users/rashmiprasanna/Desktop/EMBAA_TERM1/PR/S1-S6/End_Term_project_ch23m542 2/')
#getwd()

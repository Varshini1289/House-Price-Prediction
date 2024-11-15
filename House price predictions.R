# Load necessary libraries
library(dplyr)   
library(ggplot2)

library(gridExtra)
# Load the dataset
housing_data <- read.csv("Housing.csv")

# Explore the dataset
str(housing_data)    
summary(housing_data) 
head(housing_data)

# Check for missing values in the dataset
missing_values <- colSums(is.na(housing_data))

# Display the count of missing values for each column
cat("Missing values in each column:\n")
print(missing_values)

# Verify if the dataset is clean (no missing values)
if (sum(missing_values) == 0) {
  cat("\nThe dataset is clean: No missing values found.\n")
} else {
  cat("\nThe dataset contains", sum(missing_values), "missing values.\n")
}



# Define features and colors
features <- c("price", "area", "bedrooms", "bathrooms", "stories", "parking")
colors <- c("#ff9999", "#66b3ff", "#99ff99", "#ffcc99", "#ff99cc", "#99ccff")

# Create boxplots for each feature and store them in a list
plots <- lapply(1:length(features), function(i) {
  ggplot(housing_data, aes_string(x = features[i])) +
    geom_boxplot(fill = colors[i]) +
    labs(title = paste("Distribution of", toupper(substring(features[i], 1, 1)), substring(features[i], 2))) +
    theme_minimal() +
    theme(plot.title = element_text(size = 12, hjust = 0.5))
})

# Arrange the plots in a grid layout
grid.arrange(grobs = plots, ncol = 3, top = "Outlier Analysis for Housing Features")



# Function to remove outliers based on IQR
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  df <- df[df[[column]] >= lower_bound & df[[column]] <= upper_bound, ]
  return(df)
}

# Remove outliers for price and plot the updated boxplot
housing_data <- remove_outliers(housing_data, "price")


# Check for duplicate rows in the dataset
num_duplicates <- sum(duplicated(housing_data))
cat("Number of duplicate rows:", num_duplicates, "\n")



# Define features and colors
features <- c("price", "area", "bedrooms", "bathrooms", "stories", "parking")
colors <- scales::hue_pal()(length(features))

# Create histograms with KDE for each feature and store them in a list
plots <- lapply(1:length(features), function(i) {
  ggplot(housing_data, aes_string(x = features[i])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = colors[i], color = "black", alpha = 0.7) +
    geom_density(color = "blue", size = 1) +
    labs(title = paste("Distribution of", toupper(substring(features[i], 1, 1)), substring(features[i], 2)),
         x = features[i], y = "Density") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8)
    )
})

# Arrange the plots in a grid layout
grid.arrange(grobs = plots, ncol = 3, top = "Distribution of Housing Features")


library(GGally)
library(ggplot2)

# Select only numeric columns for the pairplot
numeric_cols <- housing_data[, sapply(housing_data, is.numeric)]

# Create the pair plot with customized parameters
g <- ggpairs(
  numeric_cols,
  upper = list(continuous = wrap("cor", size = 3, color = "black")),
  diag = list(continuous = wrap("densityDiag", fill = "blue", alpha = 0.3)),
  lower = list(continuous = wrap("points", alpha = 0.6, size = 1.5, color = "darkblue"))
) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10)
  )

# Add a title
g <- g + ggtitle("Pairwise Relationships of Numeric Housing Features")

print(g)


library(scales)

# List of categorical features
cat_features <- c("mainroad", "guestroom", "basement", "hotwaterheating", "airconditioning", "furnishingstatus")

# Create boxplots for each categorical feature against 'price' and store them in a list
plots <- lapply(cat_features, function(feature) {
  ggplot(housing_data, aes_string(x = feature, y = "price")) +
    geom_boxplot(fill = "skyblue", color = "black", outlier.size = 1) +
    scale_y_continuous(labels = function(x) paste0(x / 1e6, "M")) +
    labs(title = paste(toupper(substring(feature, 1, 1)), substring(feature, 2), "vs Price"),
         x = feature, y = ifelse(feature == "mainroad", "Price", "")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
})

# Arrange the plots in a 2x3 grid layout
grid.arrange(grobs = plots, ncol = 3, top = "Price Distribution by Categorical Features")


library(caTools)

set.seed(100)  # Ensures reproducibility
split <- sample.split(housing_data$price, SplitRatio = 0.7)

# Create train and test datasets
df_train <- subset(housing_data, split == TRUE)
df_test <- subset(housing_data, split == FALSE)

# Check the sizes of the train and test datasets
cat("Training set size:", nrow(df_train), "\n")
cat("Test set size:", nrow(df_test), "\n")


# Data Transformation. 

library(dplyr)

# Function to label encode categorical variables into 0 and 1
label_encode <- function(data) {
  categorical_cols <- sapply(data, is.character) | sapply(data, is.factor)
  
  for (col in names(data)[categorical_cols]) {
    if (all(data[[col]] %in% c("yes", "no"))) {
      # Convert "yes" to 1 and "no" to 0
      data[[col]] <- ifelse(data[[col]] == "yes", 1, 0)
    } else {
      # For other categorical variables, convert to factor and then to numeric
      data[[col]] <- as.numeric(factor(data[[col]])) - 1  # -1 to start from 0
    }
  }
  return(data)
}

# Encode the categorical variables in train and test datasets
df_train <- label_encode(df_train)
df_test <- label_encode(df_test)

# Check the structure of the datasets to confirm encoding
str(df_train)
str(df_test)


# Normality check

# Function to check normality using Shapiro-Wilk test
check_normality <- function(data, feature_name) {
  shapiro_result <- shapiro.test(data[[feature_name]])
  distribution <- ifelse(shapiro_result$p.value > 0.05, "normally", "not normally")
  cat(sprintf("%s is %s distributed.\n", feature_name, distribution))
}

# Perform Shapiro-Wilk test for normality on price and area
check_normality(df_train, "price")
check_normality(df_train, "area")


# Load necessary library
library(stats)

# Function to perform log transformation and check normality
check_log_normality <- function(data, feature_name) {
  # Apply log transformation (ensure values are positive)
  data[[feature_name]] <- log(data[[feature_name]] + 1)  # Adding 1 to avoid log(0)
  
  # Perform Shapiro-Wilk test for normality
  shapiro_result <- shapiro.test(data[[feature_name]])
  distribution <- ifelse(shapiro_result$p.value > 0.05, "normally", "not normally")
  
  cat(sprintf("%s is %s distributed after log transformation.\n", feature_name, distribution))
}

# Perform log transformation and normality check for price and area
check_log_normality(df_train, "price")
check_log_normality(df_train, "area")


# Load necessary libraries
library(ggplot2)

# Perform log transformation on 'price' and 'area'
df_train$price <- log(df_train$price)
df_train$area <- log(df_train$area)

# Check if the transformation was successful
head(df_train)

# Visualize the distribution of transformed price and area
price_plot <- ggplot(df_train, aes(x = price)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#66b3ff", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of Log-Transformed Price") +
  xlab("Log(Price)") +
  ylab("Density") +
  theme_minimal()

area_plot <- ggplot(df_train, aes(x = area)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#99ccff", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  ggtitle("Distribution of Log-Transformed Area") +
  xlab("Log(Area)") +
  ylab("Density") +
  theme_minimal()

# Combine the two plots into one output
library(gridExtra)
grid.arrange(price_plot, area_plot, ncol = 2)

# Load necessary library
library(dplyr)

# Define numerical variables to be scaled
numerical_features <- c( 'area', 'bedrooms', 'bathrooms', 'stories', 'parking', 'furnishingstatus')

# Apply StandardScaler (scale function) to numerical features in both training and test datasets
df_train[numerical_features] <- scale(df_train[numerical_features])
df_test[numerical_features] <- scale(df_test[numerical_features])

# Check the scaled data
head(df_train[numerical_features])
head(df_test[numerical_features])

print(df_train)


# Separate features and target variable using base R
X <- df_train[, !names(df_train) %in% "price"]  # Drop the 'price' column
y <- df_train$price                             # Assign 'price' column to y

# Check the dimensions of X and y
print(dim(X))  # Print dimensions of X
print(length(y))  # Print length of y

# Drop the 'price' column to create X_test
X_test <- df_test[, !names(df_test) %in% "price"]

# Extract the 'price' column to create y_test
y_test <- df_test$price


# Fit a linear regression model
lm_model <- lm(price ~ ., data = df_train)


plot(lm_model)
# Display the summary of the model
summary(lm_model)

# ANOVA
anova_results <- anova(lm_model)
print(anova_results)

lm_y_pred <- predict(lm_model, newdata = df_test)  # Use lm_model instead of lm

# Reverse the log transformation on predictions
lm_original_scale_predictions <- exp(lm_y_pred)
print(lm_y_pred)

# Calculate MSE and R² on the original scale
lm_rmse_original <- sqrt(mean((y_test - lm_original_scale_predictions)^2))
lm_r_squared_original <- cor(y_test, lm_original_scale_predictions)^2

cat("LM RMSE on original scale:", lm_rmse_original, "\n")
cat("LM R² on original scale for Ridge:", lm_r_squared_original, "\n")

#-------------------
# Mutliple Linear REgression on few variables handpicked by looking at ANOVA f-values 

# Fit a multiple linear regression model
#selected_model <- lm(price ~ area + bedrooms + bathrooms + stories + guestroom + basement  + airconditioning + prefarea + furnishingstatus, data = df_train)

#print(summary(selected_model))
#lm_y_pred_selected <- predict(selected_model, newdata = df_test)  # Use lm_model instead of lm

# Reverse the log transformation on predictions
#lm_original_scale_predictions <- exp(lm_y_pred_selected)
#print(lm_y_pred_selected)

# Calculate MSE and R² on the original scale
#lm_rmse_original <- sqrt(mean((y_test - lm_original_scale_predictions)^2))
#lm_r_squared_original <- cor(y_test, lm_original_scale_predictions)^2

#cat("selected LM RMSE on original scale:", lm_rmse_original, "\n")
#cat("selected LM R² on original scale for Ridge:", lm_r_squared_original, "\n")


# -------------------

# Fit a quadratic regression model
quadratic_model <- lm(price ~ poly(area, 2)+poly(bedrooms, 2)+poly(bathrooms, 2)+poly(stories, 2), data = df_train)

# Summary of the model
summary(quadratic_model)

lm2_y_pred <- predict(quadratic_model, newdata = df_test)  # Use lm_model instead of lm

# Reverse the log transformation on predictions
lm2_original_scale_predictions <- exp(lm2_y_pred)
print(lm2_y_pred)

# Calculate MSE and R² on the original scale
lm_rmse_original <- sqrt(mean((y_test - lm2_original_scale_predictions)^2))
lm_r_squared_original <- cor(y_test, lm2_original_scale_predictions)^2

cat("LM2 RMSE on original scale:", lm_rmse_original, "\n")
cat("LM2 R² on original scale for Ridge:", lm_r_squared_original, "\n")



# -------------------------

# Fit a quadratic regression model
polynomial_model <- lm(price ~ poly(area, 3)+poly(bedrooms, 3)+poly(stories, 3), data = df_train)

# Summary of the model
summary(polynomial_model)

lm3_y_pred <- predict(polynomial_model, newdata = df_test)  # Use lm_model instead of lm

# Reverse the log transformation on predictions
lm3_original_scale_predictions <- exp(lm3_y_pred)
print(lm3_y_pred)

# Calculate MSE and R² on the original scale
lm3_rmse_original <- sqrt(mean((y_test - lm3_original_scale_predictions)^2))
lm3_r_squared_original <- cor(y_test, lm3_original_scale_predictions)^2

cat("LM2 RMSE on original scale:", lm3_rmse_original, "\n")
cat("LM2 R² on original scale for Ridge:", lm3_r_squared_original, "\n")

# ---------------------------
# Load the necessary library
library(glmnet)

# Prepare the data
X_matrix <- as.matrix(X)  
y_vector <- as.vector(y)  

# Perform cross-validation to find the best lambda for Ridge regression (alpha = 0 for Ridge)
cv_ridge <- cv.glmnet(X_matrix, y_vector, alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
cat("Best lambda for Ridge:", best_lambda_ridge, "\n")

# Fit the Ridge model with the best lambda
ridge_model_best <- glmnet(X_matrix, y_vector, alpha = 0, lambda = best_lambda_ridge)
# Make predictions on the test data
ridge_y_pred_log <- predict(ridge_model_best, newx = as.matrix(X_test), s = best_lambda_ridge)
print(ridge_model_best)
print(coef(ridge_model_best))
ridge_original_scale_predictions <- exp(ridge_y_pred_log)
print(ridge_original_scale_predictions)


# Calculate MSE and R² on the original scale
ridge_rmse_original <- sqrt(mean((y_test - ridge_original_scale_predictions)^2))
ridge_r_squared_original <- cor(y_test, ridge_original_scale_predictions)^2

cat("Ridge RMSE on original scale:", ridge_rmse_original, "\n")
cat("Ridge R² on original scale for Ridge:", ridge_r_squared_original, "\n")



library(caret)       # For train/test split and other utilities
library(Metrics)     # For calculating MSE


# Load necessary libraries
library(ggplot2)

# Create a data frame for actual vs predicted prices
predicted_data <- data.frame(Actual = y_test, Predicted = ridge_y_pred_best)


# Ensure the column names are correctly set
names(predicted_data) <- c("Actual", "Predicted")

# Create the plot
ggplot(data = predicted_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +  # Scatter plot with transparency
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Line with slope 1 for ideal predictions
  ggtitle("Actual vs Predicted Housing Prices (Ridge Regression)") +
  xlab("Actual Prices") +
  ylab("Predicted Prices") +
  theme_minimal()  # Use a minimal theme

# -----------------

#Lasso Regressison

# Perform cross-validated Lasso regression
lasso_cv_model <- cv.glmnet(X_matrix, y_vector, alpha = 1, nfolds = 10)

# Find the best lambda value (one that minimizes cross-validation error)
best_lambda <- lasso_cv_model$lambda.min

# Fit the final Lasso model using the best lambda
lasso_model <- glmnet(X_matrix, y_vector, alpha = 1, lambda = best_lambda)

# Display the coefficients
print("Coefficients of the Lasso model:")
print(coef(lasso_model))
library(car)
print(vif(lm_model))
# Print best lambda value
cat("Best lambda for Lasso regression: ", best_lambda, "\n")

# Make predictions
lasso_y_pred <- predict(lasso_cv_model, newx = as.matrix(X_test), s = best_lambda)  # Choose an appropriate lambda (s)

lasso_original_scale_predictions <- exp(lasso_y_pred)
print(lasso_original_scale_predictions)

# Calculate MSE and R² on the original scale
lasso_rmse_original <- sqrt(mean((y_test - lasso_original_scale_predictions)^2))
lasso_r_squared_original <- cor(y_test, lasso_original_scale_predictions)^2

cat("Ridge RMSE on original scale:", lasso_rmse_original, "\n")
cat("Ridge R² on original scale for Ridge:", lasso_r_squared_original, "\n")





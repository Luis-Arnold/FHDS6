# -------------------------------------
#           SET WORK DIRECTORY
# -------------------------------------

# This directory is in my laptop, therefore it will be acces in here
setwd("D:/OneDrive - FHNW/Data Science/Final_Exam")

# -------------------------------------
#           INSTALL LIBRARY
# -------------------------------------

# Installing package
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("leaps", quietly = TRUE)) {
  install.packages("leaps")
}
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")
}
if (!requireNamespace("xgboost", quietly = TRUE)) {
  install.packages("xgboost")
}

# Call out package
library(plotly)
library(corrplot)
library(readr)
library(caret)
library(leaps)
library(tidyr)
library(dplyr)
library(randomForest)
library(ggplot2)
library(car)
library(parallel)
library(xgboost)

# -------------------------------------
#             INPUT DATA
# -------------------------------------

#loan_data = Assignment_1_Dataset

# Input Dataset 1
loan_data <- read_delim("Assignment_1-Dataset.csv",
                        delim = ";",
                        show_col_types = FALSE,
                        trim_ws = TRUE)

# -------------------------------------
#         OMIT SOME COLUMNS
# -------------------------------------

# Omit some non-related columns
df_1 <- subset(loan_data, select = -c(collection_recovery_fee,
                                      installment,
                                      funded_amnt,
                                      funded_amnt_inv,
                                      issue_d,
                                      last_pymnt_amnt,
                                      last_pymnt_d,
                                      loan_status,
                                      next_pymnt_d,
                                      out_prncp,
                                      out_prncp_inv,
                                      pymnt_plan,
                                      recoveries,
                                      total_pymnt,
                                      total_pymnt_inv,
                                      total_rec_int,
                                      total_rec_late_fee,
                                      total_rec_prncp,
                                      url,
                                      desc))


# -------------------------------------
#           DATA TYPE DIVIDER
# -------------------------------------

# Get the data types of each column
column_types <- sapply(df_1, class)

# Unique data types
unique_types <- unique(column_types)

# Automatically create a data frame for each data type
for (type in unique_types) {
  # Create a dynamic variable name for each data type
  df_type_name <- paste0("df_", gsub(" ", "_", type))  # Replace spaces with underscores
  # Subset the columns that match the current type and assign to the data frame
  assign(df_type_name, df_1[, column_types == type, drop = FALSE])
  # Optionally print the names of the created data frames
  print(paste("Data frame created for type:", df_type_name))
}

# Clean up unnecessary variables
rm(column_types, unique_types, type, df_type_name)

# -------------------------------------
#     TIDY UP CHARACTER / TEXT
# -------------------------------------

# Columns to omit
chr_omit <- c("emp_title", "verification_status", "title", 
                     "initial_list_status", "last_credit_pull_d", 
                     "earliest_cr_line", "verification_status_joint")

# Create df_character_2 by omitting the specified columns
df_character_2 <- df_character[, !(names(df_character) %in% chr_omit)]

# Convert character columns in df_character_2 to factors
df_character_3 <- data.frame(lapply(df_character_2, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}), stringsAsFactors = FALSE)  # Use stringsAsFactors = FALSE to avoid unwanted behavior

# Optionally print the structure of the new data frame to verify the changes
str(df_character_3)

# Deleting Variable
rm(chr_omit)

# -------------------------------------
#             TIDY UP NUMERIC
# -------------------------------------

# Checking some of numeric
## Annual_Income | Checking percentage
cat(round(mean(is.na(df_numeric$annual_inc)) * 100, 2), "%")
## Annual_Income_Joint | Checking percentage
cat(round(mean(is.na(df_numeric$annual_inc_joint)) * 100, 2), "%")
unique(df_numeric$annual_inc_joint)
## Open il 6m | Checking percentage
cat(round(mean(is.na(df_numeric$open_il_6m)) * 100, 2), "%")

# Seeing Missing Data
colSums(is.na(df_numeric))
# Data Mutating
## Assume `df` is your data frame
df_numeric_2 <- df_numeric %>%
  # Impute with mean
  mutate(
    open_acc = ifelse(is.na(open_acc), mean(open_acc, na.rm = TRUE), open_acc),
    total_acc = ifelse(is.na(total_acc), mean(total_acc, na.rm = TRUE), total_acc),
    annual_inc = ifelse(is.na(annual_inc), mean(annual_inc, na.rm = TRUE), annual_inc),
    revol_util = ifelse(is.na(revol_util), mean(revol_util, na.rm = TRUE), revol_util)
  ) %>%
  # Impute with 0
  mutate_at(vars(collections_12_mths_ex_med, delinq_2yrs, inq_last_6mths,
                 mths_since_last_record, revol_bal, open_acc_6m, open_il_6m,
                 open_il_12m, open_il_24m, mths_since_rcnt_il, total_bal_il,
                 inq_last_12m, acc_now_delinq, tot_coll_amt, tot_cur_bal,
                 annual_inc_joint, dti_joint, mths_since_last_delinq,
                 mths_since_last_major_derog, pub_rec, il_util,
                 open_rv_12m, open_rv_24m, max_bal_bc, all_util,
                 total_rev_hi_lim, inq_fi, total_cu_tl),
            ~ ifelse(is.na(.), 0, .))
## Checking Na or missing value
colSums(is.na(df_numeric_2))

# Clearer Image for Correlation
## Save as PNG with larger dimensions and correlation numbers inside the squares
png("correlation_plot.png", width = 1720, height = 1720)
## Generate the correlation plot with numbers inside the squares
corrplot(cor(df_numeric_2), 
         method = "color",          # Use color for the squares
         tl.col = "black",          # Text label color
         tl.srt = 45,               # Rotate the text labels at 45 degrees
         addCoef.col = "black")      # Add correlation numbers inside the squares with black text
dev.off()  # Save and close the PNG file

# Omit some columns making new dataframe
df_numeric_3 <- df_numeric_2[, c("int_rate","loan_amnt", "annual_inc", "dti", 
                                        "delinq_2yrs","mths_since_last_record",
                                        "inq_last_6mths", "revol_util", 
                                        "tot_cur_bal", "total_rev_hi_lim")]

# -------------------------------------
#         COMBINING DATAFRAME
# -------------------------------------

# Combining the Dataset (dataframe)
df_2 <- cbind(df_numeric_3, df_character_3)

# Omit zip code, because we have addr_state
df_2 <- df_2[, !(names(df_2) %in% c("zip_code"))]

# Seeing again correlation test to see the correlation
## Identify factor columns in df_2
factor_cols <- sapply(df_2, is.factor)
## Create dummy variables for the factor columns
dummy_vars <- model.matrix(~ . - 1, data = df_2[, factor_cols])  # Exclude the intercept
## Combine dummy variables with the original data frame (excluding factor columns)
df_dummies <- cbind(df_2[, !factor_cols], dummy_vars)  # Keep non-factor columns and add dummy variables
## Delete variable
rm(dummy_vars)
rm(factor_cols)

# Corelation test
## Calculate the correlation matrix
correlation_matrix <- cor(df_dummies, use = "pairwise.complete.obs")  # Use pairwise complete observations
## View the correlation matrix
View(correlation_matrix)
## Save the correlation plot as a PNG
png("correlation_plot_2.png", width = 1800, height = 1800)
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.srt = 45)
dev.off()  # Close the file
## df_dummies to df_3
df_3 <- df_dummies

# -------------------------------------
#             OUTLIERS
# -------------------------------------

# Let us check the outlier for each numerical column in the dataframe
## Convert df_3 to long format for easier faceting
df_long <- df_3 %>%
  select(loan_amnt, annual_inc, dti, delinq_2yrs, mths_since_last_record,
         inq_last_6mths, revol_util, tot_cur_bal, total_rev_hi_lim) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
## Create combined boxplot with faceting
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplots of Numeric Columns", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3)

# Based on the picture, it looks like only "loan_amnt" that not having outliers
# lets take out the outliers

# Some data we need to capping the outlier and other we can remove the outlier
# Based on the visualizer The one that will be remove is revol_util, total_rev_hi_lim, and dti
# Based on the visualizer The one that will be capping is annual_inc, delinq_2yrs and inq_last_6mths

# Helper function to calculate IQR bounds
get_iqr_bounds <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(c(lower_bound, upper_bound))
}
# Step 1: Create `df_3_no_outliers` by removing outliers for specified columns
df_3_no_outliers <- df_3
for (col in c("revol_util", "total_rev_hi_lim", "dti")) {
  bounds <- get_iqr_bounds(df_3_no_outliers[[col]])
  df_3_no_outliers <- df_3_no_outliers[df_3_no_outliers[[col]] >= bounds[1] & 
                                         df_3_no_outliers[[col]] <= bounds[2], ]
}
# Step 2: Create `df_3_capped` by capping outliers for specified columns
df_3_capped <- df_3_no_outliers  # Start from `df_3_no_outliers` for consistency
for (col in c("annual_inc", "delinq_2yrs", "inq_last_6mths")) {
  bounds <- get_iqr_bounds(df_3_capped[[col]])
  df_3_capped[[col]] <- ifelse(df_3_capped[[col]] < bounds[1], bounds[1],
                               ifelse(df_3_capped[[col]] > bounds[2], bounds[2], df_3_capped[[col]]))
}
# Step 3: Assign the final result to `df_4` and remove unnevessary dataframe
df_4 <- df_3_capped
rm(df_3_capped,df_3_no_outliers)

# lets check the visulalizer again
## Convert df_4 to long format for easier faceting
df_long <- df_4 %>%
  select(loan_amnt, annual_inc, dti, delinq_2yrs, mths_since_last_record,
         inq_last_6mths, revol_util, tot_cur_bal, total_rev_hi_lim) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
## Create combined boxplot with faceting
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplots of Numeric Columns", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ variable, scales = "free_y", ncol = 3)

# Based on the second picture, it will be best, if we just not use tot_cur_bal and mths_since_last_record
df_5 <- df_4 %>% select(-tot_cur_bal, -mths_since_last_record)

# -------------------------------------
#     CROSS VALIDATION (K-METHOD)
# -------------------------------------

# Getting DF for Linear Regression Model
df <- df_5

# Sample data (replace with your actual df)
set.seed(1)  # Set seed for reproducibility

# Removing the 'term60 months' column if it exists
df <- subset(df, select = -`term60 months`)

# Split df into 90% training and 10% validation
train_index <- sample(1:nrow(df), 0.9 * nrow(df))  # Get 90% of row indices for training
train_data <- df[train_index, ]  # Training data
validation_data <- df[-train_index, ]  # Validation data

# Set up cross-validation control (e.g., 10-fold)
control <- trainControl(method = "cv", number = 10)

# Train the Linear Model using cross-validation
# model <- train(int_rate ~ ., data = train_data, method = "lm", trControl = control)

# Checking model
# summary(model)
# IT looks like delinq_2yrs having an NA moment, we can exclude it
df <- subset(df, select = -delinq_2yrs)

# Split df into 90% training and 10% validation
train_index <- sample(1:nrow(df), 0.9 * nrow(df))  # Get 90% of row indices for training
train_data <- df[train_index, ]  # Training data
validation_data <- df[-train_index, ]  # Validation data

# Train the Linear Model using cross-validation
model <- train(int_rate ~ ., data = train_data, method = "lm", trControl = control)

# Calculate evaluation metrics on the validation set using predictions
predictions <- predict(model, newdata = validation_data)

# Calculate Evaluation Metrics
## Mean Squared Error (MSE)
mse <- mean((validation_data$int_rate - predictions)^2)
mse


# -------------------------------------
# CHECKING MULTICOLINIEARITY AGAIN
# -------------------------------------

# There is this warning message:
# Warning message:
#   In predict.lm(modelFit, newdata) :
#   prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
# There is perhaps something to do with the multicoloniarity

# Checking multicollinearity
final_model <- model$finalModel
vif(final_model)

# In this case:
  
# Some variables like home_ownershipMORTGAGE,
# home_ownershipOWN, and home_ownershipRENT
# have very high VIF values (e.g., 1.7e+05, 1.65e+05).
# These very high values likely indicate that the variables are highly correlated
# with each other, which could lead to multicollinearity problems.

# The other predictors like loan_amnt, annual_inc, dti, inq_last_6mths,
# and revol_util have VIF values around 1, indicating no multicollinearity.

# Let see again the cor matrix
# USing plotly so you can zoom in and out easly for the plot

# Convert correlation matrix to a long format for plotly
cor_matrix <- cor(df_4)
cor_matrix_long <- as.data.frame(as.table(cor_matrix))

# Create an interactive heatmap
plot_ly(data = cor_matrix_long, x = ~Var1, y = ~Var2, z = ~Freq, 
        type = "heatmap", colors = colorRampPalette(c("blue", "white", "red"))(100)) %>%
  layout(title = "Correlation Matrix", xaxis = list(title = ""), 
         yaxis = list(title = ""))
# You can zoom in and out easly and you can make the things bigger or smaller

# Based on the picture, we can say that these are the variable having multicollinearity
# homeowenrship_RENT -- home_ownershipMORTGAGE
# purposedebt_consolidation -- purposecredit_card
# We can try to delete one of them
# in here these are the column that we delete homeowenrship_RENT & purposedebt_consolidation
# lets take the dataframe from df_5 and put in df_6 with also remove some things

df_6 <- df_5 %>%
  select(-`term60 months`, 
         -home_ownershipRENT, 
         -purposedebt_consolidation, 
         -delinq_2yrs)


# -------------------------------------
#     CROSS VALIDATION (K-METHOD) 2.0
# -------------------------------------

# Getting DF for Linear Regression Model
df <- df_6

# Sample data (replace with your actual df)
set.seed(1)  # Set seed for reproducibility

# Split df into 90% training and 10% validation
train_index <- sample(1:nrow(df), 0.9 * nrow(df))  # Get 90% of row indices for training
train_data <- df[train_index, ]  # Training data
validation_data <- df[-train_index, ]  # Validation data

# Set up cross-validation control (e.g., 10-fold)
control <- trainControl(method = "cv", number = 10)

# Split df into 90% training and 10% validation
train_index <- sample(1:nrow(df), 0.9 * nrow(df))  # Get 90% of row indices for training
train_data <- df[train_index, ]  # Training data
validation_data <- df[-train_index, ]  # Validation data

# Train the Linear Model using cross-validation
model <- train(int_rate ~ ., data = train_data, method = "lm", trControl = control)

# Calculate evaluation metrics on the validation set using predictions
predictions <- predict(model, newdata = validation_data)

# Calculate Evaluation Metrics
## Mean Squared Error (MSE)
mse <- mean((validation_data$int_rate - predictions)^2)
mse
# [1] 11.01383


# -------------------------------------
#             RANDOM FOREST
# -------------------------------------

# Last things, Random forest,
# I kind of really exhausted

# Lets clean the column names
## Define a function to clean column names
clean_column_names <- function(data) {
  colnames(data) <- gsub(" ", "_", colnames(data))  # Replace spaces with underscores
  colnames(data) <- gsub("\\`", "", colnames(data))  # Remove backticks
  colnames(data) <- gsub("\\+", "_", colnames(data))  # Replace plus signs with underscores
  colnames(data) <- gsub("n/a", "_na", colnames(data))  # Replace 'n/a' with '_na'
  return(data)
}
## Apply the function to both datasets
train_data <- clean_column_names(train_data)
validation_data <- clean_column_names(validation_data)

# Random forest
model_rf <- randomForest(int_rate ~ ., data = train_data,
                         importance = FALSE,
                         ntree = 100,
                         mtry = 3, 
                         do.trace = TRUE, 
                         parallel = TRUE)

# Make predictions on the test data
predictions_rf <- predict(model_rf, newdata = validation_data)

# Calculate the MSE
mse_rf <- mean((predictions_rf - validation_data$int_rate)^2)
mse_rf

# -------------------------------------
#   TAKING SAMPLE STRATISFIED METHOD
# -------------------------------------

# My laptop is not good
# So, lets just take a sample
# Before taking the sample with stratisfied,
# We need to make our category references,
# I try to make it by divided the range automarically

# Divide income into 5 equal-width ranges
df_7 <- df_6 %>%
  mutate(income_range = cut(annual_inc, breaks = 5,
                            labels = c("Very Low", "Low", "Medium", "High", "Very High")))

# Lets make the sample data 25% from each income_range
df_sample <- df_7 %>%
  group_by(income_range) %>%
  sample_frac(size = 0.25) %>%
  ungroup()

# Do not forget to delete the income_range afterwards
# If dont, it will having a multicollinearity
df_sample <- subset(df_sample, select = -c(income_range))

# Lets try again, shall we?

# -------------------------------------
#          RANDOM FOREST 2
# -------------------------------------

# Split df into 90% training and 10% validation
train_index <- sample(1:nrow(df_sample), 0.9 * nrow(df_sample))  # Get 90% of row indices for training
train_data <- df_sample[train_index, ]  # Training data
validation_data <- df_sample[-train_index, ]  # Validation dat

# Lets clean the column names
## Define a function to clean column names
clean_column_names <- function(data) {
  colnames(data) <- gsub(" ", "_", colnames(data))  # Replace spaces with underscores
  colnames(data) <- gsub("\\`", "", colnames(data))  # Remove backticks
  colnames(data) <- gsub("\\+", "_", colnames(data))  # Replace plus signs with underscores
  colnames(data) <- gsub("n/a", "_na", colnames(data))  # Replace 'n/a' with '_na'
  return(data)
}
## Apply the function to both datasets
train_data <- clean_column_names(train_data)
validation_data <- clean_column_names(validation_data)

# Random forest
model_rf <- randomForest(int_rate ~ ., data = train_data,
                         importance = FALSE,
                         ntree = 500,
                         mtry = 3, 
                         do.trace = TRUE, 
                         parallel = TRUE)

# Make predictions on the test data
predictions_rf <- predict(model_rf, newdata = validation_data)

# Calculate the MSE
mse_rf <- mean((predictions_rf - validation_data$int_rate)^2)
mse_rf

# -------------------------------------
#          XGBOOST
# -------------------------------------

# Lets Make the xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_data[,-1]), label = train_data$int_rate)
model_xgb <- xgboost(data = dtrain, max_depth = 10, eta = 0.1, nrounds = 100, objective = "reg:squarederror")

# Testing the xgboost data
## Prepare the test data (convert to xgboost DMatrix)
dtest <- xgb.DMatrix(data = as.matrix(df[,-1]))  # remove target column
## Generate predictions
predictions <- predict(model_xgb, dtest)
## Calculate MSE
actuals <- df$int_rate
mse <- mean((predictions - actuals)^2)
mse

# FINALLY
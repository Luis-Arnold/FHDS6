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

# Call out package
library(corrplot)
library(readr)
library(caret)
library(leaps)
library(tidyr)
library(dplyr)
library(randomForest)

# -------------------------------------
#             INPUT DATA
# -------------------------------------

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
#             EXPERIMENT
# -------------------------------------
# Fit the linear model
linear_model <- lm(int_rate ~ ., data = df_3)
# View the summary of the linear model
summary(linear_model)




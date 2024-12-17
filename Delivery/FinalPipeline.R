setwd("C:/Users/luisj/Documents/Github/FHDS6/RawData")

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


data_path <- "C:/Users/luisj/Documents/Github/FHDS6/RawData/Assignment_1-Dataset.csv"

# Input Dataset 1
loan_data <- read_delim(data_path,
                        delim = ";",
                        show_col_types = FALSE,
                        trim_ws = TRUE)

rm(data_path)


loan_data <- subset(loan_data, select = -c(collection_recovery_fee,
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
                                           desc,
                                           emp_title,
                                           verification_status,
                                           title, 
                                           initial_list_status,
                                           last_credit_pull_d,
                                           earliest_cr_line,
                                           verification_status_joint))

column_types <- sapply(loan_data, class)

unique_types <- unique(column_types)

for (type in unique_types) {
  df_type_name <- paste0("df_", gsub(" ", "_", type))
  assign(df_type_name, loan_data[, column_types == type, drop = FALSE])
  print(paste("Data frame created for type:", df_type_name))
}

rm(column_types, unique_types, type, df_type_name)

df_character <- data.frame(lapply(df_character, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}), stringsAsFactors = FALSE)

missingCols <- names(df_numeric)[colSums(is.na(df_numeric)) > 0]


df_numeric <- df_numeric %>%
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

df_numeric <- df_numeric[, c("int_rate","loan_amnt", "annual_inc", "dti", 
                             "delinq_2yrs","mths_since_last_record",
                             "inq_last_6mths", "revol_util", 
                             "tot_cur_bal", "total_rev_hi_lim")]


df_2 <- cbind(df_numeric, df_character)

df_2 <- df_2[, !(names(df_2) %in% c("zip_code"))]

factor_cols <- sapply(df_2, is.factor)

dummy_vars <- model.matrix(~ . - 1, data = df_2[, factor_cols])

df_3 <- cbind(df_2[, !factor_cols], dummy_vars)

get_iqr_bounds <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(c(lower_bound, upper_bound))
}

df_3_no_outliers <- df_3
for (col in c("revol_util", "total_rev_hi_lim", "dti")) {
  bounds <- get_iqr_bounds(df_3_no_outliers[[col]])
  df_3_no_outliers <- df_3_no_outliers[df_3_no_outliers[[col]] >= bounds[1] & 
                                         df_3_no_outliers[[col]] <= bounds[2], ]
}

df_3_capped <- df_3_no_outliers  # Start from `df_3_no_outliers` for consistency
for (col in c("annual_inc", "delinq_2yrs", "inq_last_6mths")) {
  bounds <- get_iqr_bounds(df_3_capped[[col]])
  df_3_capped[[col]] <- ifelse(df_3_capped[[col]] < bounds[1], bounds[1],
                               ifelse(df_3_capped[[col]] > bounds[2], bounds[2], df_3_capped[[col]]))
}

df_4 <- df_3_capped
rm(df_3_capped,df_3_no_outliers)

df <- df_4 %>% select(-tot_cur_bal, -mths_since_last_record)

df_6 <- df %>%
  select(-`term60 months`, 
         -home_ownershipRENT, 
         -purposedebt_consolidation, 
         -delinq_2yrs)

train_data <- df_6

clean_column_names <- function(data) {
  colnames(data) <- gsub(" ", "_", colnames(data))
  colnames(data) <- gsub("\\`", "", colnames(data))
  colnames(data) <- gsub("\\+", "_", colnames(data))
  colnames(data) <- gsub("n/a", "_na", colnames(data))
  return(data)
}

train_data <- clean_column_names(train_data)

# Lets Make the xgboost
dtrain <- xgb.DMatrix(data = as.matrix(train_data[,-1]), label = train_data$int_rate)
model_xgb <- xgboost(data = dtrain, max_depth = 10, eta = 0.1, nrounds = 100, objective = "reg:squarederror")

xgb.save(model_xgb, "xgb_model.bin")

# Testing the xgboost data
## Prepare the test data (convert to xgboost DMatrix)
dtest <- xgb.DMatrix(data = as.matrix(df[,-1]))  # remove target column
## Generate predictions
predictions <- predict(model_xgb, dtest)
## Calculate MSE
actuals <- df$int_rate
mse <- mean((predictions - actuals)^2)
mse










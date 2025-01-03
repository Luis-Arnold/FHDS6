
# This script is the final executable which makes use of our trained model.
# First, some preprocessing is done.
# Then, the our model is used to make predictions.
# Only the final MSE is printed out.

# Set your working directory here:
setwd("your_working_directory")

if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")
}
if (!requireNamespace("xgboost", quietly = TRUE)) {
  install.packages("xgboost")
}

library(readr)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(parallel)
library(xgboost)


# set your dataset here:
data_path <- "path_to_your_dataset"

# -----------------------------PREPROCESSING_START---------------------------------------

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

df_united <- cbind(df_numeric, df_character)

df_united <- df_united[, !(names(df_united) %in% c("zip_code"))]

factor_cols <- sapply(df_united, is.factor)

dummy_vars <- model.matrix(~ . - 1, data = df_united[, factor_cols])

df_united <- cbind(df_united[, !factor_cols], dummy_vars)

get_iqr_bounds <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(c(lower_bound, upper_bound))
}

df_united_no_outliers <- df_united
for (col in c("revol_util", "total_rev_hi_lim", "dti")) {
  bounds <- get_iqr_bounds(df_united_no_outliers[[col]])
  df_united_no_outliers <- df_united_no_outliers[df_united_no_outliers[[col]] >= bounds[1] & 
                                                   df_united_no_outliers[[col]] <= bounds[2], ]
}

df_united_capped <- df_united_no_outliers  # Start from `df_united_no_outliers` for consistency
for (col in c("annual_inc", "delinq_2yrs", "inq_last_6mths")) {
  bounds <- get_iqr_bounds(df_united_capped[[col]])
  df_united_capped[[col]] <- ifelse(df_united_capped[[col]] < bounds[1], bounds[1],
                               ifelse(df_united_capped[[col]] > bounds[2], bounds[2], df_united_capped[[col]]))
}

df_united <- df_united_capped
rm(df_united_capped,df_united_no_outliers)

df_united <- df_united %>% select(-tot_cur_bal, -mths_since_last_record)

df_united <- df_united %>%
  select(-`term60 months`, 
         -home_ownershipRENT, 
         -purposedebt_consolidation, 
         -delinq_2yrs)

secret_data <- df_united


secret_data <- secret_data %>% select(-int_rate)

secret_data_matrix = xgb.DMatrix(data = as.matrix(secret_data))

# -----------------------------PREPROCESSING_END---------------------------------------

# loading model
model_xgb <- readRDS("model_xgb.rds")

## Generate predictions
predictions <- predict(model_xgb, secret_data_matrix)
## Calculate MSE
actuals <- df_united$int_rate
mse <- mean((predictions - actuals)^2)
mse




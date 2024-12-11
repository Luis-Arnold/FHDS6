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
if (!requireNamespace("fastDummies", quietly = TRUE)) {
  install.packages("fastDummies")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("lattice", quietly = TRUE)) {
  install.packages("lattice")
}
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
if (!requireNamespace("keras", quietly = TRUE)) {
  install.packages("keras")
}
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
if (!requireNamespace("neuralnet", quietly = TRUE)) {
  install.packages("neuralnet")
}
if (!requireNamespace("future.apply", quietly = TRUE)) {
  install.packages("future.apply")
}
if (!requireNamespace("purrr", quietly = TRUE)) {
  install.packages("purrr")
}

# Call out package
library(fastDummies)
library(readr)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(keras)
library(writexl)
library(neuralnet)
library(purrr)
library(future.apply)

# -------------------------------------
#             INPUT DATA
# -------------------------------------

# Input Dataset 2
df_1 <- read_csv("D:/OneDrive - FHNW/Data Science/Final_Exam/Assignment_2-Dataset.csv")

# -------------------------------------
#         EXPLORATORY DATA
# -------------------------------------

# Head checking data
head(df_1)
View(df_1)

# CHecking NAs
colSums(is.na(df_1))

# Taking NAs to another DataFrame
# Just checking what things that we can see
df_na <- df_1 %>% filter(is.na(OCCUPATION_TYPE))

# -------------------------------------
#           CLEANING NA
# -------------------------------------

# It seems that the one with NAs have something to do with the days_employed
# If positive, it means the person unemployed
# Changing Days_employed as numeric
# it is because the day employeed is the one can tell he Unemploymend
# When looking to income type, we see that the "income type" having a clue
# We can use it as a reference to do the NAs
# Now, when we are looking for the "income type" 
# THere is a "student" we can make the occupation type students
# Then we apply it to df_1 turn it into df_2
df_2 <- df_1 %>%
  mutate(
    DAYS_EMPLOYED = as.numeric(DAYS_EMPLOYED),
    OCCUPATION_TYPE = case_when(
      DAYS_EMPLOYED == 365243 ~ "Unemployed",
      NAME_INCOME_TYPE == "Pensioner" ~ "Pensionary",
      NAME_INCOME_TYPE == "Student" ~ "Student",
      DAYS_EMPLOYED < 0 & is.na(OCCUPATION_TYPE) ~ "Unspecified",
      TRUE ~ OCCUPATION_TYPE
    )
  )
rm(df_na)

# -------------------------------------
#      DIFFERNTIATE DATA TYPE
# -------------------------------------

# Dataset with numeric columns
df_numeric <- df_2 %>% select_if(is.numeric)

# Dataset with character columns and adding the ID column
df_character <- df_2 %>% select(ID, where(is.character))

# Checking all
str(df_numeric)
str(df_character)

# -------------------------------------
#         MODIFYING DATA
# -------------------------------------

# This section following the instruction,
# On how it supposed to be
# Remove NAs
# All values should be numeric
# – Floating points preferred
# – Integers are possible (e.g. for Booleans)
# – Please note: factors are also numeric in R (in fact they are integers)
# – If ordered, then translate with scale
# – If unordered and levels are low, then use dummy_cols() ( https://cran.r-project.org/web/packages/fastDummies/vignettes/making-dummy-variables.html)
# – Text (strings, characters) needs to be translated
# All values should be scaled to values between [0,1] (resp. [-1,1])
# Heterogenous values should be normalized (ideally: mean = 0; stddev = 1)
# Feature engineering on small data; for big data trust the network J

# -------------------------------------
#     START MODIFYING THE CHARACTER
# -------------------------------------

## Lets start with three columns:
## $ CODE_GENDER        : chr  "M" "F" "F" "F" ...
## $ FLAG_OWN_CAR       : chr  "Y" "N" "N" "N" ...
## $ FLAG_OWN_REALTY    : chr  "Y" "Y" "Y" "N" ...

# Checking the uniqueness
lapply(df_character[, c("CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY")], unique)

# Changing the other things
mapping <- c("M" = 1, "F" = 0, "Y" = 1, "N" = 0)
df_character$CODE_GENDER <- as.integer(mapping[df_character$CODE_GENDER])
df_character$FLAG_OWN_CAR <- as.integer(mapping[df_character$FLAG_OWN_CAR])
df_character$FLAG_OWN_REALTY <- as.integer(mapping[df_character$FLAG_OWN_REALTY])
rm(mapping)

# Checking again the structure
str(df_character)

# Because Gender is "nominal" we need make it in one hot encoding
df_character <- df_character %>%
  mutate(
    Gender_M = ifelse(CODE_GENDER == 1, 1, 0),
    Gender_F = ifelse(CODE_GENDER == 0, 1, 0)
  )

# Making the gender into the first column
df_character <- df_character %>%
  relocate(Gender_M, Gender_F, .after = ID )

# Further down the line, we have to see the structure again
str(df_character)

# Now we can just make the remaining Character into a one hot encoding
## Perform one-hot encoding
df_encoded <- df_character %>%
  dummy_cols(select_columns = c("NAME_INCOME_TYPE",
                                "NAME_EDUCATION_TYPE",
                                "NAME_FAMILY_STATUS",
                                "NAME_HOUSING_TYPE",
                                "OCCUPATION_TYPE"))

# Checking again the things
str(df_encoded)

# Let us delete the one that not necessary
df_encoded_2 <- df_encoded[, !(names(df_encoded) %in% c("NAME_INCOME_TYPE",
                                                        "NAME_EDUCATION_TYPE",
                                                        "NAME_FAMILY_STATUS",
                                                        "NAME_HOUSING_TYPE",
                                                        "OCCUPATION_TYPE"))]
str(df_encoded_2)

# Put Status into the end of the column
df_encoded_2 <- df_encoded_2 %>%
  select(-status, everything(), status)

# Now all good, we can continue to the numeric one

# -------------------------------------
#     START MODIFYING THE NUMERIC
# -------------------------------------

# CHecking the Structure
str(df_numeric)

# Now lets take care of the four columns,
# Then we will make it into groups
# $ CNT_CHILDREN    : num [1:67614] 2 0 0 1 0 0 1 0 0 0 ...
# $ AMT_INCOME_TOTAL: num [1:67614] 81000 225000 54000 157500 81000 ...
# $ DAYS_BIRTH      : num [1:67614] -18416 -13964 -15328 -14850 -23191 ...
# $ DAYS_EMPLOYED   : num [1:67614] 365243 -664 -855 -1416 365243 ...
# $ CNT_FAM_MEMBERS : num [1:67614] 4 1 2 3 1 2 3 2 2 2 ...

# Let us go to the Days Birth
# Days birth will be divided by 365 to define the year
df_numeric$AGE <- abs(df_numeric$DAYS_BIRTH) / 365

# Days employed Amount
# How many days, they are employed or works
df_numeric$DAYS_EMPLOYED_AMOUNT <- ifelse(df_numeric$DAYS_EMPLOYED < 0, 
                                             abs(df_numeric$DAYS_EMPLOYED), 
                                             0)

# Let us omit the one that we no longer need
df_numeric_2 <- subset(df_numeric, select = -c(DAYS_EMPLOYED, DAYS_BIRTH))
str(df_numeric_2)

# now we i want to go put the flag into the df encoded 2
# because it is not actually a numeric
df_3 <- merge(df_encoded_2, df_numeric_2[, c("ID", "FLAG_MOBIL", "FLAG_PHONE", "FLAG_WORK_PHONE", "FLAG_EMAIL")], by = "ID", all.x = TRUE)

# for encoded
df_3 <- df_3 %>%
  select(-status, everything(), status)

# Lets delete some column
df_numeric_2 <- df_numeric_2[, !(names(df_numeric_2) %in% c("FLAG_MOBIL", "FLAG_PHONE", "FLAG_WORK_PHONE", "FLAG_EMAIL"))]

# Standardize the table from the column
df_numeric_3 <- df_numeric_2 %>%
  mutate(across(-ID, ~ (. - min(.)) / (max(.) - min(.)), .names = "{.col}_norm"))

# -------------------------------------
#     COMBINING THE DATA
# -------------------------------------

df_4 <- merge(df_3, df_numeric_3[, c("ID",
                                             "CNT_CHILDREN_norm",
                                             "AMT_INCOME_TOTAL_norm",
                                             "CNT_FAM_MEMBERS_norm",
                                             "AGE_norm",
                                     "DAYS_EMPLOYED_AMOUNT_norm")],
              by = "ID", all.x = TRUE)

df_4 <- df_4 %>%
  select(-status, everything(), status)
str(df_4)

# Let us output the data
write_xlsx(df_4, "D:\\Software\\Github\\FHDS6\\Scripts\\AzulJohn\\clean_df_assignment_2_2.xlsx")

# -------------------------------------
#       FEED FORWARDS NETWORK
# -------------------------------------

# STEP 1
## Create a new dataframe to avoid modifying df_3 (now called df_main)
df_main <- df_4
## Exclude the ID column
df_main <- df_main %>% select(-ID)
## Convert 'status' to a factor
df_main$status <- as.factor(df_main$status)
## Split into training and testing sets
set.seed(123)
indices <- sample(1:nrow(df_main), size = 0.8 * nrow(df_main))
train_data <- df_main[indices, ]
test_data <- df_main[-indices, ]
## One Hot Encoding
status_one_hot <- model.matrix(~ status - 1, data = df_main)
## Ensure the target is one-hot encoded for training
train_y <- status_one_hot[indices, ]
test_y <- status_one_hot[-indices, ]
## Update your input features (X) without the status column
train_x <- as.matrix(train_data[, -which(names(train_data) == "status")])
test_x <- as.matrix(test_data[, -which(names(test_data) == "status")])

# STEP 2
## Modelling
model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 8, activation = "softmax")  # Output layer with 8 units for 8 clas
## Compiling
model %>% compile(
  loss = "categorical_crossentropy",  # For multi-class classification
  optimizer = "adam",
  metrics = c("accuracy")
)
## Train the Model
history <- model %>% fit(
  x = train_x,  # Input features
  y = train_y,  # One-hot encoded target variable
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# -------------------------------------
#     ACCCURACY IN TEST DATA
# -------------------------------------

# Assuming test_x and test_y are your test features and labels
result <- model %>% evaluate(test_x, test_y)
# Print the result
cat("Test loss: ", result[1], "\n")
cat("Test accuracy: ", result[2], "\n")

# -------------------------------------
#             EXPERIMENT
# -------------------------------------

model_2 <- keras_model_sequential() %>%
  layer_dense(units = 32, input_shape = ncol(train_x)) %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_dense(units = 64) %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_dense(units = 128, kernel_regularizer = regularizer_l2(0.001)) %>%
  layer_dense(units = 128) %>%
  layer_batch_normalization() %>%
  layer_activation("relu") %>%
  layer_dense(units = 8, activation = "softmax")

model_2 %>% compile(
  optimizer = optimizer_adam(learning_rate = 0.001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

history <- model_2 %>% fit(
  train_x, train_y,
  epochs = 300,
  batch_size = 32,
  validation_split = 0.2,
  callbacks = list(
    callback_early_stopping(monitor = "val_loss", patience = 10),
    callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 5)
  )
)

# Assuming test_x and test_y are your test features and labels
result_2 <- model_2 %>% evaluate(test_x, test_y)
# Print the result
cat("Test loss: ", result_2[1], "\n")
cat("Test accuracy: ", result_2[2], "\n")

# -------------------------------------
#             EXPERIMENT 2
# -------------------------------------

# Parameters
k <- 10 # Number of folds
epochs <- 250
batch_size <- 32

# Assuming train_x and train_y are your training features and labels
# Ensure they are matrices or data frames
train_x <- as.matrix(train_x)
train_y <- as.matrix(train_y)

# Generate folds
folds <- createFolds(seq_len(nrow(train_x)), k = k, list = TRUE)

# To store results
all_results <- list()

# Loop through each fold
for (i in seq_along(folds)) {
  
  cat("Running Fold", i, "...\n")
  
  # Get indices for this fold
  val_indices <- folds[[i]] # Validation indices
  train_indices <- setdiff(seq_len(nrow(train_x)), val_indices) # Training indices
  
  # Split the data for the current fold
  train_x_fold <- train_x[train_indices, , drop = FALSE] # Ensure it's a matrix/data frame
  train_y_fold <- train_y[train_indices, , drop = FALSE]
  val_x_fold <- train_x[val_indices, , drop = FALSE]
  val_y_fold <- train_y[val_indices, , drop = FALSE]
  
  # Define the model
  model_3 <- keras_model_sequential() %>%
    layer_dense(units = 32, input_shape = ncol(train_x)) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dense(units = 64) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dense(units = 128, kernel_regularizer = regularizer_l2(0.001)) %>%
    layer_dense(units = 128) %>%
    layer_batch_normalization() %>%
    layer_activation("relu") %>%
    layer_dense(units = 8, activation = "softmax")
  
  # Compile the model
  model_3 %>% compile(
    optimizer = optimizer_adam(learning_rate = 0.001),
    loss = "categorical_crossentropy",
    metrics = c("accuracy")
  )
  
  # Train the model on this fold
  history <- model_3 %>% fit(
    train_x_fold, train_y_fold,
    epochs = epochs,
    batch_size = batch_size,
    validation_data = list(val_x_fold, val_y_fold),
    callbacks = list(
      callback_early_stopping(monitor = "val_loss", patience = 10),
      callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 5)
    )
  )
  
  # Evaluate the model on validation data
  val_result <- model_3 %>% evaluate(val_x_fold, val_y_fold)
  
  # Store the validation result for this fold
  all_results[[i]] <- val_result
}

# Aggregate results: Collect loss and accuracy for each fold
all_losses <- sapply(all_results, function(x) x[[1]]) # Extract loss values
all_accuracies <- sapply(all_results, function(x) x[[2]]) # Extract accuracy values

# Print mean results
cat("Mean Validation Loss: ", mean(all_losses), "\n")
cat("Mean Validation Accuracy: ", mean(all_accuracies), "\n")

# -------------------------------------
#       SAVING THE MODEL
# -------------------------------------

save_model_hdf5(model, "D:/OneDrive - FHNW/Data Science/Final_Exam/nn_model_1.rds")
save_model_hdf5(model_2, "D:/OneDrive - FHNW/Data Science/Final_Exam/nn_model_2.rds")
save_model_hdf5(model_3, "D:/OneDrive - FHNW/Data Science/Final_Exam/nn_model_3.rds")


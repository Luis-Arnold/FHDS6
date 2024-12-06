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

# -------------------------------------
#             INPUT DATA
# -------------------------------------

# Input Dataset 2
df_0 <- read_csv("D:/OneDrive - FHNW/Data Science/Final_Exam/Assignment_2-Dataset.csv")
df_1 <- df_0

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
df_na$DAYS_EMPLOYED<- as.numeric(df_na$DAYS_EMPLOYED)
df_na <- df_na %>% mutate(OCCUPATION_TYPE = ifelse(DAYS_EMPLOYED == 365243, "Unemployed", "Unspecified"))
# it is because the day employeed is the one can tell he Unemploymend

# When looking to income type, we see that the "income type" having a clue
# We can use it as a reference to do the NAs
df_na <- df_na %>% mutate(OCCUPATION_TYPE = ifelse(NAME_INCOME_TYPE == "Pensioner", "Pensionary", OCCUPATION_TYPE))

# Now, when we are looking for the "income type" 
# THere is a "student" we can mae the occupation type students
df_na <- df_na %>% mutate(OCCUPATION_TYPE = ifelse(NAME_INCOME_TYPE == "Student", "Student", OCCUPATION_TYPE))

# THen we apply it to df_1 turn it into df_2
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

# To delete the experiment DataFrame 
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

# Checking again the structure
str(df_character)

# Because Gender is "ordinal" we need make it in one hot encoding
df_character <- df_character %>%
  mutate(
    Gender_M = ifelse(CODE_GENDER == 1, 1, 0),
    Gender_F = ifelse(CODE_GENDER == 0, 1, 0)
  )

# Making the gender into the first column
df_character <- df_character %>%
  relocate(Gender_M, Gender_F, .before = 1)

# Further down the line, we have to see how much the uniqueness
unique_counts <- df_character %>%
  summarise(across(everything(), ~ n_distinct(.)))
View(unique_counts)

# Now we can just make the remaining Character into a one hot encoding
## Perform one-hot encoding
df_encoded <- df_character %>%
  dummy_cols(select_columns = c("NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE"))

# Checking again the things
str(df_encoded)

# Let us delete the one that not necessary
df_encoded_2 <- df_encoded[, !(names(df_encoded) %in% c("NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE"))]
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

df_numeric_2 <- df_numeric %>%
  # 1. Categorize 'CNT_CHILDREN' into ranges
  mutate(CNT_CHILDREN = case_when(
    CNT_CHILDREN == 0 ~ "NO_CHILDREN",
    CNT_CHILDREN == 1 ~ "1_CHILD",
    CNT_CHILDREN == 2 ~ "2_CHILDREN",
    CNT_CHILDREN >= 3 ~ "3_OR_MORE_CHILDREN"
  )) %>%
  
  # 2. Categorize 'AMT_INCOME_TOTAL' into income brackets
  mutate(AMT_INCOME_TOTAL = case_when(
    AMT_INCOME_TOTAL < 50000 ~ "LOW_INCOME",
    AMT_INCOME_TOTAL >= 50000 & AMT_INCOME_TOTAL < 150000 ~ "MEDIUM_INCOME",
    AMT_INCOME_TOTAL >= 150000 ~ "HIGH_INCOME"
  )) %>%
  
  # 3. Convert 'DAYS_BIRTH' to age (positive value)
  mutate(AGE = abs(DAYS_BIRTH) / 365,
         AGE_CATEGORY = case_when(
           AGE < 25 ~ "YOUNG",
           AGE >= 25 & AGE < 45 ~ "MIDDLE_AGED",
           AGE >= 45 & AGE < 65 ~ "SENIOR",
           AGE >= 65 ~ "ELDERLY"
         )) %>%
  
  # 4. Convert 'DAYS_EMPLOYED' to number of days employed or NA if positive (unemployed)
  mutate(DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED < 0, abs(DAYS_EMPLOYED), NA),
         EMPLOYMENT_CATEGORY = case_when(
           is.na(DAYS_EMPLOYED) ~ "UNEMPLOYED",
           DAYS_EMPLOYED <= 365 ~ "RECENTLY_EMPLOYED",
           DAYS_EMPLOYED > 365 & DAYS_EMPLOYED <= 1825 ~ "EMPLOYED_1_5_YEARS",
           DAYS_EMPLOYED > 1825 ~ "EMPLOYED_5_PLUS_YEARS"
         )) %>%
  
  # 5. Categorize 'CNT_FAM_MEMBERS' into family size ranges
  mutate(CNT_FAM_MEMBERS = case_when(
    CNT_FAM_MEMBERS <= 2 ~ "1_2_MEMBERS",
    CNT_FAM_MEMBERS > 2 & CNT_FAM_MEMBERS <= 4 ~ "3_4_MEMBERS",
    CNT_FAM_MEMBERS > 4 ~ "5_OR_MORE_MEMBERS"
  ))

# View the structure of the final transformed dataframe
str(df_numeric_2)

# Let us omit the one that we no longer need
df_numeric_3 <- subset(df_numeric_2, select = -c(DAYS_EMPLOYED, DAYS_BIRTH, AGE))
str(df_numeric_3)

# Use dummyVars() to create one-hot encoded variables
dummies <- dummyVars(~ ., data = df_numeric_3, fullRank = TRUE)
df_numeric_4 <- predict(dummies, newdata = df_numeric_3)
df_numeric_4 <- as.data.frame(df_numeric_4)

rm(dummies)
rm(mapping)

str(df_numeric_4)

# -------------------------------------
#     COMBINING THE DATA
# -------------------------------------

df_3 <- df_numeric_4 %>% 
  left_join(df_encoded_2, by = "ID")

str(df_3)

# Let us output the data
write_xlsx(df_3, "D:\\Software\\Github\\FHDS6\\Scripts\\AzulJohn\\clean_df_assignment_2.xlsx")

# -------------------------------------
#       FEED FORWARDS NETWORK
# -------------------------------------

# STEP 1
## Create a new dataframe to avoid modifying df_3 (now called df_main)
df_main <- df_3
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
cat("Test loss: ", result[1], "\n") # Lost 0.90
cat("Test accuracy: ", result[2], "\n") # 0.80

# -------------------------------------
#            EXPERIMENT
# -------------------------------------

# Dropout
#Dropout is a technique to randomly drop a fraction of neurons during training to prevent overfitting. It helps the model generalize better by making it less dependent on specific neurons.

# Modelling
model_2 <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.2) %>%  # Dropout with a rate of 20%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.2) %>%  # Dropout with a rate of 20%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 8, activation = "softmax")  # Output layer with 8 units for 8 clas

# Compiling
model_2 %>% compile(
  loss = "categorical_crossentropy",  # For multi-class classification
  optimizer = "adam",
  metrics = c("accuracy")
)

# Train the Model
history <- model_2 %>% fit(
  x = train_x,  # Input features
  y = train_y,  # One-hot encoded target variable
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

# Assuming test_x and test_y are your test features and labels
result_2 <- model_2 %>% evaluate(test_x, test_y)

# Print the result
cat("Test loss: ", result_2[1], "\n")
cat("Test accuracy: ", result_2[2], "\n")


# -------------------------------------
#         EXPERIMENT 2
# -------------------------------------

# Step 1: Create a new dataframe to preserve the original df_main
df <- df_main

# Step 2: Convert the 'status' column to a factor (for classification)
df$status <- as.factor(df$status)

# Step 3: One-hot encode the target variable (status)
df$status <- as.factor(df$status)
y <- to_categorical(as.integer(df$status) - 1)  # One-hot encode the target column (status)

# Step 4: Split the data into features (X) and target (y)
X <- df %>% select(-status) %>% as.matrix()

# Step 5: Set up K-fold cross-validation
K <- 10  # Define number of folds (you can adjust K as needed)
set.seed(123)  # For reproducibility

# Create the folds using caret's createFolds function
folds <- createFolds(df$status, k = K, list = TRUE, returnTrain = TRUE)

# Step 6: Loop through each fold, train and evaluate the model
results <- data.frame(Fold = integer(), Accuracy = numeric())  # to store results

for(i in 1:K) {
  # Split the data into training and testing sets
  train_data <- df[folds[[i]], ]
  test_data <- df[-folds[[i]], ]
  
  # Prepare the training and testing data
  X_train <- train_data %>% select(-status) %>% as.matrix()
  y_train <- to_categorical(as.integer(train_data$status) - 1)
  
  X_test <- test_data %>% select(-status) %>% as.matrix()
  y_test <- to_categorical(as.integer(test_data$status) - 1)
  
  # Step 7: Build the feedforward neural network model
  model_3 <- keras_model_sequential() %>%
    layer_dense(units = 32, activation = "relu", input_shape = ncol(train_x)) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(0.5) %>%  # Dropout with a rate of 50%
    layer_dense(units = 256, activation = "relu") %>%
    layer_dropout(0.2) %>%  # Dropout with a rate of 20%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dropout(0.5) %>%  # Dropout with a rate of 50%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = ncol(y_train), activation = 'softmax')  # Multi-class output
  
  # Compile the model
  model_3 %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
  
  # Step 8: Train the model
  model_3 %>% fit(X_train, y_train, epochs = 50, batch_size = 32, verbose = 0)
  
  # Step 9: Evaluate the model on the test set
  evaluation <- model_3 %>% evaluate(X_test, y_test, verbose = 0)
  
  # Step 10: Store the results (accuracy)
  results <- rbind(results, data.frame(Fold = i, Accuracy = evaluation[2]))
}

# Print the stored results for each fold
print(results)

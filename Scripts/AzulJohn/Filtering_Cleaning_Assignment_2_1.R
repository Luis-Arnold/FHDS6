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

# Call out package
library(fastDummies)
library(readr)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(keras)
library(writexl)


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

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

# Call out package
library(readr)
library(dplyr)

# -------------------------------------
#             INPUT DATA
# -------------------------------------

# Input Dataset 2
df_0 <- read_csv("D:/OneDrive - FHNW/Data Science/Final_Exam/Assignment_2-Dataset.csv")

# -------------------------------------
#         OMIT SOME COLUMNS
# -------------------------------------

# Seeing the data
View(df_0)

# Omit some non-related columns
df_1 <- subset(df_0, select = -c(ID))

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

# Dataset with character columns
df_character <- df_2 %>% select_if(is.character)

# Checking all
str(df_numeric)
str(df_character)


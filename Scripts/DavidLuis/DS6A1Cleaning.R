install.packages("tidyr")
install.packages("dplyr")
install.packages("caret")
install.packages("randomForest")

library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)

Assignment_1_Dataset <- read_delim("RawData/Assignment_1-Dataset.csv", 
                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

tempData <- Assignment_1_Dataset

# Initial exploration
head(tempData)

plot(tempData)

tempData <- tempData %>%
  select(-id, -member_id, -desc, -pymnt_plan, -policy_code, -application_type, -verification_status_joint, -url) %>%
  na.omit(tempData$int_rate)

View(tempData)

which(is.na(tempData$int_rate))

clean <- function() {
}

# Split into Test and Training set
set.seed(1)
# help(topic = "createDataPartition", package = "caret")
splitIndex <- createDataPartition(tempData$int_rate, p = .8, list = FALSE)
trainData <- tempData[splitIndex,]
testData <- tempData[-splitIndex,]

model <- randomForest(trainData, trainData$int_rate)



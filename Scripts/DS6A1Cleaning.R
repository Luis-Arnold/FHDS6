library(readr)
DataSetA1 <- read_delim("~/GitHub/FHDS6/Data/DataSetA1.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

head(DataSetA1)

plot(DataSetA1)

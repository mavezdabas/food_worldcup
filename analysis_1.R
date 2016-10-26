library(dplyr)
library(ggplot2)
library(plyr)

########################
# Loading the dataset
########################
foodData <- read.csv("food-world-cup-data.csv",header = TRUE)


########################
# Data Set Information
########################
colnames(foodData)

# Count with number of empty entries per column
na_count <-sapply(foodData, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Count with number of non empty entries per column
non_na_count <-sapply(foodData, function(y) sum(length(which(!is.na(y)))))
non_na_count <- data.frame(non_na_count)

# Number of entries per Location
locationCount <- data.frame(table(foodData$Location))

# Selecting Columns with No NA values {Complete Data}
collist <- c(colnames(foodData))
allValues <- data.frame(foodData[complete.cases(foodData), collist])

########################
# Explatory Data Analysis
########################































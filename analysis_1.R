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
#colnames(foodData)

# Count with number of empty entries per column
na_count <-sapply(foodData, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Count with number of non empty entries per column
non_na_count <-sapply(foodData, function(y) sum(length(which(!is.na(y)))))
non_na_count <- data.frame(non_na_count)

# Number of entries per Location
locationCount <- data.frame(table(foodData$Location))
knowledgeCount <- data.frame(table(foodData$Level_of_knowledge))
interestCount <- data.frame(table(foodData$Interested_cuisines_world))

# Selecting Columns with No NA values {Complete Data}
collist <- c(colnames(foodData))
allValues <- data.frame(foodData[complete.cases(foodData), collist])

##########################
# Explatory Data Analysis
##########################

# Most Liked food
# View(data.frame(sapply(foodData, function(y) sum(length(which((y == "5")))))))

# Worst liked food
# View(data.frame(sapply(foodData, function(y) sum(length(which((y == "1")))))))


# Counting rating for each cusinies
countRating <- sapply(foodData, function(y) summary(as.factor(y)))
countRating <- countRating[c(4:43)]
countRating <- as.data.frame(countRating)
countRating <- data.frame(t(countRating))

# Summary of the data.frame
summaryRating <- sapply(foodData, function(y) summary(as.factor(y)))

# Advance user Rating
advanceRating <- sapply(foodData %>%
                          dplyr::filter(Level_of_knowledge == "Advanced"),
                        function(y) summary(as.factor(y)))
advanceRating <- advanceRating[c(4:43)]  
advanceRating <- data.frame(Name = unlist(advanceRating))
advanceRating$Food <- rownames(advanceRating)



rating <- as.data.frame(sapply(advanceRating$Food, function(x) substr(x, nchar(x), nchar(x))))
advanceRating$Rating <- rating$`sapply(advanceRating$Food, function(x) substr(x, nchar(x), nchar(x)))`

advanceRating <- advanceRating %>%
  dplyr::filter(Rating != "s")

summary(advanceRating$Rating)












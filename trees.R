setwd("C:/Users/80498/1 yelp")
data <- read.csv('selected_data.csv')
head(data)

names <- colnames(data)
#match('WheelchairAccessible', names)

#data <- data[,-9]

set.seed(123)
library(rpart)
library(rpart.plot)

for(i in 2:length(colnames(data))){
  data[, i] <- factor(data[,i])
}

summary(data)

length(data[data$RestaurantsPriceRange2 == 'None', 1])
length(data[data$RestaurantsPriceRange2 == '1.0', 1])
length(data[data$RestaurantsPriceRange2 == '2.0', 1])
length(data[data$RestaurantsPriceRange2 == '3.0', 1])
length(data[data$RestaurantsPriceRange2 == '4.0', 1])

data <- data[data$RestaurantsPriceRange2 != 'None', ]

length(data[,1])

rp <- rpart(stars ~ ., data = data, method = 'anova')
rpart.plot(rp, type = 2)



library(partykit)

#ctrl <- ctree_control(minbucket = 100, minsplit = 100)
tree.model <- ctree(stars ~ ., data = data, control = ctrl)
plot(tree.model)

mean(data$stars)

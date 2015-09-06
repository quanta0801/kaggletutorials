library(data.table)
library(dplyr)
library(randomForest)

# getwd()
setwd("~/GitHub/kaggletutorials/titanic/")

# list.files()
train <- fread("train.csv")

train
names(train)

table(train[, list(Pclass, Survived)])
table(train[, list(Sex, Survived)])
summary(train[, Age])
hist(train[, Age], breaks=20)
summary(train[, Fare])
hist(train[, Fare], breaks = 20)
hist(train[, Fare], xlim = c(0,40), breaks = seq(0,520,2))
ceil40 <- function(fare){
    vec = numeric(length(fare))
    for (i in 1:length(vec)){
        if (fare[i] > 40) vec[i] = 40
        else vec[i] = fare[i]
    }
    return (vec)
}
train[, NewFare := ceil40(Fare)]
hist(train[, NewFare], xlim = c(0,40), breaks = seq(0,40,2))

table(train[, list(Embarked, Survived)])

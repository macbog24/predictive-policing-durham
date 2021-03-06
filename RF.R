
knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(data.table)
library(mltools)
library(car)
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(e1071)
library(caret)
library(tree)
library(randomForest)
library(rpart)
library(C50)
library(ipred)
if(!require(dplyr)) {install.packages("dplyr"); library("dplyr")}
if(!require(tree)) {install.packages("tree"); library("tree")}
if(!require(knitr)) {install.packages("knitr"); library("knitr")}
if(!require(kableExtra)) {install.packages("kableExtra"); library("kableExtra")}
if(!require(gbm)) {install.packages("gbm"); library("gbm")}
if(!require(randomForest)) {install.packages("randomForest"); library("randomForest")}

crimes <- read_csv("crimes_tidy.csv")
sort(unique(crimes$chrgdesc))
Charge <- as.data.frame(crimes[,19])
Var <- as.data.frame(crimes[,-19])

crimes <- with(crimes, data.frame(chrgdesc, month_occur, hour_occur, DIST, downtown))
#crimes$downtown <- ifelse(crimes$downtown == 1, "Yes", "No")
crimes$chrgdesc <- as.character(crimes$chrgdesc)
crimes[crimes == "PROPERTY OFFENSES (STOLEN/DESTRUCTION)"] <- "PROPERTY OFFENSES"
sample <- sample(x=nrow(crimes), size=.7*nrow(crimes))
train <- crimes[sample, ]

test <- crimes[-sample, ]
Charge.test <- crimes[-sample, ]

tree.fit <- rpart(chrgdesc ~ ., data=train, method = 'class')
tree.fit
plot(tree.fit,margin=.5)
text(tree.fit, pretty = 0)

tree.pred <- predict(tree.fit, test, type = 'class')

printcp(tree.fit)
plotcp(tree.fit) 
summary(tree.fit)

set.seed(1)
retrain <- train[complete.cases(train),]
retest <- test[complete.cases(test),]
retrain$chrgdesc = factor(retrain$chrgdesc) 
rf.fit <- randomForest(chrgdesc ~ ., data = retrain, mtry = 4)
rf.pred <- predict(rf.fit, newdata = retest)
table(retest$chrgdesc, rf.pred)
misclass <- sum(retest$chrgdesc != rf.pred)
rf.err <- misclass/length(rf.pred)
cat("Test Error: ", rf.err)
importance(rf.fit)


set.seed(1)
fit <- bagging(chrgdesc~., retrain, coob=TRUE)
predictions <- predict(fit, retest, type="class")
table(predictions, retest$chrgdesc)
print(fit)

cfit <- C5.0(chrgdesc~., data=retrain, trials=10)
predictions <- predict(cfit, retest)
table(predictions, retest$chrgdesc)
summary(cfit)
cfit
plot(cfit)

retrain$downtown = factor(retrain$downtown) 
cfit <- C5.0(downtown~ chrgdesc+month_occur+hour_occur, data=retrain, trials=10)
predictions <- predict(cfit, retest)
table(predictions, retest$downtown)
summary(cfit)

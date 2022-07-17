#ASDM Assignment Task 1: Classification
#Programmed by S. Mehran Ronaghi

#Setting working directory
mypath = 'C:/ASDM/Assignment/Task1'
setwd(mypath)
getwd()

#Importing Greater Manchester Road Safety Dataset
GMacc <- read.csv('gm-accident.csv', header= TRUE)
plot(GMacc$longitude,GMacc$latitude,type="p", col=rainbow(7))

dim(GMacc)
names(GMacc)
head(GMacc)
tail(GMacc)
summary(GMacc)
str(GMacc)

#Cleaning Variable Names
install.packages('janitor')
library(janitor)

GMacc <- GMacc %>% janitor::clean_names()

#Data Wrangling
install.packages('tidyverse')
library(tidyverse)

GMacc <- GMacc %>% rename('year' = 'i_accident_year')

names(GMacc)

#Missing Values
install.packages('skimr')
library(skimr)

skim(GMacc)

#Outliers
boxplot(GMacc$accident_severity)

#Normalisation
#install.packages('caret')
#library(caret)

#preproc<- preProcess(GMacc, method = 'range')
#GMacc <- predict(preproc,GMacc)

#summary(GMacc)

#Skew
plot(density(GMacc$accident_severity))
abline(v=mean(GMacc$accident_severity), col='red')
abline(v=median(GMacc$accident_severity), col='blue')

#install.packages('Boruta')
#library('Boruta')
#set.seed(123)
#GMacc_boruta <- Boruta(accident_severity ~ ., data = GMacc, ntree = 500)
#GMacc_boruta
#plot(GMacc_boruta)

#convert target variable to factor
GMacc$accident_severity <- as.factor(GMacc$accident_severity)

str(GMacc)

#specifying train and validate(test) data
set.seed(1234)
pd <- sample(2, nrow(GMacc), replace=TRUE, prob=c(0.8,0.2))
pd

train <- GMacc[pd==1,]
validate <- GMacc[pd==2,]

dim(train)
dim(validate)

#implementing Decision trees
install.packages('party')
library(party)

GMtree <- ctree(accident_severity ~ . ,data = train)
GMtree
plot(GMtree)

#Calculating classification accuracy and error on train data
predict(GMtree)
train_tab <- table(predict(GMtree), train$accident_severity)
print(train_tab)

sum(diag(train_tab))/sum(train_tab)
1-sum(diag(train_tab))/sum(train_tab)

#validating the model
predict(GMtree, newdata= validate)
validate_tab <- table(predict(GMtree, newdata= validate), validate$accident_severity)
print(validate_tab)

#Calculating classification accuracy and error on validate data
sum(diag(validate_tab))/sum(validate_tab)
1-sum(diag(validate_tab))/sum(validate_tab)
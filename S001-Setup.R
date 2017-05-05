#Kaggle Housing Prices Setup
library(tidyverse)
#install.packages(c("data.table","FeatureHashing","Matrix", "xgboost", "randomForest","randomForest",
#                  "pROC", "dummies", "Metrics", "kernlab", "mlbench" )0

install.packages("caret")

library(data.table)
library(FeatureHashing)
library(Matrix)
library(xgboost)
require(randomForest)
require(caret)
require(dplyr)
require(ggplot2)
library(pROC)
library(stringr)
library(dummies)
library(Metrics)
library(kernlab)
library(mlbench)


train <- read.csv("train.csv")
test <- read.csv("test.csv")


str(train)

summary(train)

my_lm <-  lm(SalePrice ~ GrLivArea, data = train) 
summary(my_lm)
new.df <- data.frame(GrLivArea=c(2000, 1500, 2500))
new.df2 <- data.frame(GrLivArea = train$GrLivArea)

p <- predict(my_lm, new.df2)

p$

ggplot(train, aes(GrLivArea, SalePrice)) + geom_point() + geom_smooth(method="loess")



lm2 <- lm(SalePrice ~ GrLivArea + MSSubClass + LotFrontage + LotArea + 
            OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
            MasVnrArea + 
            X1stFlrSF + 
            X2ndFlrSF + 
            LowQualFinSF + 
            BsmtFullBath + 
            BsmtHalfBath + 
            FullBath + 
            HalfBath + 
            BedroomAbvGr + 
            KitchenAbvGr + 
            TotRmsAbvGrd + 
            Fireplaces + 
            GarageCars + 
            GarageArea, data = train)

summary(lm2)

lm3 <- lm(SalePrice ~  + MSSubClass + LotFrontage + LotArea + 
            OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
            MasVnrArea + 
            X1stFlrSF + 
            X2ndFlrSF + 
            LowQualFinSF + 
            BsmtFullBath + 
            FullBath + 
            BedroomAbvGr + 
            KitchenAbvGr + 
            TotRmsAbvGrd + 
            Fireplaces + 
            GarageCars + 
             regshape + flat + normalSale
            , data = train)




summary(lm3)


table(train$Heating)


# ? Select column if is numeric or interger


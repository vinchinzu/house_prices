
# Load Packages
library(MASS) 
library(Metrics)
library(corrplot)
library(randomForest)
library(lars)
library(ggplot2)
library(xgboost)
library(Matrix)
library(methods)

# Read Data
Training <- read.csv("train.csv")
Test <- read.csv("test.csv")
# Test whether data is successfully loaded
names(Training)

```


Num_NA<-sapply(Training,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(Training),Count=Num_NA)

NA_Count
```

Among 1460 variables, 'Alley',  'PoolQC', 'Fence' and 'MiscFeature' have amazingly high number of missing value. Therefore, I 
have decided to remove those variables. After that, the number of effective variables has shrunken to 75 (excluding id). 

```{r,message=FALSE, warning=FALSE}
Training<- Training[,-c(7,73,74,75)]
```
Then, I transferred dummny variables into numeric form. Due to the intimidating size of dummy variables, I decided to transfer them 
directly by implementing 'as.integer' method. This is why I let the string as factor when reading the data file. The numeric variables
are sorted out in particular for the convenience of descriptive analysis.

```{r,message=FALSE, warning=FALSE}
# Numeric Variables
Num<-sapply(Training,is.numeric)
Num<-Training[,Num]

for(i in 1:77){
  if(is.factor(Training[,i])){
    Training[,i]<-as.integer(Training[,i])
  }
}

# Test
Training$Street[1:50]
```
Finally, for the remaining missing values, I replaced them with zero directly. The data cleansing procedure ends here.

```{r,message=FALSE, warning=FALSE}
Training[is.na(Training)]<-0
Num[is.na(Num)]<-0
```
### Descriptive Analysis

Exploring dataset could be diffcult when the quantity of variables is quite huge. Therefore, I mainly focused on the exploration of numeric
variables in this report. The descriptive analysis of dummy variables are mostly finished by drawing box plots. Some dummy variables, like 'Street',
are appeared to be ineffective due to the extreme box plot. The numeric variables are sorted out before turning dummy variables into numeric form.

We first draw a corrplot of numeric variables. Those with strong correlation with sale price are examined.
```{r,message=FALSE, warning=FALSE}
correlations<- cor(Num[,-1],use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
```
'OverallQual','TotalBsmtSF','GarageCars' and 'GarageArea' have relative strong correlation with each other. Therefore, as an example, we plot the correlation
among those four variables and SalePrice.
```{r,message=FALSE, warning=FALSE}
pairs(~SalePrice+OverallQual+TotalBsmtSF+GarageCars+GarageArea,data=Training,
      main="Scatterplot Matrix")
```
The dependent variable (SalePrice) looks having decent linearity when plotting with other variables. However, it is also obvious that some independent variables 
also have linear relationship with others. The problem of multicollinearity is obvious and should be treated when the quantity of variables in regression formula is huge.

The final descriptive analysis I put here would be the relationship between the variable 'YearBu' and Sale Price.

```{r,message=FALSE, warning=FALSE}
p<- ggplot(Training,aes(x= YearBuilt,y=SalePrice))+geom_point()+geom_smooth()
p
```
It is not diffcult to find that the price of house increases generally with the year built, the trend is obvious. 

### Model Selection

Before implementing models, one should first split the training set of data into 2 parts: a training set within the training set and a test set that can be used for evaluation.
Personally I prefer to split it with the ratio of 6:4, ***But if someone can tell me what spliting ratio is proved to be scienticfic I will be really grateful***
  
  ```{r,message=FALSE, warning=FALSE}
# Split the data into Training and Test Set # Ratio: 6:4 ###
Training_Inner<- Training[1:floor(length(Training[,1])*0.6),]
Test_Inner<- Training[(length(Training_Inner[,1])+1):1460,]
```
I will fit three regression models to the training set and choose the most suitable one by checking RMSE value.

#### Model 1: Linear Regression

The first and simplest but useful model is linear regression model. As the first step, I put all variables into the model.
```{r,message=FALSE,warning=FALSE}
reg1<- lm(SalePrice~., data = Training_Inner)
summary(reg1)

R Square is not bad, but many variables do not pass the Hypothesis Testing, so the model is not perfect. Potential overfitting will occur if someone insist on using it. Therefore,
the variable selection process should be involved in model construction. I prefer to use Step AIC method.

Several variables still should not be involved in model. By checking the result of Hypothesis Test, I mannually build the final linear regression model.

```{r,message=FALSE,warning=FALSE}
reg1_Modified_2<-lm(formula = SalePrice ~ MSSubClass + LotArea + 
                      Condition2 + OverallQual + OverallCond + 
                      YearBuilt  + RoofMatl +  ExterQual + 
                      BsmtQual + BsmtCond + BsmtFinSF1 + BsmtFinSF2 + 
                      BsmtUnfSF + X1stFlrSF + X2ndFlrSF + BedroomAbvGr + KitchenAbvGr + 
                      KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + 
                      GarageYrBlt + GarageCars +  SaleCondition, 
                    data = Training_Inner)
summary(reg1_Modified_2)
```
The R Square is not bad, and all variables pass the Hypothesis Test. The diagonsis of residuals is also not bad. The diagnosis can be viewed below.
```{r,message=FALSE,warning=FALSE}
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(reg1_Modified_2)
par(mfrow=c(1,1))
```

We check the performance of linear regression model with RMSE value.

```{r,message=FALSE,warning=FALSE}
Prediction_1<- predict(reg1_Modified_2, newdata= Test_Inner)
rmse(log(Test_Inner$SalePrice),log(Prediction_1))
```
#### Model 2: LASSO Regression

For the avoidance of multicollinearity, implementing LASSO regression is not a bad idea. Transferring the variables into the form of matrix, we can automate
the selection of variables by implementing 'lars' method in Lars package.

```{r,message=FALSE,warning=FALSE}
Independent_variable<- as.matrix(Training_Inner[,1:76])
Dependent_Variable<- as.matrix(Training_Inner[,77])
laa<- lars(Independent_variable,Dependent_Variable,type = 'lasso')
plot(laa)
```


```{r,message=FALSE,warning=FALSE}
best_step<- laa$df[which.min(laa$Cp)]
Prediction_2<- predict.lars(laa,newx =as.matrix(Test_Inner[,1:76]), s=best_step, type= "fit")
rmse(log(Test_Inner$SalePrice),log(Prediction_2$fit))

```

#### Model 3: Random Forest

#The other model I chose to fit in the training set is Random Forest model. The model, prediction and RMSE calculation can be found below:

for_1<- randomForest(SalePrice~.,data= Training_Inner)
Prediction_3 <- predict(for_1, newdata= Test_Inner)
rmse(log(Test_Inner$SalePrice),log(Prediction_3))

#Obviously, Random Forest may produce the best result within the training set so far. 

#### Model 4: XGBoost 

This amazing package really impressed me! And I have enthusiam to explore it. The first step of XGBoost is to transform the dataset into Sparse matrix.

```{r,message=FALSE,warning=FALSE}
train<- as.matrix(Training_Inner, rownames.force=NA)
test<- as.matrix(Test_Inner, rownames.force=NA)
train <- as(train, "sparseMatrix")
test <- as(test, "sparseMatrix")
# Never forget to exclude objective variable in 'data option'
train_Data <- xgb.DMatrix(data = train[,2:76], label = train[,"SalePrice"])
```
#Then I tune the parameters of xgboost model by building a 20-iteration for-loop. **Not sure whether this method is reliable but really time-consuming**
# Tuning the parameters #
# Creat Empty List
library(xgboost)
All_rmse<- c()
Param_group<-c()
for (iter in 1:20) {
param <- list(objective = "reg:linear",
eval_metric = "rmse",
booster = "gbtree",
max_depth = sample(6:10, 1),
eta = runif(1, 0.01, 0.3),
gamma = runif(1, 0.0, 0.2), 
subsample = runif(1, 0.6, 0.9),
colsample_bytree = runif(1, 0.5, 0.8)

)
cv.nround = 500
cv.nfold = 4
mdcv <- xgb.cv(data=train_Data, params = param, nthread=6, 
nfold=cv.nfold, nrounds=cv.nround,verbose = TRUE)
# Least Mean_Test_RMSE as Indicator # 
min_rmse<- min(mdcv[,test.rmse.mean])
All_rmse<-append(All_rmse,min_rmse)
Param_group<-append(Param_group,param)
# Select Param
param<-Param_group[(which.min(All_rmse)*8+1):(which.min(All_rmse)*8+8)]
}


#Then, the parameter can be selected by the random process.
#Since the process is relatively boring, I just skip it in RMarkdown file
#and use the optimal parameters 
#I got in my local R script for the prediction and evaluation. ** Can I ask some more efficient and intelligent method of parameter tuning from smart Kagglers?
#Looking forward to your advice!!**

param<-list(
objective = "reg:linear",
eval_metric = "rmse",
booster = "gbtree",
max_depth = 8,
eta = 0.123,
gamma = 0.0385, 
subsample = 0.734,
colsample_bytree = 0.512
)

#The model should be tested before making actual prediction.

Training <-
xgb.train(params = param,
data = train_Data,
nrounds = 600,
watchlist = list(train = train_Data),
verbose = TRUE,
print_every_n = 50,
nthread = 6)

test_data <- xgb.DMatrix(data = test[,2:76])

prediction <- predict(Training, test_data)
rmse(log(Test_Inner$SalePrice),log(prediction))


# Transforming Test set #
Test<- Test[,-c(7,73,74,75)]
for(i in 1:76){
if(is.factor(Test[,i])){
Test[,i]<-as.integer(Test[,i])
}
}
Test[is.na(Test)]<-0

re_train<- as.matrix(Training,rownames.force =NA)
re_train<- as(re_train,'sparseMatrix')
retrain_Data<- xgb.DMatrix(data = re_train[,2:76],label=re_train[,"SalePrice"])
bstSparse_retrain<- xgb.train(params=param,
data=retrain_Data,
nrounds = 600,
watchlist = list(train = retrain_Data),
verbose = TRUE,
print_every_n = 50,
nthread = 2
)

Test_Matrix<-as.matrix(Test,rownames.force = FALSE)
Test_Matrix<-as(Test_Matrix,"sparseMatrix")
Test_Matrix<-xgb.DMatrix(data = Test_Matrix[,2:76])


#Then the prediction process begins.

Submit<- predict(Training, newdata=Test_Matrix)
Submit<-data.frame(Id= Test$Id, SalePrice= Submit)

write.csv(Submit, "submit.csv")

Fun with Real Estate
Data Driven Real Estate Analysis
Reality tv shows about real estate are all the rage lately, and they drive me nuts because of how unscientific and illogical they are. (Well, that’s not the only reason, but it’s the relevant one.) People with more money than they know what to do with going 10 rounds about the kind of drawer pulls they want to have in their kitchen and where exactly their imported bathroom tile should come from. Blech.

This dataset gives us a chance to look into the data on what really influences the value of a house without the garbage impulses of reality tv getting in the way, so I am excited to take a look!
  
  Plan
Assemble the data and explore it
Clean variables, build what is needed
Three Models: Linear, randomForest, and xgboost
Choose the best model and make the prediction for entry
Clean the Data
So, what do we have here?

##  [1] "Id"            "MSSubClass"    "MSZoning"      "LotFrontage"  
##  [5] "LotArea"       "Street"        "Alley"         "LotShape"     
##  [9] "LandContour"   "Utilities"     "LotConfig"     "LandSlope"    
## [13] "Neighborhood"  "Condition1"    "Condition2"    "BldgType"     
## [17] "HouseStyle"    "OverallQual"   "OverallCond"   "YearBuilt"    
## [21] "YearRemodAdd"  "RoofStyle"     "RoofMatl"      "Exterior1st"  
## [25] "Exterior2nd"   "MasVnrType"    "MasVnrArea"    "ExterQual"    
## [29] "ExterCond"     "Foundation"    "BsmtQual"      "BsmtCond"     
## [33] "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"    "BsmtFinType2" 
## [37] "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "Heating"      
## [41] "HeatingQC"     "CentralAir"    "Electrical"    "X1stFlrSF"    
## [45] "X2ndFlrSF"     "LowQualFinSF"  "GrLivArea"     "BsmtFullBath" 
## [49] "BsmtHalfBath"  "FullBath"      "HalfBath"      "BedroomAbvGr" 
## [53] "KitchenAbvGr"  "KitchenQual"   "TotRmsAbvGrd"  "Functional"   
## [57] "Fireplaces"    "FireplaceQu"   "GarageType"    "GarageYrBlt"  
## [61] "GarageFinish"  "GarageCars"    "GarageArea"    "GarageQual"   
## [65] "GarageCond"    "PavedDrive"    "WoodDeckSF"    "OpenPorchSF"  
## [69] "EnclosedPorch" "X3SsnPorch"    "ScreenPorch"   "PoolArea"     
## [73] "PoolQC"        "Fence"         "MiscFeature"   "MiscVal"      
## [77] "MoSold"        "YrSold"        "SaleType"      "SaleCondition"
## [81] "SalePrice"
I think the best steps to start with would be reformatting some character variables that we can easily convert to numeric. What’s the street type about?

table(train$Street)
## 
## Grvl Pave 
##    6 1454
Not exactly fancy, let’s just make that paved or not. What about Lot Shape?

train$paved[train$Street == "Pave"] <- 1
train$paved[train$Street != "Pave"] <- 0

table(train$LotShape)
## 
## IR1 IR2 IR3 Reg 
## 484  41  10 925
I assume these are something like variations on “irregular”. So let’s go with regular or not, and then we’ll have this shape variable still if we want to go more granular later. Taking up land contour as the next.

train$regshape[train$LotShape == "Reg"] <- 1
train$regshape[train$LotShape != "Reg"] <- 0

table(train$LandContour)

train$flat[train$LandContour == "Lvl"] <- 1

train$flat[train$LandContour != "Lvl"] <- 0

table(train$SaleCondition)
train$normalSale[train$SaleCondition == "Normal"] <- 1
train$normalSale[train$SaleCondition != "Normal"] <- 0

train %>% group_by(MoSold) %>% summarize(AvgPrice = mean(SalePrice)) %>% ggplot(aes(x=MoSold, y = AvgPrice)) + geom_point()



library(corrplot)

correlations <- cor(train[,c(5,6,7,8, 16:25)], use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")


correlations <- cor(train[,c(5,6,7,8, 26:35)], use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")


correlations <- cor(train[,c(5,6,7,8, 66:75)], use="everything")
## Warning in cor(train[, c(5, 6, 7, 8, 66:75)], use = "everything"): the
## standard deviation is zero
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")


Anyway, the correlations that both have to do with square footage I am going to discount, because size of the total and size of a floor, for example, are obvious correlations.

pairs(~YearBuilt+OverallQual+TotalBsmtSF+GrLivArea,data=train,
      main="Simple Scatterplot Matrix")


#This is fun too- I picked a few of the variables that had a lot of correlation strengths. Basements have been getting bigger over time, apparently. As have the sizes of the living areas. Good to know!
  
 # I’m also interested in the relationship between sale price and some numeric variables, but these can be tougher to visualize.

library(car)

scatterplot(SalePrice ~ YearBuilt, data=train,  xlab="Year Built", ylab="Sale Price", grid=FALSE)


scatterplot(SalePrice ~ YrSold, data=train,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)


scatterplot(SalePrice ~ X1stFlrSF, data=train,  xlab="Square Footage Floor 1", ylab="Sale Price", grid=FALSE)


#Prices are higher for new houses, that makes sense. Also, we can see that sale prices dropped when we would expect (thanks, housing crisis). We also have some loopy outliers on first floor square footage- probably bad data but it’s not going to have a huge influence.

#Fix some NAs
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- 0
train$MasVnrArea[is.na(train$MasVnrArea)] <- 0
train$LotFrontage[is.na(train$LotFrontage)] <- 0

#Interactions based on correlation
train$year_qual <- train$YearBuilt*train$OverallQual #overall condition
train$year_r_qual <- train$YearRemodAdd*train$OverallQual #quality x remodel
train$qual_bsmt <- train$OverallQual*train$TotalBsmtSF #quality x basement size

train$livarea_qual <- train$OverallQual*train$GrLivArea #quality x living area
train$qual_bath <- train$OverallQual*train$FullBath #quality x baths

train$qual_ext <- train$OverallQual*train$exterior_cond #quality x exterior

#names(train)
Model Prepping
Then, partition! I always like to use the caret partitioning function.

outcome <- train$SalePrice

partition <- createDataPartition(y=outcome,
                                 p=.5,
                                 list=F)
training <- train[partition,]
testing <- train[-partition,]
A Linear Model
Finally, we have our data and can build some models. Since our outcome is a continuous numeric variable, we want a linear model, not a GLM. First, let’s just toss it all in there. I always like to use a proper regression model as my first examination of the data, to get a feel for what’s there.

lm_model_15 <- lm(SalePrice ~ ., data=training)
summary(lm_model_15)
## 
## Call:
## lm(formula = SalePrice ~ ., data = training)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -232667  -14959     -97   13514  173651 
## 
## Coefficients: (5 not defined because of singularities)
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        1.668e+06  2.088e+06   0.799 0.424630    
## Id                -1.714e+00  2.891e+00  -0.593 0.553480    
## MSSubClass        -1.692e+02  4.194e+01  -4.035 6.11e-05 ***
## LotFrontage       -6.983e+01  4.057e+01  -1.721 0.085713 .  
## LotArea           -1.538e-01  2.015e-01  -0.763 0.445686    
## OverallQual       -2.199e+05  1.469e+05  -1.498 0.134740    
## OverallCond        8.638e+03  1.597e+03   5.409 8.94e-08 ***
## YearBuilt         -2.774e+02  2.660e+02  -1.043 0.297487    
## YearRemodAdd      -3.798e+02  4.297e+02  -0.884 0.377150    
## MasVnrArea         1.333e+01  1.073e+01   1.243 0.214464    
## BsmtFinSF1         4.520e+01  1.793e+01   2.520 0.011962 *  
## BsmtFinSF2         5.020e+01  1.877e+01   2.675 0.007662 ** 
## BsmtUnfSF          2.517e+01  1.736e+01   1.450 0.147651    
## TotalBsmtSF               NA         NA      NA       NA    
## X1stFlrSF          6.945e+01  1.823e+01   3.810 0.000152 ***
## X2ndFlrSF          4.897e+01  1.909e+01   2.565 0.010539 *  
## LowQualFinSF      -1.573e+01  3.251e+01  -0.484 0.628590    
## GrLivArea                 NA         NA      NA       NA    
## BsmtFullBath       7.686e+01  3.697e+03   0.021 0.983419    
## BsmtHalfBath      -1.376e+04  5.783e+03  -2.380 0.017594 *  
## FullBath          -7.716e+03  1.444e+04  -0.534 0.593365    
## HalfBath           4.247e+03  3.887e+03   1.093 0.274961    
## BedroomAbvGr      -3.450e+03  2.388e+03  -1.445 0.148963    
## KitchenAbvGr      -1.174e+04  8.531e+03  -1.376 0.169211    
## TotRmsAbvGrd       4.940e+03  1.641e+03   3.009 0.002719 ** 
## Fireplaces         8.204e+03  3.923e+03   2.091 0.036904 *  
## GarageYrBlt       -1.192e+01  7.525e+00  -1.584 0.113672    
## GarageCars         1.427e+04  3.730e+03   3.827 0.000142 ***
## GarageArea        -1.192e+01  1.273e+01  -0.936 0.349533    
## WoodDeckSF         4.207e+01  1.083e+01   3.885 0.000113 ***
## OpenPorchSF       -5.440e+00  1.969e+01  -0.276 0.782361    
## EnclosedPorch     -1.710e+01  2.135e+01  -0.801 0.423446    
## X3SsnPorch         8.117e+01  3.738e+01   2.171 0.030263 *  
## ScreenPorch        7.006e+01  2.426e+01   2.888 0.004008 ** 
## PoolArea          -1.657e+02  3.855e+01  -4.297 1.99e-05 ***
## MiscVal           -9.980e-01  1.768e+00  -0.565 0.572583    
## MoSold            -2.377e+02  4.568e+02  -0.520 0.602937    
## YrSold            -2.507e+02  9.341e+02  -0.268 0.788471    
## paved              2.186e+04  1.726e+04   1.267 0.205654    
## regshape          -9.187e+02  2.770e+03  -0.332 0.740281    
## flat               1.459e+04  4.857e+03   3.003 0.002774 ** 
## pubutil            6.443e+04  3.382e+04   1.905 0.057171 .  
## gentle_slope      -1.564e+04  6.784e+03  -2.305 0.021494 *  
## culdesac_fr3              NA         NA      NA       NA    
## nbhd_price_level   1.772e+04  2.641e+03   6.708 4.29e-11 ***
## pos_features_1    -2.946e+04  8.600e+03  -3.426 0.000652 ***
## pos_features_2            NA         NA      NA       NA    
## twnhs_end_or_1fam -7.559e+03  7.279e+03  -1.038 0.299465    
## house_style_level -2.528e+03  4.158e+03  -0.608 0.543374    
## roof_hip_shed      8.045e+03  3.418e+03   2.354 0.018889 *  
## roof_matl_hi       2.820e+04  1.537e+04   1.835 0.066928 .  
## exterior_1        -1.215e+04  7.285e+03  -1.668 0.095797 .  
## exterior_2         1.883e+04  7.133e+03   2.640 0.008491 ** 
## exterior_mason_1  -6.577e+03  3.723e+03  -1.766 0.077797 .  
## exterior_cond     -7.244e+04  1.431e+04  -5.062 5.42e-07 ***
## exterior_cond2    -1.458e+03  3.984e+03  -0.366 0.714469    
## found_concrete    -2.059e+03  4.042e+03  -0.509 0.610686    
## bsmt_cond1         5.173e+03  3.217e+03   1.608 0.108281    
## bsmt_cond2        -1.516e+03  3.797e+03  -0.399 0.689781    
## bsmt_exp           3.453e+03  1.493e+03   2.313 0.021051 *  
## bsmt_fin1          3.168e+03  1.501e+03   2.111 0.035124 *  
## bsmt_fin2          2.452e+03  1.975e+03   1.241 0.214897    
## gasheat            9.562e+03  1.517e+04   0.630 0.528765    
## heatqual           9.957e+00  1.682e+03   0.006 0.995277    
## air               -1.267e+04  6.738e+03  -1.881 0.060473 .  
## standard_electric -3.917e+03  5.117e+03  -0.765 0.444277    
## kitchen            9.784e+03  3.213e+03   3.045 0.002422 ** 
## fire              -2.155e+03  1.830e+03  -1.178 0.239331    
## gar_attach        -5.462e+02  3.861e+03  -0.141 0.887550    
## gar_finish         1.349e+03  3.660e+03   0.369 0.712498    
## garqual            1.218e+04  7.688e+03   1.584 0.113684    
## garqual2          -5.855e+03  7.688e+03  -0.762 0.446598    
## paved_drive               NA         NA      NA       NA    
## housefunction      1.251e+04  5.454e+03   2.293 0.022158 *  
## pool_good          2.745e+05  3.211e+04   8.548  < 2e-16 ***
## priv_fence         3.332e+02  6.474e+03   0.051 0.958971    
## sale_cat           4.281e+03  3.160e+03   1.355 0.175908    
## sale_cond          9.957e+03  4.580e+03   2.174 0.030044 *  
## zone              -1.465e+02  3.457e+03  -0.042 0.966218    
## alleypave         -1.018e+03  7.531e+03  -0.135 0.892559    
## year_qual          5.615e+01  4.335e+01   1.295 0.195672    
## year_r_qual        5.088e+01  7.915e+01   0.643 0.520561    
## qual_bsmt         -6.519e+00  2.494e+00  -2.614 0.009166 ** 
## livarea_qual      -2.214e+00  2.475e+00  -0.895 0.371274    
## qual_bath          1.879e+03  2.314e+03   0.812 0.417072    
## qual_ext           1.113e+04  2.084e+03   5.338 1.30e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30930 on 650 degrees of freedom
## Multiple R-squared:  0.8694, Adjusted R-squared:  0.8533 
## F-statistic: 54.07 on 80 and 650 DF,  p-value: < 2.2e-16
Lots of stuff we can drop right off, that’s good. Some multicollinearity is making the model drop a few variables, but that’s ok.

Also, our R-squared is not too bad! In case you’re unfamiliar, that indicates what percent of the variation in the outcome is explained using the model we designed.

lm_model_15 <- lm(SalePrice ~ MSSubClass+LotArea+BsmtUnfSF+
                    X1stFlrSF+X2ndFlrSF+GarageCars+
                    WoodDeckSF+nbhd_price_level+
                    exterior_cond+pos_features_1+
                    bsmt_exp+kitchen+housefunction+pool_good+sale_cond+
                    qual_ext+qual_bsmt, data=training)
summary(lm_model_15)
## 
## Call:
## lm(formula = SalePrice ~ MSSubClass + LotArea + BsmtUnfSF + X1stFlrSF + 
##     X2ndFlrSF + GarageCars + WoodDeckSF + nbhd_price_level + 
##     exterior_cond + pos_features_1 + bsmt_exp + kitchen + housefunction + 
##     pool_good + sale_cond + qual_ext + qual_bsmt, data = training)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -322019  -16772    -503   14881  218443 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      -4.815e+04  1.506e+04  -3.197 0.001449 ** 
## MSSubClass       -1.865e+02  3.162e+01  -5.899 5.64e-09 ***
## LotArea          -2.211e-01  1.839e-01  -1.202 0.229787    
## BsmtUnfSF        -1.354e+01  3.248e+00  -4.169 3.43e-05 ***
## X1stFlrSF         6.134e+01  6.535e+00   9.387  < 2e-16 ***
## X2ndFlrSF         4.349e+01  3.703e+00  11.743  < 2e-16 ***
## GarageCars        1.069e+04  2.221e+03   4.812 1.83e-06 ***
## WoodDeckSF        3.850e+01  1.102e+01   3.493 0.000507 ***
## nbhd_price_level  1.854e+04  2.247e+03   8.249 7.68e-16 ***
## exterior_cond    -4.108e+04  7.112e+03  -5.776 1.14e-08 ***
## pos_features_1   -1.696e+04  8.914e+03  -1.903 0.057443 .  
## bsmt_exp          7.569e+03  1.396e+03   5.421 8.12e-08 ***
## kitchen           1.196e+04  3.136e+03   3.813 0.000149 ***
## housefunction     1.240e+04  5.449e+03   2.275 0.023223 *  
## pool_good         1.419e+05  2.536e+04   5.596 3.13e-08 ***
## sale_cond         1.386e+04  3.270e+03   4.238 2.55e-05 ***
## qual_ext          7.227e+03  7.563e+02   9.555  < 2e-16 ***
## qual_bsmt        -1.534e+00  7.689e-01  -1.995 0.046382 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34240 on 713 degrees of freedom
## Multiple R-squared:  0.8244, Adjusted R-squared:  0.8202 
## F-statistic: 196.9 on 17 and 713 DF,  p-value: < 2.2e-16
That’s our model with the important stuff, more or less. How does the RMSE turn out? That is our outcome of interest, after all.

prediction <- predict(lm_model_15, testing, type="response")
model_output <- cbind(testing, prediction)

model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

#Test with RMSE

rmse(model_output$log_SalePrice,model_output$log_prediction)
## [1] 0.1625762
A Random Forest
Not too bad, given that this is just an LM. Let’s try training the model with an RF. Let’s use all the variables and see how things look, since randomforest does its own feature selection.

model_1 <- randomForest(SalePrice ~ ., data=training)


# Predict using the test set
prediction <- predict(model_1, testing)
model_output <- cbind(testing, prediction)


model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

#Test with RMSE

rmse(model_output$log_SalePrice,model_output$log_prediction)
## [1] 0.129874
An xgboost
Nice! Try it with xgboost?

#Assemble and format the data

training$log_SalePrice <- log(training$SalePrice)
testing$log_SalePrice <- log(testing$SalePrice)

#Create matrices from the data frames
trainData<- as.matrix(training, rownames.force=NA)
testData<- as.matrix(testing, rownames.force=NA)

#Turn the matrices into sparse matrices
train2 <- as(trainData, "sparseMatrix")
test2 <- as(testData, "sparseMatrix")

#####
#colnames(train2)
#Cross Validate the model

vars <- c(2:37, 39:86) #choose the columns we want to use in the prediction matrix

trainD <- xgb.DMatrix(data = train2[,vars], label = train2[,"SalePrice"]) #Convert to xgb.DMatrix format

#Cross validate the model
cv.sparse <- xgb.cv(data = trainD,
                    nrounds = 600,
                    min_child_weight = 0,
                    max_depth = 10,
                    eta = 0.02,
                    subsample = .7,
                    colsample_bytree = .7,
                    booster = "gbtree",
                    eval_metric = "rmse",
                    verbose = TRUE,
                    print_every_n = 50,
                    nfold = 4,
                    nthread = 2,
                    objective="reg:linear")
## [1]  train-rmse:195005.003906+1012.923119    test-rmse:195150.914062+2962.570278 
## [51] train-rmse:78467.261719+662.759181  test-rmse:82230.642578+4199.727883 
## [101]    train-rmse:34697.281250+804.307500  test-rmse:44915.177735+6595.603107 
## [151]    train-rmse:17470.770752+945.518765  test-rmse:34572.629395+7150.926451 
## [201]    train-rmse:9939.554443+894.324742   test-rmse:32001.610351+6913.437300 
## [251]    train-rmse:6230.806641+720.441702   test-rmse:31467.872070+6651.298992 
## [301]    train-rmse:4304.733704+586.261003   test-rmse:31520.323731+6552.300338 
## [351]    train-rmse:3130.306519+460.549863   test-rmse:31694.579590+6542.268936 
## [401]    train-rmse:2337.401337+340.867469   test-rmse:31877.824218+6583.607992 
## [451]    train-rmse:1782.607483+260.568346   test-rmse:32051.724121+6624.026290 
## [501]    train-rmse:1359.533905+190.523414   test-rmse:32144.135254+6630.878098 
## [551]    train-rmse:1048.664414+146.240873   test-rmse:32221.508301+6641.308896 
## [600]    train-rmse:821.868271+116.823602    test-rmse:32290.149414+6656.170419
#Train the model

#Choose the parameters for the model
param <- list(colsample_bytree = .7,
              subsample = .7,
              booster = "gbtree",
              max_depth = 10,
              eta = 0.02,
              eval_metric = "rmse",
              objective="reg:linear")


#Train the model using those parameters
bstSparse <-
  xgb.train(params = param,
            data = trainD,
            nrounds = 600,
            watchlist = list(train = trainD),
            verbose = TRUE,
            print_every_n = 50,
            nthread = 2)
## [1]  train-rmse:195014.750000 
## [51] train-rmse:77897.039062 
## [101]    train-rmse:34041.437500 
## [151]    train-rmse:17198.125000 
## [201]    train-rmse:9977.225586 
## [251]    train-rmse:6436.147461 
## [301]    train-rmse:4580.320312 
## [351]    train-rmse:3416.647949 
## [401]    train-rmse:2639.834473 
## [451]    train-rmse:2092.339355 
## [501]    train-rmse:1679.307617 
## [551]    train-rmse:1357.634521 
## [600]    train-rmse:1093.774414
Predict and test the RMSE.

testD <- xgb.DMatrix(data = test2[,vars])
#Column names must match the inputs EXACTLY
prediction <- predict(bstSparse, testD) #Make the prediction based on the half of the training data set aside

#Put testing prediction and test dataset all together
test3 <- as.data.frame(as.matrix(test2))
prediction <- as.data.frame(as.matrix(prediction))
colnames(prediction) <- "prediction"
model_output <- cbind(test3, prediction)

model_output$log_prediction <- log(model_output$prediction)
model_output$log_SalePrice <- log(model_output$SalePrice)

#Test with RMSE

rmse(model_output$log_SalePrice,model_output$log_prediction)
## [1] 0.1197072
Nice, that’s pretty good stuff. I’ll take the xgboost I think, let’s call that good and make up the submission. Honestly, this is where the interesting stuff basically ends, unless you want to see the retraining and submission formatting.

Retrain on the full sample
rm(bstSparse)

#Create matrices from the data frames
retrainData<- as.matrix(train, rownames.force=NA)

#Turn the matrices into sparse matrices
retrain <- as(retrainData, "sparseMatrix")

param <- list(colsample_bytree = .7,
              subsample = .7,
              booster = "gbtree",
              max_depth = 10,
              eta = 0.02,
              eval_metric = "rmse",
              objective="reg:linear")

retrainD <- xgb.DMatrix(data = retrain[,vars], label = retrain[,"SalePrice"])

#retrain the model using those parameters
bstSparse <-
  xgb.train(params = param,
            data = retrainD,
            nrounds = 600,
            watchlist = list(train = trainD),
            verbose = TRUE,
            print_every_n = 50,
            nthread = 2)
## [1]  train-rmse:194950.546875 
## [51] train-rmse:77041.585938 
## [101]    train-rmse:33339.394531 
## [151]    train-rmse:16391.695312 
## [201]    train-rmse:9333.628906 
## [251]    train-rmse:6018.882324 
## [301]    train-rmse:4418.943848 
## [351]    train-rmse:3487.291504 
## [401]    train-rmse:2858.591553 
## [451]    train-rmse:2388.746338 
## [501]    train-rmse:1956.183472 
## [551]    train-rmse:1606.430786 
## [600]    train-rmse:1324.839355
Prepare the prediction data
Here I just repeat the same work I did on the training set, check the code tab to see all the details.

Then, format it for xgboost, I’m just using my boilerplate code for that.

# Get the supplied test data ready #

predict <- as.data.frame(test) #Get the dataset formatted as a frame for later combining

#Create matrices from the data frames
predData<- as.matrix(predict, rownames.force=NA)

#Turn the matrices into sparse matrices
predicting <- as(predData, "sparseMatrix")
Make sure your training sample and prediction sample have the same variables. I have been including this in code lately because I was making silly mistakes on variable choice.

colnames(train[,c(2:37, 39:86)])
##  [1] "MSSubClass"        "LotFrontage"       "LotArea"          
##  [4] "OverallQual"       "OverallCond"       "YearBuilt"        
##  [7] "YearRemodAdd"      "MasVnrArea"        "BsmtFinSF1"       
## [10] "BsmtFinSF2"        "BsmtUnfSF"         "TotalBsmtSF"      
## [13] "X1stFlrSF"         "X2ndFlrSF"         "LowQualFinSF"     
## [16] "GrLivArea"         "BsmtFullBath"      "BsmtHalfBath"     
## [19] "FullBath"          "HalfBath"          "BedroomAbvGr"     
## [22] "KitchenAbvGr"      "TotRmsAbvGrd"      "Fireplaces"       
## [25] "GarageYrBlt"       "GarageCars"        "GarageArea"       
## [28] "WoodDeckSF"        "OpenPorchSF"       "EnclosedPorch"    
## [31] "X3SsnPorch"        "ScreenPorch"       "PoolArea"         
## [34] "MiscVal"           "MoSold"            "YrSold"           
## [37] "paved"             "regshape"          "flat"             
## [40] "pubutil"           "gentle_slope"      "culdesac_fr3"     
## [43] "nbhd_price_level"  "pos_features_1"    "pos_features_2"   
## [46] "twnhs_end_or_1fam" "house_style_level" "roof_hip_shed"    
## [49] "roof_matl_hi"      "exterior_1"        "exterior_2"       
## [52] "exterior_mason_1"  "exterior_cond"     "exterior_cond2"   
## [55] "found_concrete"    "bsmt_cond1"        "bsmt_cond2"       
## [58] "bsmt_exp"          "bsmt_fin1"         "bsmt_fin2"        
## [61] "gasheat"           "heatqual"          "air"              
## [64] "standard_electric" "kitchen"           "fire"             
## [67] "gar_attach"        "gar_finish"        "garqual"          
## [70] "garqual2"          "paved_drive"       "housefunction"    
## [73] "pool_good"         "priv_fence"        "sale_cat"         
## [76] "sale_cond"         "zone"              "alleypave"        
## [79] "year_qual"         "year_r_qual"       "qual_bsmt"        
## [82] "livarea_qual"      "qual_bath"         "qual_ext"
vars <- c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond","YearBuilt",
          "YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF","TotalBsmtSF"   ,   
          "X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea","BsmtFullBath","BsmtHalfBath"  ,   
          "FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces"     ,  
          "GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF","EnclosedPorch"    ,
          "X3SsnPorch","ScreenPorch","PoolArea","MiscVal","MoSold","YrSold",
          "paved","regshape","flat","pubutil","gentle_slope","culdesac_fr3"     ,
          "nbhd_price_level" , "pos_features_1","pos_features_2","twnhs_end_or_1fam","house_style_level", "roof_hip_shed"    ,
          "roof_matl_hi","exterior_1","exterior_2","exterior_mason_1","exterior_cond","exterior_cond2"   ,
          "found_concrete","bsmt_cond1","bsmt_cond2","bsmt_exp","bsmt_fin1","bsmt_fin2"    ,   
          "gasheat","heatqual","air","standard_electric", "kitchen","fire",
          "gar_attach","gar_finish","garqual","garqual2","paved_drive","housefunction",
          "pool_good","priv_fence","sale_cat","sale_cond","zone","alleypave",
          "year_qual","year_r_qual","qual_bsmt","livarea_qual","qual_bath", "qual_ext")

#colnames(predicting)
colnames(predicting[,vars])
##  [1] "MSSubClass"        "LotFrontage"       "LotArea"          
##  [4] "OverallQual"       "OverallCond"       "YearBuilt"        
##  [7] "YearRemodAdd"      "MasVnrArea"        "BsmtFinSF1"       
## [10] "BsmtFinSF2"        "BsmtUnfSF"         "TotalBsmtSF"      
## [13] "X1stFlrSF"         "X2ndFlrSF"         "LowQualFinSF"     
## [16] "GrLivArea"         "BsmtFullBath"      "BsmtHalfBath"     
## [19] "FullBath"          "HalfBath"          "BedroomAbvGr"     
## [22] "KitchenAbvGr"      "TotRmsAbvGrd"      "Fireplaces"       
## [25] "GarageYrBlt"       "GarageCars"        "GarageArea"       
## [28] "WoodDeckSF"        "OpenPorchSF"       "EnclosedPorch"    
## [31] "X3SsnPorch"        "ScreenPorch"       "PoolArea"         
## [34] "MiscVal"           "MoSold"            "YrSold"           
## [37] "paved"             "regshape"          "flat"             
## [40] "pubutil"           "gentle_slope"      "culdesac_fr3"     
## [43] "nbhd_price_level"  "pos_features_1"    "pos_features_2"   
## [46] "twnhs_end_or_1fam" "house_style_level" "roof_hip_shed"    
## [49] "roof_matl_hi"      "exterior_1"        "exterior_2"       
## [52] "exterior_mason_1"  "exterior_cond"     "exterior_cond2"   
## [55] "found_concrete"    "bsmt_cond1"        "bsmt_cond2"       
## [58] "bsmt_exp"          "bsmt_fin1"         "bsmt_fin2"        
## [61] "gasheat"           "heatqual"          "air"              
## [64] "standard_electric" "kitchen"           "fire"             
## [67] "gar_attach"        "gar_finish"        "garqual"          
## [70] "garqual2"          "paved_drive"       "housefunction"    
## [73] "pool_good"         "priv_fence"        "sale_cat"         
## [76] "sale_cond"         "zone"              "alleypave"        
## [79] "year_qual"         "year_r_qual"       "qual_bsmt"        
## [82] "livarea_qual"      "qual_bath"         "qual_ext"
Actually do the predicting.

#Column names must match the inputs EXACTLY
prediction <- predict(bstSparse, predicting[,vars])

prediction <- as.data.frame(as.matrix(prediction))  #Get the dataset formatted as a frame for later combining
colnames(prediction) <- "prediction"
model_output <- cbind(predict, prediction) #Combine the prediction output with the rest of the set

sub2 <- data.frame(Id = model_output$Id, SalePrice = model_output$prediction)
length(model_output$prediction)
## [1] 1459
write.csv(sub2, file = "sub3.csv", row.names = F)
head(sub2$SalePrice)
## [1] 126802.0 158142.5 172708.9 190513.9 192890.8 175320.2
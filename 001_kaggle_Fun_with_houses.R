library(data.table)
library(FeatureHashing)
library(Matrix)
library(xgboost)
require(randomForest)
require("caret")
require(dplyr)
require(ggplot2)
library(pROC)
library(stringr)
library(dummies)
library(Metrics)
library(kernlab)
library(mlbench)
library(tidyverse)

#install.packages(c("data.table","FeatureHashing","Matrix", "xgboost", "randomForest","randomForest",
                 "pROC", "dummies", "Metrics", "kernlab", "mlbench" ))



###Plan

* Assemble the data and explore it
* Clean variables, build what is needed
* Three Models: Linear, randomForest, and xgboost
* Choose the best model and make the prediction for entry


###Clean the Data

So, what do we have here?
```{r load, echo=FALSE}
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors=FALSE)

#train <- read.csv("U:/Programming/R/Kaggle/houses/train.csv", stringsAsFactors=FALSE)
#test <- read.csv("U:/Programming/R/Kaggle/houses/test.csv", stringsAsFactors=FALSE)

names(train)

```

I think the best steps to start with would be reformatting some character variables that we can easily convert to numeric. What's the street type about?

```{r formatting_street}
table(train$Street)
```
Not exactly fancy, let's just make that paved or not. What about Lot Shape?

```{r formatting_street2}
train$paved[train$Street == "Pave"] <- 1
train$paved[train$Street != "Pave"] <- 0

table(train$LotShape)
train$regshape[train$LotShape == "Reg"] <- 1
train$regshape[train$LotShape != "Reg"] <- 0

table(train$LandContour)


train$flat[train$LandContour == "Lvl"] <- 1
train$flat[train$LandContour != "Lvl"] <- 0

train$pubutil[train$Utilities == "AllPub"] <- 1
train$pubutil[train$Utilities != "AllPub"] <- 0

train$gentle_slope[train$LandSlope == "Gtl"] <- 1
train$gentle_slope[train$LandSlope != "Gtl"] <- 0



# summarize(group_by(train, LotConfig),
#           mean(SalePrice, na.rm=T))

train$culdesac_fr3[train$LandSlope %in% c("CulDSac", "FR3")] <- 1
train$culdesac_fr3[!train$LandSlope %in% c("CulDSac", "FR3")] <- 0


nbhdprice <- summarize(group_by(train, Neighborhood),
mean(SalePrice, na.rm=T))

#nbhdprice[order(nbhdprice$`mean(SalePrice, na.rm = T)`),]

nbhdprice_lo <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)
nbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &
nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )
nbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)

train$nbhd_price_level[train$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1
train$nbhd_price_level[train$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
train$nbhd_price_level[train$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3

 summarize(group_by(train, Condition1),
          mean(SalePrice, na.rm=T))

train$pos_features_1[train$Condition1 %in% c("PosA", "PosN")] <- 1
train$pos_features_1[!train$Condition1 %in% c("PosA", "PosN")] <- 0

 summarize(group_by(train, Condition2),
           mean(SalePrice, na.rm=T))

train$pos_features_2[train$Condition1 %in% c("PosA", "PosN")] <- 1
train$pos_features_2[!train$Condition1 %in% c("PosA", "PosN")] <- 0


 summarize(group_by(train, BldgType),
           mean(SalePrice, na.rm=T))

train$twnhs_end_or_1fam[train$BldgType %in% c("1Fam", "TwnhsE")] <- 1
train$twnhs_end_or_1fam[!train$BldgType %in% c("1Fam", "TwnhsE")] <- 0

housestyle_price <- summarize(group_by(train, HouseStyle),
mean(SalePrice, na.rm=T))

housestyle_lo <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 140000)
housestyle_med <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 200000 &
housestyle_price$`mean(SalePrice, na.rm = T)` >= 140000 )
housestyle_hi <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 200000)

train$house_style_level[train$HouseStyle %in% housestyle_lo$HouseStyle] <- 1
train$house_style_level[train$HouseStyle %in% housestyle_med$HouseStyle] <- 2
train$house_style_level[train$HouseStyle %in% housestyle_hi$HouseStyle] <- 3


roofstyle_price <- summarize(group_by(train, RoofStyle),
mean(SalePrice, na.rm=T))

train$roof_hip_shed[train$RoofStyle %in% c("Hip", "Shed")] <- 1
train$roof_hip_shed[!train$RoofStyle %in% c("Hip", "Shed")] <- 0

roofmatl_price <- summarize(group_by(train, RoofMatl),
mean(SalePrice, na.rm=T))

train$roof_matl_hi[train$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 1
train$roof_matl_hi[!train$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 0


price <- summarize(group_by(train, Exterior1st),
mean(SalePrice, na.rm=T))

matl_lo_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)
matl_med_1<- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &
price$`mean(SalePrice, na.rm = T)` >= 140000 )
matl_hi_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)

train$exterior_1[train$Exterior1st %in% matl_lo_1$Exterior1st] <- 1
train$exterior_1[train$Exterior1st %in% matl_med_1$Exterior1st] <- 2
train$exterior_1[train$Exterior1st %in% matl_hi_1$Exterior1st] <- 3


price <- summarize(group_by(train, Exterior2nd),
mean(SalePrice, na.rm=T))

matl_lo <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)
matl_med <- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &
price$`mean(SalePrice, na.rm = T)` >= 140000 )
matl_hi <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)

train$exterior_2[train$Exterior2nd %in% matl_lo$Exterior2nd] <- 1
train$exterior_2[train$Exterior2nd %in% matl_med$Exterior2nd] <- 2
train$exterior_2[train$Exterior2nd %in% matl_hi$Exterior2nd] <- 3

price <- summarize(group_by(train, MasVnrType),
mean(SalePrice, na.rm=T))

train$exterior_mason_1[train$MasVnrType %in% c("Stone", "BrkFace") | is.na(train$MasVnrType)] <- 1
train$exterior_mason_1[!train$MasVnrType %in% c("Stone", "BrkFace") & !is.na(train$MasVnrType)] <- 0


price <- summarize(group_by(train, ExterQual),
mean(SalePrice, na.rm=T))

train$exterior_cond[train$ExterQual == "Ex"] <- 4
train$exterior_cond[train$ExterQual == "Gd"] <- 3
train$exterior_cond[train$ExterQual == "TA"] <- 2
train$exterior_cond[train$ExterQual == "Fa"] <- 1


price <- summarize(group_by(train, ExterCond),
mean(SalePrice, na.rm=T))

train$exterior_cond2[train$ExterCond == "Ex"] <- 5
train$exterior_cond2[train$ExterCond == "Gd"] <- 4
train$exterior_cond2[train$ExterCond == "TA"] <- 3
train$exterior_cond2[train$ExterCond == "Fa"] <- 2
train$exterior_cond2[train$ExterCond == "Po"] <- 1


price <- summarize(group_by(train,Foundation), mean(SalePrice, na.rm=T))


train$found_concrete[train$Foundation == "PConc"] <- 1
train$found_concrete[train$Foundation != "PConc"] <- 0


price <- summarize(group_by(train, BsmtQual),
mean(SalePrice, na.rm=T))

train$bsmt_cond1[train$BsmtQual == "Ex"] <- 5
train$bsmt_cond1[train$BsmtQual == "Gd"] <- 4
train$bsmt_cond1[train$BsmtQual == "TA"] <- 3
train$bsmt_cond1[train$BsmtQual == "Fa"] <- 2
train$bsmt_cond1[is.na(train$BsmtQual)] <- 1


price <- summarize(group_by(train, BsmtCond),
mean(SalePrice, na.rm=T))

train$bsmt_cond2[train$BsmtCond == "Gd"] <- 5
train$bsmt_cond2[train$BsmtCond == "TA"] <- 4
train$bsmt_cond2[train$BsmtCond == "Fa"] <- 3
train$bsmt_cond2[is.na(train$BsmtCond)] <- 2
train$bsmt_cond2[train$BsmtCond == "Po"] <- 1


price <- summarize(group_by(train, BsmtExposure),
mean(SalePrice, na.rm=T))

train$bsmt_exp[train$BsmtExposure == "Gd"] <- 5
train$bsmt_exp[train$BsmtExposure == "Av"] <- 4
train$bsmt_exp[train$BsmtExposure == "Mn"] <- 3
train$bsmt_exp[train$BsmtExposure == "No"] <- 2
train$bsmt_exp[is.na(train$BsmtExposure)] <- 1


price <- summarize(group_by(train, BsmtFinType1),
mean(SalePrice, na.rm=T))

train$bsmt_fin1[train$BsmtFinType1 == "GLQ"] <- 5
train$bsmt_fin1[train$BsmtFinType1 == "Unf"] <- 4
train$bsmt_fin1[train$BsmtFinType1 == "ALQ"] <- 3
train$bsmt_fin1[train$BsmtFinType1 %in% c("BLQ", "Rec", "LwQ")] <- 2
train$bsmt_fin1[is.na(train$BsmtFinType1)] <- 1



price <- summarize(group_by(train, BsmtFinType2),
mean(SalePrice, na.rm=T))

train$bsmt_fin2[train$BsmtFinType2 == "ALQ"] <- 6
train$bsmt_fin2[train$BsmtFinType2 == "Unf"] <- 5
train$bsmt_fin2[train$BsmtFinType2 == "GLQ"] <- 4
train$bsmt_fin2[train$BsmtFinType2 %in% c("Rec", "LwQ")] <- 3
train$bsmt_fin2[train$BsmtFinType2 == "BLQ"] <- 2
train$bsmt_fin2[is.na(train$BsmtFinType2)] <- 1


price <- summarize(group_by(train, Heating),
mean(SalePrice, na.rm=T))


train$gasheat[train$Heating %in% c("GasA", "GasW")] <- 1
train$gasheat[!train$Heating %in% c("GasA", "GasW")] <- 0


price <- summarize(group_by(train, HeatingQC),
mean(SalePrice, na.rm=T))

#add reference to second column
#ggplot(price, aes(x=HeatingQC, y = `mean(SalePrice, na.rm = T)` )) + geom_point()

train$heatqual[train$HeatingQC == "Ex"] <- 5
train$heatqual[train$HeatingQC == "Gd"] <- 4
train$heatqual[train$HeatingQC == "TA"] <- 3
train$heatqual[train$HeatingQC == "Fa"] <- 2
train$heatqual[train$HeatingQC == "Po"] <- 1


price <- summarize(group_by(train, CentralAir),
mean(SalePrice, na.rm=T))

train$air[train$CentralAir == "Y"] <- 1
train$air[train$CentralAir == "N"] <- 0


price <- summarize(group_by(train, Electrical),
mean(SalePrice, na.rm=T))

train$standard_electric[train$Electrical == "SBrkr" | is.na(train$Electrical)] <- 1
train$standard_electric[!train$Electrical == "SBrkr" & !is.na(train$Electrical)] <- 0


price <- summarize(group_by(train, KitchenQual),
mean(SalePrice, na.rm=T))

train$kitchen[train$KitchenQual == "Ex"] <- 4
train$kitchen[train$KitchenQual == "Gd"] <- 3
train$kitchen[train$KitchenQual == "TA"] <- 2
train$kitchen[train$KitchenQual == "Fa"] <- 1


price <- summarize(group_by(train, FireplaceQu),
mean(SalePrice, na.rm=T))

train$fire[train$FireplaceQu == "Ex"] <- 5
train$fire[train$FireplaceQu == "Gd"] <- 4
train$fire[train$FireplaceQu == "TA"] <- 3
train$fire[train$FireplaceQu == "Fa"] <- 2
train$fire[train$FireplaceQu == "Po" | is.na(train$FireplaceQu)] <- 1


price <- summarize(group_by(train, GarageType),
mean(SalePrice, na.rm=T))

train$gar_attach[train$GarageType %in% c("Attchd", "BuiltIn")] <- 1
train$gar_attach[!train$GarageType %in% c("Attchd", "BuiltIn")] <- 0


price <- summarize(group_by(train, GarageFinish),
mean(SalePrice, na.rm=T))

train$gar_finish[train$GarageFinish %in% c("Fin", "RFn")] <- 1
train$gar_finish[!train$GarageFinish %in% c("Fin", "RFn")] <- 0


price <- summarize(group_by(train, GarageQual),
mean(SalePrice, na.rm=T))

train$garqual[train$GarageQual == "Ex"] <- 5
train$garqual[train$GarageQual == "Gd"] <- 4
train$garqual[train$GarageQual == "TA"] <- 3
train$garqual[train$GarageQual == "Fa"] <- 2
train$garqual[train$GarageQual == "Po" | is.na(train$GarageQual)] <- 1


price <- summarize(group_by(train, GarageCond),
mean(SalePrice, na.rm=T))

train$garqual2[train$GarageCond == "Ex"] <- 5
train$garqual2[train$GarageCond == "Gd"] <- 4
train$garqual2[train$GarageCond == "TA"] <- 3
train$garqual2[train$GarageCond == "Fa"] <- 2
train$garqual2[train$GarageCond == "Po" | is.na(train$GarageCond)] <- 1


price <- summarize(group_by(train, PavedDrive),
mean(SalePrice, na.rm=T))

train$paved_drive[train$PavedDrive == "Y"] <- 1
train$paved_drive[!train$PavedDrive != "Y"] <- 0
train$paved_drive[is.na(train$paved_drive)] <- 0


price <- summarize(group_by(train, Functional),
mean(SalePrice, na.rm=T))

train$housefunction[train$Functional %in% c("Typ", "Mod")] <- 1
train$housefunction[!train$Functional %in% c("Typ", "Mod")] <- 0


price <- summarize(group_by(train, PoolQC),
mean(SalePrice, na.rm=T))

train$pool_good[train$PoolQC %in% c("Ex")] <- 1
train$pool_good[!train$PoolQC %in% c("Ex")] <- 0


price <- summarize(group_by(train, Fence),
mean(SalePrice, na.rm=T))

train$priv_fence[train$Fence %in% c("GdPrv")] <- 1
train$priv_fence[!train$Fence %in% c("GdPrv")] <- 0


price <- summarize(group_by(train, MiscFeature),
mean(SalePrice, na.rm=T))
#This doesn't seem worth using at the moment. May adjust later.


price <- summarize(group_by(train, SaleType),
                   mean(SalePrice, na.rm=T))

# price[order(price$`mean(SalePrice, na.rm = T)`),]

train$sale_cat[train$SaleType %in% c("New", "Con")] <- 5
train$sale_cat[train$SaleType %in% c("CWD", "ConLI")] <- 4
train$sale_cat[train$SaleType %in% c("WD")] <- 3
train$sale_cat[train$SaleType %in% c("COD", "ConLw", "ConLD")] <- 2
train$sale_cat[train$SaleType %in% c("Oth")] <- 1


price <- summarize(group_by(train, SaleCondition),
                   mean(SalePrice, na.rm=T))

# price[order(price$`mean(SalePrice, na.rm = T)`),]

train$sale_cond[train$SaleCondition %in% c("Partial")] <- 4
train$sale_cond[train$SaleCondition %in% c("Normal", "Alloca")] <- 3
train$sale_cond[train$SaleCondition %in% c("Family","Abnorml")] <- 2
train$sale_cond[train$SaleCondition %in% c("AdjLand")] <- 1


price <- summarize(group_by(train, MSZoning),
                   mean(SalePrice, na.rm=T))

# price[order(price$`mean(SalePrice, na.rm = T)`),]

train$zone[train$MSZoning %in% c("FV")] <- 4
train$zone[train$MSZoning %in% c("RL")] <- 3
train$zone[train$MSZoning %in% c("RH","RM")] <- 2
train$zone[train$MSZoning %in% c("C (all)")] <- 1


price <- summarize(group_by(train, Alley),
                   mean(SalePrice, na.rm=T))

# price[order(price$`mean(SalePrice, na.rm = T)`),]

train$alleypave[train$Alley %in% c("Pave")] <- 1
train$alleypave[!train$Alley %in% c("Pave")] <- 0



#Done. Now, time to drop off the variables that have been made numeric and are no longer needed.


train$Street <- NULL
train$LotShape <- NULL
train$LandContour <- NULL
train$Utilities <- NULL
train$LotConfig <- NULL
train$LandSlope <- NULL
train$Neighborhood <- NULL
train$Condition1 <- NULL
train$Condition2 <- NULL
train$BldgType <- NULL
train$HouseStyle <- NULL
train$RoofStyle <- NULL
train$RoofMatl <- NULL

train$Exterior1st <- NULL
train$Exterior2nd <- NULL
train$MasVnrType <- NULL
train$ExterQual <- NULL
train$ExterCond <- NULL

train$Foundation <- NULL
train$BsmtQual <- NULL
train$BsmtCond <- NULL
train$BsmtExposure <- NULL
train$BsmtFinType1 <- NULL
train$BsmtFinType2 <- NULL

train$Heating <- NULL
train$HeatingQC <- NULL
train$CentralAir <- NULL
train$Electrical <- NULL
train$KitchenQual <- NULL
train$FireplaceQu <- NULL

train$GarageType <- NULL
train$GarageFinish <- NULL
train$GarageQual <- NULL
train$GarageCond <- NULL
train$PavedDrive <- NULL

train$Functional <- NULL
train$PoolQC <- NULL
train$Fence <- NULL
train$MiscFeature <- NULL
train$SaleType <- NULL
train$SaleCondition <- NULL
train$MSZoning <- NULL
train$Alley <- NULL

str(train)

#Another thing I want to do is build some interactions that may be worth looking at. 
#For example, if the house has a pool, is it more important that it has a big deck, or something like that? 
#I used correlation visuals like this to do it- you can choose what you'd want to put in and how many variations
#you want to make.

library(corrplot)

correlations <- cor(train[,c(5,6,7,8, 16:25)], use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

correlations <- cor(train[,c(5,6,7,8, 26:35)], use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")

correlations <- cor(train[,c(5,6,7,8, 66:75)], use="everything")
corrplot(correlations, method="circle", type="lower",  sig.level = 0.01, insig = "blank")
```


#Anyway, the correlations that both have to do with square footage I am going to discount, because size of the total and size of a floor, for example, are obvious correlations. 


pairs(~YearBuilt+OverallQual+TotalBsmtSF+GrLivArea,data=train,
main="Simple Scatterplot Matrix")

#This is fun too- I picked a few of the variables that had a lot of correlation strengths. Basements have been getting bigger over time, apparently. As have the sizes of the living areas. Good to know!


#I'm also interested in the relationship between sale price and some numeric variables, but these can be tougher to visualize.


library(car)

scatterplot(SalePrice ~ YearBuilt, data=train,  xlab="Year Built", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ YrSold, data=train,  xlab="Year Sold", ylab="Sale Price", grid=FALSE)
scatterplot(SalePrice ~ X1stFlrSF, data=train,  xlab="Square Footage Floor 1", ylab="Sale Price", grid=FALSE)



#Prices are higher for new houses, that makes sense. Also, we can see that sale prices dropped when we would expect (thanks, housing crisis).
#We also have some loopy outliers on first floor square footage- probably bad data but it's not going to have a huge influence.



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

names(train)
str(train)




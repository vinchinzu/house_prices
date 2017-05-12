require(ggplot2) # for data visualization
require(stringr) #extracting string patterns
require(Matrix) # matrix transformations
require(glmnet) # ridge, lasso & elastinet
require(xgboost) # gbm
require(randomForest)
require(Metrics) # rmse
require(dplyr) # load this in last so plyr doens't overlap it
require(caret) # one hot encoding
require(scales) # plotting $$
require(e1071) # skewness
require(corrplot) # correlation plot


train = read.csv('train.csv', stringsAsFactors = FALSE)
test = read.csv('test.csv', stringsAsFactors = FALSE)


# combine the datasets
df.combined = rbind(within(train, rm('Id','SalePrice')), within(test, rm('Id')))
dim(df.combined)


na.cols = which(colSums(is.na(df.combined)) > 0)

na.cols = which(colSums(is.na(df.combined))>0)
sort(colSums(sapply(df.combined[na.cols], is.na)), decreasing = TRUE)



plot.categoric = function(cols, df){
  for (col in cols) {
    order.cols = names(sort(table(df.combined[,col]), decreasing = TRUE))
    
    num.plot = qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}

plot.categoric('PoolQC', df.combined)


df.combined[(df.combined$PoolArea > 0) & is.na(df.combined$PoolQC),c('PoolQC','PoolArea')]


df.combined[,c('PoolQC','PoolArea')] %>%
  group_by(PoolQC) %>%
  summarise(mean = mean(PoolArea), counts = n()) 


df.combined[2421,'PoolQC'] = 'Ex'
df.combined[2504,'PoolQC'] = 'Ex'
df.combined[2600,'PoolQC'] = 'Fa'
df.combined$PoolQC[is.na(df.combined$PoolQC)] = 'None'

length(which(df.combined$GarageYrBlt == df.combined$YearBuilt))

idx = which(is.na(df.combined$GarageYrBlt))
df.combined[idx, 'GarageYrBlt'] = df.combined[idx, 'YearBuilt']

garage.cols = c('GarageArea', 'GarageCars', 'GarageQual', 'GarageFinish', 'GarageCond', 'GarageType')

df.combined[2127,'GarageQual'] = 'TA'
df.combined[2127, 'GarageFinish'] = 'Unf'
df.combined[2127, 'GarageCond'] = 'TA'

for (col in garage.cols){
  if (sapply(df.combined[col], is.numeric) == TRUE){
    df.combined[sapply(df.combined[col], is.na), col] = 0
  }
  else{
    df.combined[sapply(df.combined[col], is.na), col] = 'None'
  }
}

df.combined$Electrical[is.na(df.combined$Electrical)] = 'SBrkr'

bsmt.cols = names(df.combined)[sapply(names(df.combined), function(x) str_detect(x, 'Bsmt'))]


df.combined[c(949, 1488, 2349), 'BsmtExposure'] = 'No'

for (col in bsmt.cols){
  if (sapply(df.combined[col], is.numeric) == TRUE){
    df.combined[sapply(df.combined[col], is.na),col] = 0
  }
  else{
    df.combined[sapply(df.combined[col],is.na),col] = 'None'
  }
}


idx = which(is.na(df.combined$Exterior1st) | is.na(df.combined$Exterior2nd))
df.combined[idx,c('Exterior1st', 'Exterior2nd')]


df.combined$Exterior1st[is.na(df.combined$Exterior1st)] = 'Other'
df.combined$Exterior2nd[is.na(df.combined$Exterior2nd)] = 'Other'


df.combined$SaleType[is.na(df.combined$SaleType)] = 'WD'

df.combined$Functional[is.na(df.combined$Functional)] = 'Typ'

which(df.combined$Utilities == 'NoSeWa') # in the training data set

col.drops = c('Utilities')
df.combined = df.combined[,!names(df.combined) %in% c('Utilities')]

df.combined$MSZoning[c(2217, 2905)] = 'RL'
df.combined$MSZoning[c(1916, 2251)] = 'RM'

df.combined[(is.na(df.combined$MasVnrType)) | (is.na(df.combined$MasVnrArea)), c('MasVnrType', 'MasVnrArea')]


na.omit(df.combined[,c('MasVnrType','MasVnrArea')]) %>%
  group_by(na.omit(MasVnrType)) %>%
  summarise(MedianArea = median(MasVnrArea,na.rm = TRUE), counts = n()) %>%
  arrange(MedianArea)

df.combined[2611, 'MasVnrType'] = 'BrkFace'

df.combined$MasVnrType[is.na(df.combined$MasVnrType)] = 'None'
df.combined$MasVnrArea[is.na(df.combined$MasVnrArea)] = 0

f.combined['Nbrh.factor'] = factor(df.combined$Neighborhood, levels = unique(df.combined$Neighborhood))

lot.by.nbrh = df.combined[,c('Neighborhood','LotFrontage')] %>%
  group_by(Neighborhood) %>%
  summarise(median = median(LotFrontage, na.rm = TRUE))
lot.by.nbrh


idx = which(is.na(df.combined$LotFrontage))

for (i in idx){
  lot.median = lot.by.nbrh[lot.by.nbrh == df.combined$Neighborhood[i],'median']
  df.combined[i,'LotFrontage'] = lot.median[[1]]
}

df.combined$Fence[is.na(df.combined$Fence)] = 'None'
df.combined$MiscFeature[is.na(df.combined$MiscFeature)] = 'None'

plot.categoric('FireplaceQu', df.combined)
which((df.combined$Fireplaces > 0) & (is.na(df.combined$FireplaceQu)))


df.combined$FireplaceQu[is.na(df.combined$FireplaceQu)] = 'None'

df.combined$Alley[is.na(df.combined$Alley)] = 'None'

df.combined$KitchenQual[is.na(df.combined$KitchenQual)] = 'TA'


paste('There are', sum(sapply(df.combined, is.na)), 'missing values left')

#sum(sapply(df.combined, is.na))


############
#Adding custom numeric features
##############


num_features = names(which(sapply(df.combined, is.numeric)))
cat_features = names(which(sapply(df.combined, is.character)))

df.numeric = df.combined[num_features]

group.df = df.combined[1:1460,]
group.df$SalePrice = train$SalePrice



# function that groups a column by its features and returns the mdedian saleprice for each unique feature. 
group.prices = function(col) {
  group.table = group.df[,c(col, 'SalePrice', 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.Quality = round(mean(OverallQual),2),
              mean.Price = mean(SalePrice), n = n()) %>%
    arrange(mean.Quality)
  
  print(qplot(x=reorder(group.table[[col]], -group.table[['mean.Price']]), y=group.table[['mean.Price']]) +
          geom_bar(stat='identity', fill='cornflowerblue') +
          theme_minimal() +
          scale_y_continuous(labels = dollar) +
          labs(x=col, y='Mean SalePrice') +
          theme(axis.text.x = element_text(angle = 45)))
  
  return(data.frame(group.table))
}

## functional to compute the mean overall quality for each quality
quality.mean = function(col) {
  group.table = df.combined[,c(col, 'OverallQual')] %>%
    group_by_(col) %>%
    summarise(mean.qual = mean(OverallQual)) %>%
    arrange(mean.qual)
  
  return(data.frame(group.table))
}


# function that maps a categoric value to its corresponding numeric value and returns that column to the data frame
map.fcn = function(cols, map.list, df){
  for (col in cols){
    df[col] = as.numeric(map.list[df.combined[,col]])
  }
  return(df)
}

qual.cols = c('ExterQual', 'ExterCond', 'GarageQual', 'GarageCond', 'FireplaceQu', 'KitchenQual', 'HeatingQC', 'BsmtQual')

group.prices('FireplaceQu')
group.prices('BsmtQual')
group.prices('KitchenQual')

qual.list = c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

df.numeric = map.fcn(qual.cols, qual.list, df.numeric)

group.prices('BsmtExposure')


bsmt.list = c('None' = 0, 'No' = 1, 'Mn' = 2, 'Av' = 3, 'Gd' = 4)

df.numeric = map.fcn(c('BsmtExposure'), bsmt.list, df.numeric)

group.prices('BsmtFinType1')

# visualization for BsmtFinTyp2 instead of another table
df.combined[,c('BsmtFinType1', 'BsmtFinSF1')] %>%
  group_by(BsmtFinType1) %>%
  summarise(medianArea = median(BsmtFinSF1), counts = n()) %>%
  arrange(medianArea) %>%
  ggplot(aes(x=reorder(BsmtFinType1,-medianArea), y=medianArea)) +
  geom_bar(stat = 'identity', fill='cornflowerblue') +
  labs(x='BsmtFinType2', y='Median of BsmtFinSF2') +
  geom_text(aes(label = sort(medianArea)), vjust = -0.5) +
  scale_y_continuous(limits = c(0,850)) +
  theme_minimal()

bsmt.fin.list = c('None' = 1, 'Unf' = 1, 'LwQ' = 2,'Rec'= 3, 'BLQ' = 4, 'ALQ' = 5, 'GLQ' = 6)
df.numeric = map.fcn(c('BsmtFinType1','BsmtFinType2'), bsmt.fin.list, df.numeric)


group.prices('Functional')


functional.list = c('None' = 0, 'Sal' = 1, 'Sev' = 2, 'Maj2' = 3, 'Maj1' = 4, 'Mod' = 5, 'Min2' = 6, 'Min1' = 7, 'Typ'= 8)

df.numeric['Functional'] = as.numeric(functional.list[df.combined$Functional])

group.prices('GarageFinish')




garage.fin.list = c('None' = 1,'Unf' = 1, 'RFn' = 1, 'Fin' = 2)

df.numeric['GarageFinish'] = as.numeric(garage.fin.list[df.combined$GarageFinish])


group.prices('Fence')


fence.list = c('None' = 1, 'MnWw' = 1, 'GdWo' = 1, 'MnPrv' = 2, 'GdPrv' = 4)

df.numeric['Fence'] = as.numeric(fence.list[df.combined$Fence])

MSdwelling.list = c('20' = 1, '30'= 0, '40' = 0, '45' = 0,'50' = 0, '60' = 1, '70' = 0, '75' = 0, '80' = 0, '85' = 0, '90' = 0, '120' = 1, '150' = 0, '160' = 0, '180' = 0, '190' = 0)

df.numeric['NewerDwelling'] = as.numeric(MSdwelling.list[as.character(df.combined$MSSubClass)])


#Correlation
corr.df = cbind(df.numeric[1:1460,], train['SalePrice'])

correlations = cor(corr.df)

corr.SalePrice = as.matrix(sort(correlations[,'SalePrice'], decreasing = TRUE))


corr.idx = names(which(apply(corr.SalePrice, 1, function(x) (x > 0.5 | x < -0.5))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)


#install.packages("GGally")
require(GGally)
lm.plt = function(data, mapping, ...){
  plt = ggplot(data = data, mapping = mapping) + 
    geom_point(shape = 20, alpha = 0.7, color = 'darkseagreen') +
    geom_smooth(method=loess, fill="red", color="red") +
    geom_smooth(method=lm, fill="blue", color="blue") +
    theme_minimal()
  return(plt)
}

ggpairs(corr.df, corr.idx[1:6], lower = list(continuous = lm.plt))
ggpairs(corr.df, corr.idx[c(1,7:11)], lower = list(continuous = lm.plt))

plot.categoric('LotShape', df.combined)
df.numeric['RegularLotShape'] = (df.combined$LotShape == 'Reg') * 1
df.numeric['LandLeveled'] = (df.combined$LandContour == 'Lvl') * 1
df.numeric['LandSlopeGentle'] = (df.combined$LandSlope == 'Gtl') * 1

df.numeric['ElectricalSB'] = (df.combined$Electrical == 'SBrkr') * 1
df.numeric['GarageDetchd'] = (df.combined$GarageType == 'Detchd') * 1
df.numeric['HasPavedDrive'] = (df.combined$PavedDrive == 'Y') * 1

df.numeric['HasWoodDeck'] = (df.combined$WoodDeckSF > 0) * 1

df.numeric['Has2ndFlr'] = (df.combined$X2ndFlrSF > 0) * 1

df.numeric['HasMasVnr'] = (df.combined$MasVnrArea > 0) * 1
df.numeric['HasShed'] = (df.combined$MiscFeature == 'Shed') * 1
df.numeric['Remodeled'] = (df.combined$YearBuilt != df.combined$YearRemodAdd) * 1
df.numeric['RecentRemodel'] = (df.combined$YearRemodAdd >= df.combined$YrSold) * 1
df.numeric['NewHouse'] = (df.combined$YearBuilt == df.combined$YrSold) * 1

cols.binary = c('X2ndFlrSF', 'MasVnrArea', 'WoodDeckSF', 'OpenPorchSF', 'EnclosedPorch', 'X3SsnPorch', 'ScreenPorch')
for (col in cols.binary){
  df.numeric[str_c('Has',col)] = (df.combined[,col] == 0) * 1
}
df.numeric['HighSeason'] = (df.combined$MoSold %in% c(5,6,7)) * 1



train[,c('Neighborhood','SalePrice')] %>%
  group_by(Neighborhood) %>%
  summarise(median.price = median(SalePrice, na.rm = TRUE)) %>%
  arrange(median.price) %>%
  mutate(nhbr.sorted = factor(Neighborhood, levels=Neighborhood)) %>%
  ggplot(aes(x=nhbr.sorted, y=median.price)) +
  geom_point() +
  geom_text(aes(label = median.price, angle = 45), vjust = 2) +
  theme_minimal() +
  labs(x='Neighborhood', y='Median price') +
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle=45))


other.nbrh = unique(df.combined$Neighborhood)[!unique(df.combined$Neighborhood) %in% c('StoneBr', 'NoRidge','NridgHt')]

ggplot(train, aes(x=SalePrice, y=GrLivArea, colour=Neighborhood)) +
  geom_point(shape=16, alpha=.8, size=4) +
  scale_color_manual(limits = c(other.nbrh, 'StoneBr', 'NoRidge', 'NridgHt'), values = c(rep('black', length(other.nbrh)), 'indianred',
                                                                                         'cornflowerblue', 'darkseagreen')) +
  theme_minimal() +
  scale_x_continuous(label=dollar)


nbrh.rich = c('Crawfor', 'Somerst, Timber', 'StoneBr', 'NoRidge', 'NridgeHt')
df.numeric['NbrhRich'] = (df.combined$Neighborhood %in% nbrh.rich) *1

group.prices('Neighborhood')


nbrh.map = c('MeadowV' = 0, 'IDOTRR' = 1, 'Sawyer' = 1, 'BrDale' = 1, 'OldTown' = 1, 'Edwards' = 1, 
             'BrkSide' = 1, 'Blueste' = 1, 'SWISU' = 2, 'NAmes' = 2, 'NPkVill' = 2, 'Mitchel' = 2,
             'SawyerW' = 2, 'Gilbert' = 2, 'NWAmes' = 2, 'Blmngtn' = 2, 'CollgCr' = 2, 'ClearCr' = 3, 
             'Crawfor' = 3, 'Veenker' = 3, 'Somerst' = 3, 'Timber' = 3, 'StoneBr' = 4, 'NoRidge' = 5, 
             'NridgHt' = 6)

df.numeric['NeighborhoodBin'] = as.numeric(nbrh.map[df.combined$Neighborhood])



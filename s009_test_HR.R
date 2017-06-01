 
library(tidyverse)
d <- read.csv("HR.csv")


plot(d[,1],d[,2])
ggplot(d, aes(satisfaction_level,last_evaluation, colour=left)) + geom_point() + geom_smooth()


ggplot(d, aes(average_montly_hours, satisfaction_level,colour=left)) + geom_point() + geom_smooth()

ggplot(d, aes(average_montly_hours, last_evaluation,colour=left)) + geom_point() + geom_smooth()

iris
str(iris)

bst = xgb.train(xgb_params,dtrain, nrounds = 20000)#, early_stopping_rounds = 300, watchlist = list(train=dtrain))

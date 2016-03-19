library(randomForest)
library(MASS)
library(rpart)
library(raster)
library(plyr)


lookup <- unique(biomass)
plyr1 <- join(leafTraits, lookup, by = "PLOT_ID")
plyr1 <- cbind(plyr1[1:19], plyr1[25:32], plyr1[35:40])
#plyr1 <- cbind(plyr1["LAI_O"],plyr1["HEIGHTC_m"],plyr1["SLA_HSA"],plyr1["LEAF_CN"],
#               plyr1["SLA_PSA"],plyr1["LEAF_LIFE"], plyr1["MAT_C"],plyr1["MAP"],plyr1["ASA"])
plyr1 <- plyr1[complete.cases(plyr1), ]
str(plyr1)
plyr1["LEAF_LIFE"]<- cut(as.numeric(unlist(plyr1["LEAF_LIFE"])), breaks = 6)

str(plyr1)

train = sample(1:nrow(plyr1), nrow(plyr1)*0.7, replace = FALSE)
test = -train

training_data = plyr1[train,]
testing_data = plyr1[test,]
dtree = randomForest(LEAF_LIFE ~ ., data = training_data, mtry = 6, ntree = 1000)

str(dtree)
p <- predict(dtree, testing_data)
table(testing_data[,6],p)
mean(testing_data[,6]==p)
fichezza = importance(dtree)
getTree(dtree, 1000, labelVar = TRUE)

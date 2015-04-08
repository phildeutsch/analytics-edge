library(caret)
library(caTools)
library(flexclust)

## QQs

g = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
t = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)
sqrt(sum((g-t)**2))

source('Unit6_Netflix.R')
table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

tail(data.frame(x=sort(colMeans(movies[clusterGroups==2,-1]))),1)

## Assignment 1

kos = read.csv("dailykos.csv")
distances = dist(kos, method = "euclidean")
clusterKos = hclust(distances, method = "ward.D") 

plot(clusterKos)

clusterGroups = cutree(clusterKos, k = 7)

hcluster1 = subset(kos, clusterGroups==1)
hcluster2 = subset(kos, clusterGroups==2)
hcluster3 = subset(kos, clusterGroups==3)
hcluster4 = subset(kos, clusterGroups==4)
hcluster5 = subset(kos, clusterGroups==5)
hcluster6 = subset(kos, clusterGroups==6)
hcluster7 = subset(kos, clusterGroups==7)

tail(sort(colMeans(hcluster1)))

tail(sort(colMeans(hcluster2)))

tail(sort(colMeans(hcluster5)))

tail(sort(colMeans(hcluster7)))

set.seed(1000)
kosKmeans = kmeans(kos, 7)
kosCluster = kosKmeans$cluster
sort(table(kosCluster))

kcluster1 = subset(kos, kosCluster==1)
kcluster2 = subset(kos, kosCluster==2)
kcluster3 = subset(kos, kosCluster==3)
kcluster4 = subset(kos, kosCluster==4)
kcluster5 = subset(kos, kosCluster==5)
kcluster6 = subset(kos, kosCluster==6)
kcluster7 = subset(kos, kosCluster==7)

tail(sort(colMeans(kcluster3)))
tail(sort(colMeans(kcluster2)))

table(clusterGroups, kosCluster)

## Assignment 2

airlines = read.csv("AirlinesCluster.csv")
data.frame(x=sort(colMeans(airlines)))

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
data.frame(x=sort(sapply(airlinesNorm, max)))
data.frame(x=sort(sapply(airlinesNorm, min)))

airDist = dist(airlinesNorm, method = "euclidean")
airHclust = hclust(airDist, method = "ward.D") 
plot(airHclust)

clusterGroups = cutree(airHclust, k = 5)
table(clusterGroups)

out = data.frame()
for (n in names(airlines)) {
  out = rbind(out, tapply(airlines[,n], clusterGroups, mean))
}
names(out) = 1:5
out = data.frame(t(out))
names(out) = names(airlines)

set.seed(88)
airKmeans = kmeans(airlinesNorm, 5, iter.max=1000)
airKclust = airKmeans$cluster
sum(table(airKclust)>1000)

out2 = data.frame()
for (n in names(airlines)) {
  out2 = rbind(out2, tapply(airlines[,n], airKclust, mean))
}
names(out2) = 1:5
out2 = data.frame(t(out2))
names(out2) = names(airlines)

## Assignment 3
stocks = read.csv("StocksCluster.csv")
nrow(stocks)
prop.table(table(stocks$PositiveDec>0))

sort(cor(stocks))

sort(colMeans(stocks))

set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec  ~ ., stocksTrain, family="binomial")
StocksModelPred = predict(StocksModel, stocksTrain, type="response")
ct = table(stocksTrain$PositiveDec, StocksModelPred>=0.5)
(ct[1,1]+ct[2,2])/nrow(stocksTrain)

StocksModelPredTest = predict(StocksModel, stocksTest, type="response")
ct = table(stocksTest$PositiveDec, StocksModelPredTest>=0.5)
(ct[1,1]+ct[2,2])/nrow(stocksTest)

sort(table(stocksTrain$PositiveDec))
sum(stocksTest$PositiveDec==1)/nrow(stocksTest)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
km = kmeans(normTrain, 3)
kmClust = km$cluster
table(kmClust)

km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTest1  = subset(stocksTest,  clusterTest==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTest2  = subset(stocksTest,  clusterTest==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)
stocksTest3  = subset(stocksTest,  clusterTest==3)
sort(tapply(stocksTrain$PositiveDec, clusterTrain, mean))

StocksModel1 = glm(PositiveDec~.,stocksTrain1,family="binomial")
StocksModel2 = glm(PositiveDec~.,stocksTrain2,family="binomial")
StocksModel3 = glm(PositiveDec~.,stocksTrain3,family="binomial")
sort( (StocksModel1$coef>0 | StocksModel2$coef>0 | StocksModel3$coef>0) &
      (StocksModel1$coef<0 | StocksModel2$coef<0 | StocksModel3$coef<0))

PredictTest1 = predict(StocksModel1, stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, stocksTest3, type="response")
ct1 = table(stocksTest1$PositiveDec, PredictTest1>=0.5)
(ct1[1,1]+ct1[2,2])/nrow(stocksTest1)
ct2 = table(stocksTest2$PositiveDec, PredictTest2>=0.5)
(ct2[1,1]+ct2[2,2])/nrow(stocksTest2)
ct3 = table(stocksTest3$PositiveDec, PredictTest3>=0.5)
(ct3[1,1]+ct3[2,2])/nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
ct = table(AllOutcomes, AllPredictions>=0.5)
(ct[1,1]+ct[2,2])/length(AllPredictions)

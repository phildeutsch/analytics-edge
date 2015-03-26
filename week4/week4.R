library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)

## QQ1

stevens = read.csv("stevens.csv")
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, 
                    data=Train, method="class", 
                    minbucket=100)
prp(StevensTree)
PredictCART = predict(StevensTree, newdata=Test, type="class")
table(Test$Reverse, PredictCART)
PredictROC = predict(StevensTree, newdata=Test)
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                             data=Train, nodesize=25, ntree=200)
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                             data=Train, nodesize=25, ntree=200)
PredictForest = predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)

set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                             data=Train, nodesize=25, ntree=200)
PredictForest = predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
(43+74)/nrow(Test)

set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                             data=Train, nodesize=25, ntree=200)
PredictForest = predict(StevensForest, newdata=Test)
table(Test$Reverse, PredictForest)
(44+76)/nrow(Test)

numFolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
  data=Train, method="rpart", trControl=numFolds, tuneGrid = cpGrid)
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + LowerCourt + Unconst,
  data = Train, method="class", cp=0.18)
PredictCV = predict(StevensTreeCV, newdata=Test, type="class")
table(Test$Reverse, PredictCV)

prp(StevensTreeCV)

## QQ2
Claims = read.csv("ClaimsData.csv")
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio=0.6)
ClaimsTrain = subset(Claims, spl)
ClaimsTest = subset(Claims, !spl)

mean(ClaimsTrain$age)
prop.table(table(ClaimsTrain$diabetes))

p = rep(1, nrow(ClaimsTest))
table(ClaimsTest$bucket2009, p)
122978/nrow(ClaimsTest)

m = as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))
sum(m*PenaltyMatrix)/nrow(ClaimsTest)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix
m = as.matrix(cbind(table(ClaimsTest$bucket2009, p)[,1], rep(0,5), rep(0,5), rep(0,5), rep(0,5)))
sum(m*PenaltyMatrix)/nrow(ClaimsTest)

## HW 1
gerber = read.csv("gerber.csv")
prop.table(table(gerber$voting))
with(gerber, table(voting, hawthorne))[2,2]/sum(with(gerber, table(voting, hawthorne))[,2])
with(gerber, table(voting, civicduty))[2,2]/sum(with(gerber, table(voting, civicduty))[,2])
with(gerber, table(voting, neighbors))[2,2]/sum(with(gerber, table(voting, neighbors))[,2])
with(gerber, table(voting, self))[2,2]/sum(with(gerber, table(voting, self))[,2])

gerberLR = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
summary(gerberLR)

p = predict(gerberLR, gerber, type="response")
table(gerber$voting, p>=0.3)
(134513+51966)/nrow(gerber)

table(gerber$voting, p>=0.5)
(235388)/nrow(gerber)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)


CARTmodelControl = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodelControl, digits=6)
abs(0.296638-0.34)


CARTmodelSex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodelSex, digits=6)
abs(0.290456-0.302795)
abs(0.334176-0.345818)

gerberLR2 = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(gerberLR2)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
p1 = predict(gerberLR2, newdata=Possibilities, type="response")
p2 = predict(CARTmodelSex, newdata=Possibilities)
round(abs(p1[4]-p2[4]),5)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

p3 = predict(LogModel2, newdata=Possibilities, type="response")
round(abs(p3[4]-p2[4]),5)


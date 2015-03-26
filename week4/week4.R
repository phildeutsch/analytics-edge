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

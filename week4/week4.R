library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(caret)
library(e1071)

## QQ

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

##
numFolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
  data=Train, method="rpart", trControl=numFolds, tuneGrid = cpGrid)
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + LowerCourt + Unconst,
  data = Train, method="class", cp=0.18)
PredictCV = predict(StevensTreeCV, newdata=Test, type="class")
table(Test$Reverse, PredictCV)

prp(StevensTreeCV)

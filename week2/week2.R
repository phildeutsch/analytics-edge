require(zoo)
require(caret)

## Quick questions

## 1
df = read.csv("wine.csv")
fit = lm(Price ~ HarvestRain + WinterRain, df)
summary(fit)

cor(df$HarvestRain, df$WinterRain)

## 2
bb = read.csv("baseball.csv")
moneyball = subset(bb, Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)

predict(WinsReg, data.frame(RD=713-614))

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
predict(RunsReg, data.frame(OBP=0.311,SLG=0.405))

AllowReg = lm(RA ~ OOBP + OSLG, data=moneyball)
predict(AllowReg, data.frame(OOBP=0.297, OSLG=0.370))

players = data.frame(
  Name = c("Eric", "Jeremy", "Frank", "Greg", "Carlos"),
  OBP  = c(0.338, 0.391, 0.369, 0.313, 0.361),
  SLG  = c(0.540, 0.450, 0.374, 0.447, 0.500),
  Salary = c(1.4, 1.065, 0.295, 0.8, 0.3))
players = cbind(players, p = predict(RunsReg, players))
players = merge(players, players, by = NULL)
players$total = players$p.x + players$p.y
players = players[(players$Name.x!=players$Name.y) & (players$Salary.x + players$Salary.y)<=1.5,]
head(players[order(-players$total),c("Name.x", "Name.y")],1)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)

## Assignment 2
climate = read.csv("climate_change.csv")
training = subset(climate, Year <= 2006)
testing = subset(climate, Year > 2006)

fit = lm(Temp ~ . - Year - Month, training)
summary(fit)

cor(training)[,6,drop=FALSE]
cor(training)[,7,drop=FALSE]

fit = lm(Temp ~ MEI + TSI + Aerosols + N2O, training)
summary(fit)

stepfit = step(lm(Temp ~ . - Year - Month, training))
summary(stepfit)
summary(lm(Temp ~ . - Year - Month, training))

p = predict(stepfit, testing)

TSS = with(testing, sum((Temp-mean(training$Temp))**2)) # not sure why we use the training mean
RSS = with(testing, sum((Temp-p)**2))
R2  = 1 - RSS/TSS
R2

R2(p, testing$Temp, formula="traditional")

## Reading test scores
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

nrow(pisaTrain)

with(pisaTrain, tapply(readingScore, male, mean))

sapply(pisaTrain, function(x) sum(is.na(x)))

pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

nrow(pisaTrain)
nrow(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~ ., pisaTrain)
summary(lmScore)

p = predict(lmScore, pisaTrain)
RSS = with(pisaTrain, sum((readingScore-p)**2))
sqrt(RSS/nrow(pisaTrain))

predTest = predict(lmScore, pisaTest)
summary(predTest)

TSS = with(pisaTest, sum((readingScore-mean(pisaTrain$readingScore))**2)) 
RSS = with(pisaTest, sum((readingScore-predTest)**2))
sqrt(RSS/nrow(pisaTest))

mean(pisaTest$readingScore)
TSS


1-RSS/TSS

## Flu epidemics

FluTrain = read.csv("FluTrain.csv")
FluTrain[which.max(FluTrain$ILI),]
FluTrain[which.max(FluTrain$Queries),]

hist(FluTrain$ILI, breaks=50)

plot(log(ILI) ~ Queries, FluTrain)

FluTrend1 = lm(log(ILI) ~ Queries, FluTrain)
summary(FluTrend1)

FluTest = read.csv("FluTest.csv")
PredTest1 = cbind(FluTest, p=exp(predict(FluTrend1, newdata=FluTest)))
head(PredTest1, 20)

with(PredTest1, sqrt(sum((ILI-p)**2)/nrow(PredTest1)))

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
sum(is.na(FluTrain$ILILag2))

plot(ILILag2 ~ ILI, FluTrain)

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]
head(FluTest)
p = exp(predict(FluTrend2, FluTest))
RSS = sum((p-FluTest$ILI)**2)
sqrt(RSS/nrow(FluTest))

library(caTools)
library(ROCR)
library(dplyr)

quality = read.csv("quality.csv")

set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog) 


QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog) 

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

##############

## Popularity of music records

songs = read.csv("songs.csv")

dim(filter(songs, year==2010))

dim(filter(songs, artistname=="Michael Jackson"))

songs %>%
  filter(artistname=="Michael Jackson", Top10==1) %>%
  select(songtitle)

table(songs$timesignature)

songs %>%
  arrange(desc(tempo)) %>%
  select(songtitle, tempo) %>%
  head

SongsTrain = subset(songs, year  < 2010)
SongsTest  = subset(songs, year == 2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
Model1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)

with(SongsTrain, cor(loudness, energy))

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

p = predict(SongsLog3, SongsTest, type="response")
table(SongsTest$Top10, p>=0.45)

p2 = rep(FALSE, nrow(SongsTest))
table(SongsTest$Top10, p2>=0.45)

sum(p>=0.45 & SongsTest$Top10==1)
sum(p>=0.45 & SongsTest$Top10==0)

## Predicting parole violators

parole = read.csv("parole.csv")
dim(parole)
table(parole$violator)

sapply(parole, function(x) length(unique(x)))
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

paroleModel = glm(violator ~ ., data=train, family="binomial")
summary(paroleModel)

p = predict(paroleModel, data.frame(
      male = 1,
      race = 1,
      age = 50,
      state = as.factor(1),
      time.served = 3,
      max.sentence = 12,
      multiple.offenses = 0,
      crime = as.factor(2)
      ), type="response")
p/(1-p)
p

predictTest = predict(paroleModel,test, type="response")
max(predictTest)

table(test$violator, predictTest >= 0.5)
12/(12+11)
167/(167+12)
(12+167)/(167+12+11+12)

table(test$violator, rep(FALSE, nrow(test)))
179/(179+23)

ROCRpredTest = prediction(predictTest, test$violator)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

ROCRperf = performance(ROCRpredTest, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE)
abline(0,1)

# Predicting loan repayment

loans = read.csv("loans.csv")
with(loans, sum(not.fully.paid))/nrow(loans)

summary(loans)
subset(loans, !complete.cases(loans))
with(subset(loans, !complete.cases(loans)), sum(not.fully.paid))/nrow(subset(loans, !complete.cases(loans)))


loans = read.csv("loans_imputed.csv")

set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

loansModel = glm(not.fully.paid ~ ., data=train, family="binomial")
summary(loansModel)

700 * (-9.317e-03)  - 710 * (-9.317e-03)
exp(700 * (-9.317e-03))  / exp( 710 * (-9.317e-03))

test$predicted.risk = predict(loansModel, test, type="response")
with(test, table(not.fully.paid, predicted.risk >= 0.5))
(2400+3)/nrow(test)

with(test, table(not.fully.paid, rep(FALSE, nrow(test))))

ROCRpredTest = prediction(test$predicted.risk, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

loansModelInt = glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(loansModelInt)

p = predict(loansModelInt, test, type="response")
max(p)
sum(p>=0.5)

ROCRpredTest = prediction(p, test$not.fully.paid)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

10 * exp(0.06*3)

test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1

max(10*test$profit)

highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
prop.table(table(highInterest$not.fully.paid))

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)

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

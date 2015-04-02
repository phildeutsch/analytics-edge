library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

## QQs

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
tweets$Negative = as.factor(tweets$Avg <= -1)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus = tm_map(corpus, stemDocument)

frequencies = DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq=20)
sparse = removeSparseTerms(frequencies, 0.995)
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative
set.seed(123)
splt = sample.split(tweetsSparse$Negative, SplitRatio=0.7)
trainSparse = tweetsSparse[splt,]
testSparse = tweetsSparse[!splt,]

findFreqTerms(frequencies, lowfreq=100)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")
prp(tweetCART)
predictCART = predict(tweetCART, testSparse, type="class")
c = table(testSparse$Negative, predictCART)
(c[1,1]+c[2,2])/nrow(testSparse)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse, method="class")
predictRF = predict(tweetRF, testSparse, type="class")
c = table(testSparse$Negative, predictRF)
(c[1,1]+c[2,2])/nrow(testSparse)

tweetLog = glm(Negative ~ ., data=trainSparse, family="binomial")
predictions = predict(tweetLog, newdata=testSparse, type="response")
c = table(testSparse$Negative, predictions>0.5)
(c[1,1]+c[2,2])/nrow(testSparse)

## Assignment 1

wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, PlainTextDocument)
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, PlainTextDocument)
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved) 
wikiWords$Vandal = wiki$Vandal
set.seed(123)
splt = sample.split(wikiWords$Vandal, SplitRatio=0.7)
trainWiki = wikiWords[splt,]
testWiki = wikiWords[!splt,]
table(testWiki$Vandal)[1]/nrow(testWiki)

wikiRT = rpart(Vandal ~., data=trainWiki, method="class")
pRT = predict(wikiRT, testWiki, type="class")
ctRT = table(testWiki$Vandal, pRT)
(ctRT[1,1]+ctRT[2,2])/nrow(testWiki)

prp(wikiRT)

wikiWords2 = wikiWords 
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, splt)
wikiTest2 = subset(wikiWords2, !splt)
wikiRT2 = rpart(Vandal ~., data=wikiTrain2, method="class")
pRT2 = predict(wikiRT2, wikiTest2, type="class")
ctRT2 = table(wikiTest2$Vandal, pRT2)
(ctRT2[1,1]+ctRT2[2,2])/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain2b = subset(wikiWords2, splt)
wikiTest2b = subset(wikiWords2, !splt)
wikiRT2b = rpart(Vandal ~., data=wikiTrain2b, method="class")
pRT2b = predict(wikiRT2b, wikiTest2b, type="class")
ctRT2b = table(wikiTest2b$Vandal, pRT3)
(ctRT2b[1,1]+ctRT2b[2,2])/nrow(wikiTest2b)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, splt)
wikiTest3 = subset(wikiWords3, !splt)
wikiRT3 = rpart(Vandal ~., data=wikiTrain3, method="class")
pRT3 = predict(wikiRT3, wikiTest3, type="class")
ctRT3 = table(wikiTest3$Vandal, pRT3)
(ctRT3[1,1]+ctRT3[2,2])/nrow(wikiTest3)

prp(wikiRT3)

## Assignment 2

trials = read.csv("clinical_trial.csv", stringsAsFactors=FALSE)
max(nchar(trials$abstract))

sum(nchar(trials$abstract)==0)

trials$title[which.min(nchar(trials$title))]

corpusTitle = Corpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
ncol(dtmTitle)
ncol(dtmAbstract)

which.max(colSums(dtmAbstract))

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

set.seed(144)
splt = sample.split(dtm$trial, SplitRatio=0.7)
train = dtm[splt,]
test = dtm[!splt,]
table(train$trial)
table(test$trial)[1]/nrow(test)

trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

ptrialCART = predict(trialCART, train)[,2]
max(ptrialCART)

ct = table(train$trial, ptrialCART>=0.5)
(ct[1,1]+ct[2,2])/nrow(train)
ct[2,2]/(ct[2,1]+ct[2,2])
ct[1,1]/(ct[1,1]+ct[1,2])

ptrialCART2 = predict(trialCART, test)[,2]
ct = table(test$trial, ptrialCART2>=0.5)
(ct[1,1]+ct[2,2])/nrow(test)

pred = prediction(ptrialCART2, test$trial)
perf = performance(pred, "tpr", "fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

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


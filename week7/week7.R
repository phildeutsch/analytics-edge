library(ggplot2)
library(maps)
library(ggmap)
library(igraph)
library(wordcloud)
library(RColorBrewer)

## Assignment 1

statesMap = map_data("state")
str(statesMap)
length(table(statesMap$group))

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 

polling = read.csv("PollingImputed.csv")
Train = subset(polling, Year<=2008)
Test = subset(polling, Year>2008)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
sum(predictionDataFrame$TestPredictionBinary==1)
mean(predictionDataFrame$TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
dim(predictionMap)
dim(statesMap)

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

predictionDataFrame[predictionDataFrame$Test.State=="Florida",]

## Assignment 2
edges = read.csv("edges.csv")
users = read.csv("users.csv")
nrow(users)
2*nrow(edges) / nrow(users)

table(users$locale)

table(users$gender, users$school)

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

sum(degree(g)>=10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
max(degree(g)/2+2)
min(degree(g)/2+2)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

g = graph.data.frame(edges, FALSE, users)
V(g)$size = degree(g)/2+2
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)

g = graph.data.frame(edges, FALSE, users)
V(g)$size = degree(g)/2+2
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA, edge.width=2)

## Assignment 3

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
dim(allTweets)

wordcloud(names(allTweets), colSums(allTweets), scale=c(2, 0.25))

corpus = tm_map(corpus, removeWords, c("apple"))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))
wordcloud(names(allTweets), colSums(allTweets), scale=c(2, 0.25))

display.brewer.all()

wordcloud(names(allTweets), colSums(allTweets), scale=c(2, 0.25),colors=brewer.pal(9, "Blues"))


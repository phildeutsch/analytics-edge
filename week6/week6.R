library(caret)


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
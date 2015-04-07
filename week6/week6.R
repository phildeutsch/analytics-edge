


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

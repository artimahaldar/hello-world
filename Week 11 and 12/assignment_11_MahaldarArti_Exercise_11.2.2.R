# Assignment: ASSIGNMENT 11.2.2
# Name: Mahaldar, Arti
# Date: 2021-11-11

## Set the working directory to the root of your DSC 520 directory
#setwd("/home/jdoe/Workspaces/dsc520")


install.packages("readxl")
library("readxl")


clusteringdata <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/data/clustering-data.csv')

#here is the structure of the data

summary(clusteringdata)

clusteringdata

#PLot Trinarydata
p <- clusteringdata$x
q <- clusteringdata$y


plot(p, q, main="Scatterplot clusteringdata",xlab="X", ylab="Y", pch=25)

kmeans(clusteringdata, 2)

kmeans(clusteringdata, 12)

# Set maximum cluster 
max_k <-20 
# Run algorithm over a range of k 
kmean_withinss <- function(k) 
wss <- sapply(2:max_k, kmean_withinss)

{cluster <- kmeans(clusteringdata, k) 
return (cluster$tot.withinss) 
}
elbow <-data.frame(2:max_k, wss)

install.packages("ggplot")
library("ggplot2")


ggplot(elbow, aes(x = X2.max_k, y = wss)) + geom_point() + geom_line() +scale_x_continuous(breaks = seq(1, 20, by = 1))

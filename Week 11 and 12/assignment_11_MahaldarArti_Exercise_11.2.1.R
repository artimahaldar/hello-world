# Assignment: ASSIGNMENT 11.2.1
# Name: Mahaldar, Arti
# Date: 2021-11-11

## Set the working directory to the root of your DSC 520 directory
#setwd("/home/jdoe/Workspaces/dsc520")


install.packages("readxl")
library("readxl")

#binary-classifier-data.csv

binarydata <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/data/binary-classifier-data.csv')

#here is the structure of the data

summary(binarydata)
binarydata

#trinary-classifier-data.csv

trinarydata <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/data/trinary-classifier-data.csv')

summary(trinarydata)

#Plot binarydata
x <- binarydata$x
y <- binarydata$y

plot(x, y, main="Scatterplot binarydata",xlab="X", ylab="Y", pch=19)

#PLot Trinarydata
a <- trinarydata$x
b <- trinarydata$y

plot(a, b, main="Scatterplot trinarydata",xlab="X", ylab="Y", pch=19)

##Generate a random number that is 90% of the total number of rows in dataset.
ran_binarydata <- sample(1:nrow(binarydata), 0.9 * nrow(binarydata)) 

##the normalization function is created
nor_binarydata <-function(x) { (x -min(x))/(max(x)-min(x))   }

##Run nomalization on first 4 coulumns of dataset because they are the predictors
binarydata_norm <- as.data.frame(lapply(binarydata[,c(2,3)], nor_binarydata))

summary(binarydata_norm)

##extract training set
binary_train <- binarydata_norm[ran_binarydata,] 
##extract testing set
binary_test <- binarydata_norm[-ran_binarydata,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
binary_target_category <- binarydata[ran_binarydata,2]
##extract 5th column if test dataset to measure the accuracy
binary_test_category <- binarydata[-ran_binarydata,2]

binary_train
binary_test
##load the package class
library(class)
##run knn function
pr <- knn(binary_train,binary_test,cl=binary_target_category,k=5)
pr
##create confusion matrix
tab <- table(pr,binary_test_category)
tab
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(pr){sum(diag(pr)/(sum(rowSums(pr)))) * 100}
accuracy(tab)

#TrinaryData set


  ##Generate a random number that is 90% of the total number of rows in dataset.
  ran_trinarydata <- sample(1:nrow(trinarydata), 0.9 * nrow(trinarydata)) 
  
  ##the normalization function is created
  nor_trinarydata <-function(x) { (x -min(x))/(max(x)-min(x))   }
  
  ##Run nomalization on first 4 coulumns of dataset because they are the predictors
  trinarydata_norm <- as.data.frame(lapply(trinarydata[,c(2,3)], nor_trinarydata))

summary(trinarydata_norm)

##extract training set
trinary_train <- trinarydata_norm[ran_trinarydata,] 
##extract testing set
trinary_test <- trinarydata_norm[-ran_trinarydata,] 
##extract 2nd column of train dataset because it will be used as 'cl' argument in knn function.
trinary_target_category <- trinarydata[ran_trinarydata,2]
##extract 5th column if test dataset to measure the accuracy
trinary_test_category <- trinarydata[-ran_trinarydata,2]

##load the package class
library(class)
##run knn function
pr <- knn(trinary_train,trinary_test,cl=trinary_target_category,k=3)
pr
##create confusion matrix
tab <- table(pr,trinary_test_category)
tab
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)



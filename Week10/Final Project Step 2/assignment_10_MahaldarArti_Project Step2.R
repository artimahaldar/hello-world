# Assignment: Final Project Step 2
# Name: Mahaldar, Arti
# Date: 2021-11-04

## Set the working directory to the root of your DSC 520 directory
#setwd("/home/jdoe/Workspaces/dsc520")


install.packages("readxl")
library("readxl")

#the excel sheet below represent Crypto Current Market Cap Data

currencydata <- read_excel('C:/Users/Sandeep Raina/Documents/dsc520/data/Cryptocurrency.xlsx')

#here is the structure of the data

summary(cryptodata)


#Data preparation and cleansing steps.

# 1. Familiarize yourself with the data set

file.info('C:/Users/Sandeep Raina/Documents/dsc520/data/Cryptocurrency.xlsx')$size

#File Size - 33921675 bytes

#an initial look at the data frame
str(cryptodata)

#2 . Check for structural errors  - we’ll evaluate the data frame for structural errors. These include entry errors such as faulty data types, non-unique ID numbers, mislabeled variables, and string inconsistencies. 
#If there are more structural pitfalls in your own dataset than the ones covered below, be sure to include additional steps in your data cleaning to address the idiosyncrasies.

#install.packages("dplyr")
library(dplyr)
cryptodata <- cryptodata %>% rename(CryptoCurrencyname = Currencyname)

#Examine if datatypes are faulty
typeof(cryptodata$MarketCap)

#Non-unique ID numbers - In this dataset uniqueness is not a problem

#3 .Check for data irregularities, like invalid values and outliers. 

summary(cryptodata)

#Data look ok

#4: Decide how to deal with missing values

sum(is.na(cryptodata))

#percent missing values per variable
apply(cryptodata, 2, function(col)sum(is.na(col))/length(col))

#identifying the rows with NAs
cryptodata_NA <- rownames(cryptodata)[apply(cryptodata, 2, anyNA)]

summary(cryptodata_NA)

#removing all observations with NAs
cryptodata_clean <- cryptodata %>% na.omit()

#Clean Data Set
summary(cryptodata_clean)

head(cryptodata_clean)

#Discuss how you plan to uncover new information in the data that is not self-evident.

install.packages("ggplot2")
library(ggplot2)

ggplot(data = cryptodata_clean, aes(x=CryptoCurrencyname,y=Volume)) + geom_boxplot()

max(cryptodata_clean$MarketCap)


install.packages("matrixStats")
library(matrixStats)
cryptodata_clean %>% rowwise() %>% mutate(Maximum_price = max(c(cryptodata_clean$MarketCap)))

#What are different ways you could look at this data to answer the questions you want to answer?


### Call the functions on currencydata_clean to examine the data frame
dim(cryptodata_clean)
str(cryptodata_clean)
summary(cryptodata_clean)
colnames(cryptodata_clean)

### The head() and tail() functions default to 6 rows, but we can adjust the number of rows using the "n = " argument
head(cryptodata_clean, n = 10)
tail(cryptodata_clean, n = 5)

### While the first 6 functions are printed to the console, the View() function opens a table in another window
#View(currencydata_clean)


#Do you plan to slice and dice the data in different ways, create new variables, or join separate data frames to create new summary information? Explain.
library("dplyr")
step_1_df <- select(cryptodata, -Volume)
dim(step_1_df)

head(step_1_df, n = 10)
#Arrange

step_2_df <-step_1_df %>% arrange(step_1_df$MarketCap, step_1_df$CryptoCurrencyname)
head(step_2_df)


#How could you summarize your data to answer key questions?

filter_df <- cryptodata %>% filter(CryptoCurrencyname == "dixasset")
filter_df

summarize(cryptodata,mean(MarketCap,na.rm=TRUE))

cryptodata_Group <- group_by(cryptodata,CryptoCurrencyname)
cryptodata_Group
summarize(cryptodata_filter_Group,avg= mean(MarketCap,na.rm=TRUE))

library(ggplot2)
ggplot(data = cryptodata_Group, aes(x=CryptoCurrencyname,y=Volume)) + geom_boxplot()

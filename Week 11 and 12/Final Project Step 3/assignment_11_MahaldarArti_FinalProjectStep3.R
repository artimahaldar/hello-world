# Assignment: Final Project Step3
# Name: Mahaldar, Arti
# Date: 2021-11-15

## Set the working directory to the root of your DSC 520 directory
#setwd("/home/jdoe/Workspaces/dsc520")

#Introduction:

#Initial appearance of Cryptocurrency/Bitcoin/altcoin happened in 2009.
#Now we know it’s going to stay. Question being addressed is what the right percentage of crypto should be in one’s investment portfolio.


install.packages("readxl")
library("readxl")

#the excel sheet below represent Crypto Current Market Cap Data

currencydata <- read_excel('C:/Users/Sandeep Raina/Documents/dsc520/data/Cryptocurrency.xlsx')

#here is the structure of the data

summary(cryptodata)


#The problem statement addressed. 

#This data set shows the MarketCap for various Crypto Currencies, date wise High low and Open and close.
#This data can help us understand how each curries has changed in last few years and how the market cap has been for each of them.
#That can help us anlayze how volatile each one of them is and answer few research question:
#What are top cryptocurrency to be invested in and why? 
#What mix of cryptocurrency in portfolio is best? To determine risky vs safe
#How much can cryptocurrency appreciate?

str(cryptodata)

#How you addressed this problem statement
#First data needs to be cleaned 
#Check for structural errors  - we’ll evaluate the data frame for structural errors. These include entry errors such as faulty data types, non-unique ID numbers, mislabeled variables, and string inconsistencies. 
#If there are more structural pitfalls in your own dataset than the ones covered below, be sure to include additional steps in your data cleaning to address the idiosyncrasies.

#install.packages("dplyr")
library(dplyr)
cryptodata <- cryptodata %>% rename(CryptoCurrencyname = Currencyname)

#Examine if datatypes are faulty
typeof(cryptodata$MarketCap)

#Non-unique ID numbers - In this dataset uniqueness is not a problem

#Check for data irregularities, like invalid values and outliers. 

summary(cryptodata)

#Data look ok

#Decide how to deal with missing values

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
currencydata_clean %>% rowwise() %>% mutate(Maximum_price = max(c(cryptodata_clean$MarketCap)))

##Summarize how you addressed this problem statement (the data used and the methodology employed, including a recommendation for a model that could be implemented). 

#What are different ways you could look at this data to answer the questions you want to answer?

#There are various steps followed below to uncover the data 

### Call the functions on currencydata_clean to examine the data frame
dim(cryptodata_clean)
str(cryptodata_clean)
summary(cryptodata_clean)
colnames(cryptodata_clean)

##These functions help seeing the patterns within data to answer questions like
##Market volume and users for different cryptocurrency
##Identify Crypto use case and market valuation of use case vs adoption.

### The head() and tail() functions default to 6 rows, but we can adjust the number of rows using the "n = " argument
head(cryptodata_clean, n = 10)
tail(cryptodata_clean, n = 5)



#Do you plan to slice and dice the data in different ways, create new variables, or join separate data frames to create new summary information? Explain.
library("dplyr")
step_1_df <- select(cryptodata, -Volume)
dim(step_1_df)

step_1_df_novolume <- head(step_1_df, n = 10)
#Arrange
str(step_1_df_novolume)


summarize(cryptodata,mean(MarketCap,na.rm=TRUE))

cryptodata_Group <- group_by(cryptodata,Currencyname)
cryptodata_Group
summarize(cryptodata_filter_Group,avg= mean(MarketCap,na.rm=TRUE))
str(cryptodata_Group)
library(ggplot2)
ggplot(data = cryptodata_Group, aes(x=Currencyname,y=Volume)) + geom_boxplot()

## Making plots help visualizing data and making decisions.

##Model Suggestion

## As per the analysis I am trying to do, Regression Model works best for this case as that will help
## Forecast the future as per the problem statement
## Analyze Market returns
## Demand

#How could you summarize your data to answer key questions?

filter_df <- cryptodata %>% filter(Currencyname == "dixasset" | Currencyname == "acoin" | Currencyname == "vector" | Currencyname == "ufocoin" )
filter_df

##Implication

##Running a simple model on the data set to see Marketup predictions


summary(glm(MarketCap ~ 1, data = filter_df))
summary(glm(MarketCap ~ 1, data = cryptodata))
unique(cryptodata$Currencyname)

##Limitation
cryptodata_Group

##Concluding Remarks

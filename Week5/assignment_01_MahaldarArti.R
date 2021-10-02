# Assignment: ASSIGNMENT 5
# Name: Mahaldar, Arti
# Date: 2021-09-29

#Using the dplyr package, use the 6 different operations to analyze/transform the data - 
#GroupBy, Summarize, Mutate, Filter, Select, and Arrange ând Remember this is not just modifying data, you are learning about your data also so play around and start to understand your dataset in more detail

community_survey <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/data/acs-14-1yr-s0201.csv')

library(dplyr)
install.packages("readxl")
library("readxl")
housing <- read_excel('C:/Users/Sandeep Raina/Documents/dsc520/data/week-7-housing.xlsx')

housing

summary(community_survey)

df <- data.frame(housing$`Sale Price`)
apply(df, 2, sum)

#Use the aggregate function on a variable in your dataset

agg = aggregate(housing,by = list(housing$sale_reason),FUN = mean)
agg

#Group By

housinggroup <- group_by(housing, housing$sale_warning)
head(housinggroup,3)

Community_Survey_Group <- group_by(community_survey, Geography)
Community_Survey_Group
#Summarize

housing_Summarize <-summarize(housinggroup,avg= mean(housing$`Sale Price`,na.rm=TRUE))
housing_Summarize

library(plyr)

max_le_by_cont <- ddply(housing, ~ sale_reason, summarize, max_le = max(`Sale Price`))
str(max_le_by_cont)

#mutate
install.packages("lubridate")
library(lubridate)
df %>% mutate(houseage = year(Sys.Date()) - housing$year_built)


#Filter
housing.query1 <- filter(housing, housing$`Sale Price`>=875000)
summary(housing.query1)


#Select

step_1_df <- select(housing, -sitetype)

dim(step_1_df)

#Arrange

step_2_df <-step_1_df %>% arrange(housing$`Sale Price`, housing$sale_warning)
head(step_2_df)

snippet <- subset(housing, housing$`Sale Price`>=875000)
snippet



dat.housing <- mutate(housing, sale_warning = ifelse(sale_warning == "NA","Missing","Not Missing"))
dat.housing
head(dat.housing,3)

#Few Observation about data
summary(housing)


#Using the purrr package perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.

install.packages("purrr")
library(purrr)


x <- list(list(a = 1, b = 2), list(a = 2, b = 1))
x %>% str()

# Creating a dataframe zip_n->transpose()
M <- matrix(1:6, nrow = 2) 

#  Setting the column and row names
t <- t(M) 
t

library(purrr)

rep(10, 10) %>%
  map(sample, 5) %>%
  keep(function(x) mean(x) > 6)

# Or use a formula
rep(10, 10) %>%
  map(sample, 5) %>%
  keep(~ mean(.x) > 6)

# Using a string instead of a function will select all list elements
# where that subelement is TRUE
x <- rerun(5, a = rbernoulli(1), b = sample(10))
x
x %>% keep("a")
x %>% discard("a")

# compact() discards elements that are NULL or that have length zero
list(a = "a", b = NULL, c = integer(0), d = NA, e = list()) %>%
  compact()
    

#Use the cbind and rbind function on your dataset

x <- 1:6
y <- c(1,2)
cbind(x,y)
rbind(x,y)

cbind(community_survey$Geography,community_survey$PopGroupID)
rbind(community_survey$Geography,community_survey$PopGroupID)

#Split a string, then concatenate the results back together
install.packages("stringr")              # Install stringr package
library("stringr")     

quotes <- c(
  "R is really Awesome"
)

str_split(quotes, " ")

split.ing = strsplit(quotes, split=" ")[[1]]
split.ing

paste(split.ing, collapse=" ")

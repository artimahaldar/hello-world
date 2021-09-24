# Assignment: ASSIGNMENT 2
# Name: Mahaldar, Arti
# Date: 2021-09-21

community_survey <- read.csv('dsc520/data/acs-14-1yr-s0201.csv')

(community_survey)
install.packages("readxl")
library("readxl")
housing <- read_excel('dsc520/data/week-7-housing.xlsx')

housing

df <- data.frame(housing$`Sale Price`)
apply(df, 2, sum)

#Use the aggregate function on a variable in your dataset

agg = aggregate(housing,by = list(housing$sale_reason),FUN = mean)
agg

#Use the plyr function on a variable in your dataset more specifically, I want to see you split some data, perform a modification to the data, and then bring it back together

install.packages("plyr")
library(plyr)

snippet <- subset(housing, housing$`Sale Price`>=875000)
snippet
max_le_by_cont <- ddply(housing, ~ sale_reason, summarize, max_le = max(`Sale Price`))
str(max_le_by_cont)

library(dplyr)

housing.query1 <- filter(housing, housing$`Sale Price`>=875000)
summary(housing.query1)


housing.query2 <- filter(housing, housing$`Sale Price`<875000)
summary(housing.query2)


housing.sort1 <- arrange(housing,housing$`Sale Price`<875000 )
head(housing.sort1)


dat.housing <- mutate(housing, sale_warning = ifelse(sale_warning == "NA","Missing","Not Missing"))
dat.housing
head(dat.housing,3)

join(housing.query1, housing.query2, by = NULL, type = "left", match = "all")

summary(housing$sq_ft_lot)

#Looking at the summary function to identify outliers I don't right away see any anomalies in data.

hist(housing$sq_ft_lot,xlab = "sq_ft",main = "Histogram of housing sq fts",breaks = sqrt(nrow(housing)))

#We can also use percentile method with interval for 2.5 and 97.5

lower_bound <- quantile(housing$sq_ft_lot, 0.025)
lower_bound

upper_bound <- median(housing$sq_ft_lot) + 3 * mad(housing$sq_ft_lot, constant = 1)
upper_bound

outlier_ind <- which(housing$sq_ft_lot < lower_bound | housing$sq_ft_lot > upper_bound)

#List of outliers as per Percentile method
outlier_ind

#Create at least 2 new variables

Sys.time()
Sys.Date()
format(Sys.Date(), "%Y")

# Calculating age of the house

install.packages("lubridate")
library(lubridate)
df %>% mutate(houseage = year(Sys.Date()) - housing$year_built)

Var2 <- ifelse((housing$year_renovated == 0), "Never Renovated", "Renovated")
Var2

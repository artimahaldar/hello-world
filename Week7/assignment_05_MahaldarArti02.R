## Assignment: ASSIGNMENT 5 -02
## Name: Mahaldar, Arti
## Date: 2021-10-11


## Load the `data/student-survey.csv` to
studentsurvey <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/data/student-survey.csv')
str(studentsurvey)


#2. Examine the Survey data variables. What measurement is being used for the variables? Explain what effect changing the measurement being used for the variables would have on the covariance calculation. Would this be a problem? Explain and provide a better alternative if needed.

str(studentsurvey)
summary(studentsurvey)

# In order to use describe on package
install.packages("psych")


library(psych) 

#describe(studentsurvey)

#correlation
cor(studentsurvey)

class(studentsurvey$TimeReading)

class(studentsurvey$TimeTV)
sapply(studentsurvey,class)

#Converting the class 

studentsurvey$TimeReading.f = factor(studentsurvey$TimeReading/60,ordered = TRUE)
cor(studentsurvey)

#3. Choose the type of correlation test to perform, explain why you chose this test, and make a prediction if the test yields a positive or negative correlation?

#Correlation method can be pearson, spearman or kendall

#The pearson correlation coefficient measure the linear dependence between two variables.

#If method is “kendall” or “spearman”, Kendall’s tau or Spearman’s rho statistic is used to estimate a rank-based measure of association. These are more robust and have been recommended if the data do not come from a bivariate normal distribution.

pearson_test<-cor.test(studentsurvey$TimeReading, studentsurvey$TimeTV/60, method="pearson")
pearson_test

spearman_test<-cor.test(studentsurvey$TimeReading, studentsurvey$TimeTV/60, method="spearman",exact=FALSE)

spearman_test

plot(studentsurvey$TimeReading, studentsurvey$TimeTV/60)

#Looking at the plot Pearson_test method sounds the best for this

#4. Perform a correlation analysis of:

#All Variables

studentsurvey <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/datastudent-survey.csv')
cor(studentsurvey)

## TimeReading vs. TimeTV
cor(studentsurvey$TimeReading, studentsurvey$TimeTV/60, method = c("pearson", "kendall", "spearman"))
### TimeReading vs. Happiness
cor(studentsurvey$TimeReading, studentsurvey$Happiness, method = c("pearson", "kendall", "spearman"))
### TimeReading vs. Gender
cor(studentsurvey$TimeReading, studentsurvey$Gender, method = c("pearson", "kendall", "spearman"))

#Repeat your correlation test in step 2 but set the confidence interval at 99%
timereadingvstimetv<-cor.test(studentsurvey$TimeReading, studentsurvey$TimeTV/60,method = c("pearson", "kendall", "spearman"),exact = NULL, conf.level = 0.95, continuity = FALSE)

#Describe what the calculations in the correlation matrix suggest about the relationship between the variables. Be specific with your explanation.
names(timereadingvstimetv)
timereadingvstimetv$conf.int
timereadingvstimetv$p.value
timereadingvstimetv$method
timereadingvstimetv$null.value
timereadingvstimetv$statistic

#Most variables show very little relationship and negative correlationship

#Calculate the correlation coefficient and the coefficient of determination, describe what you conclude about the results.
cofofdet <- lm( studentsurvey$Gender ~ studentsurvey$TimeReading + TimeTV + Happiness, data=studentsurvey)
summary(cofofdet)

#Based on your analysis can you say that watching more TV caused students to read less? Explain.

#Yes, That what it looks like

#Pick three variables and perform a partial correlation, documenting which variable you are “controlling”. Explain how this changes your interpretation and explanation of the results.
install.packages('ppcor')
install.packages('MASS')
library(ppcor)
pcor(studentsurvey)

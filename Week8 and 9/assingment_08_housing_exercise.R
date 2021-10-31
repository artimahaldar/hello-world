# Assignment: ASSIGNMENT Housing
# Name: Mahaldar, Arti
# Date: 2021-10-23

## Set the working directory to the root of your DSC 520 directory
#setwd("/home/jdoe/Workspaces/dsc520")


## Load the `data/r4ds/heights.csv` to
install.packages("readxl")
library(readxl)
housing_df <- read_excel('C:/Users/Sandeep Raina/Documents/dsc520/data/week-7-housing.xlsx')
head(housing_df)

#Create two variables; one that will contain the variables Sale Price and Square Foot of Lot (same variables used from previous assignment on simple regression) and one that will contain Sale Price and several additional predictors of your choice. Explain the basis for your additional predictor selections.


df_var1 <- lm(formula = housing_df$`Sale Price` ~ housing_df$sq_ft_lot, data = housing_df)
df_var1

df_var2 <- lm(formula = housing_df$`Sale Price` ~ housing_df$sq_ft_lot + housing_df$year_built + housing_df$sale_reason, data = housing_df)
df_var2

summary(df_var1)
summary(df_var2)

#Execute a summary() function on two variables defined in the previous step to compare the model results. What are the R2 and Adjusted R2 statistics? Explain what these results tell you about the overall model. Did the inclusion of the additional predictors help explain any large variations found in Sale Price?

# standardize
install.packages("lm.beta")
library(lm.beta)
lm.beta <- lm.beta(df_var2)
print(lm.beta)
summary(lm.beta)
coef(lm.beta)

#Calculate the confidence intervals for the parameters in your model and explain what the results indicate.


confint(df_var2, level=0.95)

#Results:
  
2.5 %        97.5 %
  (Intercept)            -1.196086e+07 -1.039160e+07
housing_df$sq_ft_lot    9.972488e-01  1.233559e+00
housing_df$year_built   5.544318e+03  6.330861e+03
housing_df$sale_reason -1.635320e+04 -1.133365e+04

#Assess the improvement of the new model compared to your original model (simple regression model) by testing whether this change is significant by performing an analysis of variance.

anova(df_var1)
anova(df_var2)

#Perform casewise diagnostics to identify outliers and/or influential cases, storing each function's output in a dataframe assigned to a unique variable name.

#outliers can be detected by very simply min and max values from the summary function

summary(df_var1)
summary(df_var2)

#Calculate the standardized residuals using the appropriate command, specifying those that are +-2, storing the results of large residuals in a variable you create.

predicted_df1 <- data.frame(Sale = predict(df_var1))
predicted_df2 <- data.frame(Sale = predict(df_var2))

head(predicted_df1)

## Residuals
#residuals_var1 <- housing_df$`Sale Price` - predicted_df1$Sale
residuals_var1 <- residuals(df_var1)
#residuals_var2 <- housing_df$`Sale Price` - predicted_df2$Sale
residuals_var1
residuals_var2 <- residuals(df_var2)

residuals_var2

#Use the appropriate function to show the sum of large residuals.

residuals_var1_sq_sum <- sum(residuals_var1^2)

residuals_var2_sq_sum <- sum(residuals_var2^2)

#Which specific variables have large residuals (only cases that evaluate as TRUE)?

residuals_var1_sq_sum

residuals_var2_sq_sum

#Investigate further by calculating the leverage, cooks distance, and covariance rations. Comment on all cases that are problematics.

hats_1 <- as.data.frame(hatvalues(df_var1))
hats_2 <- as.data.frame(hatvalues(df_var2))

max(hats_1)
max(hats_2)

install.packages("tinytex")


install.packages("predictmeans")
library(predictmeans)

CookD_var1 <- CookD(df_var1)
CookD_var1

CookD_var2 <- CookD(df_var2)

CookD_var2


#Perform the necessary calculations to assess the assumption of independence and state if the condition is met or not.
library(MASS)
tbl = table(housing_df$`Sale Price`, housing_df$sq_ft_lot) 
tbl
chisq.test(tbl) 


#Perform the necessary calculations to assess the assumption of no multicollinearity and state if the condition is met or not.
#install.packages("olsrr")
library(olsrr)
ols_coll_diag(df_var2)

#Visually check the assumptions related to the residuals using the plot() and hist() functions. Summarize what each graph is informing you of and if any anomalies are present.

plot(df_var1)
plot(df_var2)

#Overall, is this regression model unbiased? If an unbiased regression model, what does this tell us about the sample vs. the entire population model?

#install.packages("biadbetareg")
bias(df_var1, parameter = NULL, relative = FALSE)
bias(df_var2, parameter = NULL, relative = FALSE)

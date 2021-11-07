# Assignment: ASSIGNMENT 10.2.2 
# Name: Mahaldar, Arti
# Date: 2021-11-04

## Set the working directory to the root of your DSC 520 directory
#setwd("/home/jdoe/Workspaces/dsc520")

binary_classifier_data <- read.csv('C:/Users/Sandeep Raina/Documents/dsc520/data/binary-classifier-data.csv')

View (binary_classifier_data)

#Check bias
table(binary_classifier_data$label)

# Create Training Data
input_ones <- binary_classifier_data[which(binary_classifier_data$label == 1), ]  # all 1's
input_zeros <- binary_classifier_data[which(binary_classifier_data$label == 0), ]  # all 0's
set.seed(100)  # for repeatability of samples

#Model fitting

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))  # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
input_ones_training_rows

set.seed(42)
default_idx = sample(nrow(binary_classifier_data), 500)
default_trn = binary_classifier_data[default_idx, ]
default_tst = binary_classifier_data[-default_idx, ]


model_glm = glm(label ~ x, data = default_trn, family = "binomial")
model_glm
summary(model_glm)

coef(model_glm)
head(predict(model_glm))

head(predict(model_glm, type = "link"))


head(predict(model_glm, type = "response"))


model_glm_pred = ifelse(predict(model_glm, type = "link") > 0, "Yes", "No")
model_glm_pred = predict(model_glm, type = "response")

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err(actual = default_trn$default, predicted = model_glm_pred)

model_glm
model_glm_pred

train_tab = table(predicted = model_glm_pred, actual = default_trn$label)
train_tab = table(predicted = model_glm_pred, actual = model_glm)

install.packages("caret")
library(caret)

train_tab
confusionMatrix(train_tab)

confusionMatrix(data=train_tab, reference = default_trn)

confusionMatrix( table(prediction, true_value) )
train_con_mat = confusionMatrix(train_tab, positive = "Yes")

c(train_con_mat$overall["Accuracy"], 
  train_con_mat$byClass["Sensitivity"], 
  train_con_mat$byClass["Specificity"])

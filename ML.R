library(readr)
library(MLmetrics)
inclaims <- read_csv("~/Downloads/claims.csv")

#Splitting our data 80/20
split <- round(nrow(inclaims) * .80)

# Create train
train <- inclaims[1:split,]

# Create test
test <- inclaims[(split + 1):nrow(inclaims),]

#Model
reg <- lm(fraud_reported ~ ., data = train)
summary(reg)

#Prediction for Train data set
train$prediction <- round(predict(reg, train))

#Recall Score for Training Data
Recall_train <- Recall(train$fraud_reported, train$prediction, positive = NULL)

#Precision for Training data set
Precision_train <- Precision(train$fraud_reported, train$prediction, positive = NULL)

#F1 Score for Training data set
f1_train <- F1_Score(y_pred = train$prediction, y_true = train$fraud_reported)

#Prediction for Test data set
test$prediction <- round(predict(reg, test))

#Recall Score for Test Data
Recall_test <- Recall(test$fraud_reported, test$prediction, positive = NULL)

#Precision for Test data set
Precision_test <- Precision(test$fraud_reported, test$prediction, positive = NULL)

#F1 Score for Test data set
f1_test <- F1_Score(y_pred = test$prediction, y_true = test$fraud_reported)

#Checking for Overfitting
Recall_Overfit  <- Recall_train - Recall_test
Precision_Overfit <- Precision_train - Precision_test
F1_Overfit <- f1_train - f1_test

#confusion Matrix for train
train_matrix <- table(train$fraud_reported, train$prediction)
train_matrix

#confusion Matrix for Test
test_matrix <- table(test$fraud_reported, test$prediction)
test_matrix

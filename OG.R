df <-read.csv("~/Downloads/inclaims_updated.csv")
install.packages("caret")
library(caret)
View(df)


#Spliting our data 80/20
split <- round(nrow(df) * .80)

# Create train
train <- df[1:split,]

# Create test
test <- df[(split + 1):nrow(df),]

# dimensions of dataset
dim(df)

# list types for each attribute
sapply(df, class)

# summarize attribute distributions
summary(df)

#Assesing fraud rate
mean(df$fraud_reported)
#24.7% fraud
count <- table(df$fraud_reported)
barplot(count)

data <- as.matrix(df)
heatmap(data)

#looking at relationships
hobbies_reg <- lm(fraud_reported ~ insured_hobbies , data = df)
summary(hobbies_reg)
#high fraud rate chess and crossfit 

reg <- lm(fraud_reported ~ ., data = df)
summary(reg)
mc_reg <- lm(fraud_reported ~ months_as_customer , data = df)
summary(mc_reg)

is_reg <- lm(fraud_reported ~ incident_severity , data = df)
summary(is_reg)

#Bar chart for assesment
count <- table(df$fraud_reported, df$policy_state)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$insured_education_level)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$insured_relationship)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$incident_type)
barplot(count, beside = TRUE, legend = rownames(count))

#worth looking into:
count <- table(df$fraud_reported, df$incident_severity)
barplot(count, beside = TRUE, legend = rownames(count))
reg <- lm(fraud_reported ~ incident_severity + insured_hobbies + insured_education_level, data=df)
summary(reg)

reg <- lm(fraud_reported ~ incident_severity, data=df)



count <- table(df$fraud_reported, df$number_of_vehicles_involved)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$property_damage)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$witnesses)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$police_report_available)
barplot(count, beside = TRUE, legend = rownames(count))

mean(df$total_claim_amount)
count <- table(df$total_claim_amount, m)
barplot(count, beside = TRUE, legend = rownames(count))

df1 <- data.frame(df)
#Spliting our data 80/20
split1 <- round(nrow(df1) * .80)

# Create train
train1 <- df1[1:split,]

# Create test
test1 <- df1[(split + 1):nrow(df1),]

model1 <- train(fraud_reported_n ~ ., data = df,
            method = "lm",
            na.action = na.omit,
            preProcess = c("scale","center"),
            colClasses=c("Class"="character"),
            trControl = trainControl(method = "none"))
View(train)



#Question 4
install.packages("kernlab")
install.packages("e1071")

install.packages("e1071")
install.packages("kernlab")

library(e1071)
library(kernlab)


data(spam)
dim(train)
head(train)
set.seed(02115)

sample <- sample( c(TRUE, FALSE), nrow(df), replace=TRUE)
train <- df[sample,]
test <- df[!sample,]
View(test)

#4.1:
svmfit <-svm( fraud_reported_n ~.,data=train,kernel="linear",cost=1,scale=FALSE)
svmfit
#Fitting a svm to the data we get a model with 402 support vectors.

#4.2:
pred <-predict(svmfit, train)
table(pred)
#The predicted classes on the test data are 1222 for non-spam and 1043 for spam.
#How does this compare to the true classes?
table(Predict=pred,Truth=train$fraud_reported_n)
#We can see that we get several mis-classifications. We predict 189 emails would be spam that are in fact not spam (false positive) and there are 53 instances in which our classifier would “miss” spam emails (false negative).
#In total, we get 242 classification errors.

#4.3:
svmfit <-svm( fraud_reported_n ~.,data=train,kernel="linear",cost=.01,scale=FALSE)
pred <-predict(svmfit, train)
table(Predict=pred,Truth=train$fraud_reported_n)

#4.4:
tuned <-tune.svm(type ~.,data =train,gamma =10^(-3:-1),cost =c(1,5,10,100)) # tune
summary(tuned) # to select best gamma and cost


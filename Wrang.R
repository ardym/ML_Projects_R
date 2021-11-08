df <-read.csv("~/Downloads/inclaims_updated.csv")
library(caret)
View(df)

# dimensions of dataset
dim(df)

# list types for each attribute
sapply(df, class)

# summarize attribute distributions
summary(df)

#check for null values
is.null(df)

#Assessing fraud rate
mean(df$fraud_reported)
#24.7% fraud
count <- table(df$fraud_reported)
barplot(count)

#looking at relationships
hobbies_reg <- lm(fraud_reported_n ~ insured_hobbies , data = df)
summary(hobbies_reg)

mc_reg <- lm(fraud_reported_n ~ months_as_customer , data = df)
summary(mc_reg)

is_reg <- lm(fraud_reported_n ~ incident_severity , data = df)
summary(is_reg)

#Bar chart for assessment
count <- table(df$fraud_reported, df$policy_state)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$insured_education_level)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$insured_relationship)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$incident_type)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$incident_severity)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$number_of_vehicles_involved)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$property_damage)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$witnesses)
barplot(count, beside = TRUE, legend = rownames(count))

count <- table(df$fraud_reported, df$police_report_available)
barplot(count, beside = TRUE, legend = rownames(count))

mean(df$total_claim_amount)
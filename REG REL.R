inclaims <- read_csv("~/Downloads/claims.csv")

#fraud report bar chart
ggplot(data = inclaims, aes(x=fraud_reported, fill =fraud_reported )) + geom_bar()
count <-table(inclaims$fraud_reported)
library(plyr)
download.packages("Mlmetrics")
library(MLmetrics)

count(inclaims, vars = "fraud_reported")

#Hobbies
hobbies_reg <- lm(fraud_reported_n ~ insured_hobbies, data = inclaims)
summary(hobbies_reg)
ggplot(data = inclaims, aes(x=insured_hobbies, fill =fraud_reported )) + geom_bar()

#Incident Severity
incident_reg <- lm(fraud_reported_n ~ incident_severity, data=inclaims)
summary(incident_reg)
ggplot(data = inclaims, aes(x=incident_severity, fill =fraud_reported )) + geom_bar()

#relationship
relationship_reg <- lm(fraud_reported_n ~ insured_relationship, data=inclaims)
summary(relationship_reg)
ggplot(data = inclaims, aes(x=insured_relationship, fill =fraud_reported )) + geom_bar()

#authorities_contacted
authorities_reg <- lm(fraud_reported_n ~ authorities_contacted, data=inclaims)
summary(authorities_reg)
ggplot(data = inclaims, aes(x=authorities_contacted, fill =fraud_reported )) + geom_bar()
                                                                                               
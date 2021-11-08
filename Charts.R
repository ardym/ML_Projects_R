inclaims <- read_csv("~/Downloads/claims.csv")
library(plyr)
library(MLmetrics)

#fraud report bar chart
ggplot(data = inclaims, aes(x=fraud_reported, fill =fraud_reported )) + geom_bar()
count <-table(inclaims$fraud_reported)

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

#Correlation Matrix
mydata <- inclaims[, c(1,2,32,33,34,41,40)]
data.frame(colnames(inclaims$total_claim_amount))
cormat <- round(cor(mydata),2)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

                                                                                               
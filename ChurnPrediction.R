#churn_data<-read.csv("Z:/Statistical learning/package/churn_pred_data.csv", header=T, na.strings=c("","NA"))
churn_data<-read.csv("E:/semester 5/churn predictionj/WA_Fn-UseC_-Telco-Customer-Churn.csv", header=T, na.strings=c("","NA"))
attach(churn_data)
#-----------------------------------------------------------------
#Data overview :

head(churn_data)
cat ("Rows     : "  , dim(churn_data)[1])
cat ("\nColumns  : " ,  dim(churn_data)[2] ,"\n")
print("Features : " )
print(names(churn_data))
sapply(churn_data, class)

#------------------------Plots of all categorical variables)------------------------------------
library(gridExtra)
library(ggthemes)
p1 <- ggplot(churn_data, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p2 <- ggplot(churn_data, aes(x=SeniorCitizen)) + ggtitle("Senior Citizen") + xlab("Senior Citizen") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()

p3 <- ggplot(churn_data, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p4 <- ggplot(churn_data, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p1, p2, p3, p4, ncol=2)


p5 <- ggplot(churn_data, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p6 <- ggplot(churn_data, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p7 <- ggplot(churn_data, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p8 <- ggplot(churn_data, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p5, p6, p7, p8, ncol=2)


p9 <- ggplot(churn_data, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p10 <- ggplot(churn_data, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p11 <- ggplot(churn_data, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p12 <- ggplot(churn_data, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p9, p10, p11, p12, ncol=2)



p13 <- ggplot(churn_data, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p14 <- ggplot(churn_data, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p15 <- ggplot(churn_data, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
p16 <- ggplot(churn_data, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(p13, p14, p15, p16, ncol=2)




#------------------------------Tenure and Monthly charges Histogram -----------------

par(mfrow=c(2,2))
b<-hist(tenure,xlim=c(0,80),breaks=10,main='Tenure freq histo',ylab='freq',xlab='Tenure',col=2)
text(b$mids,b$counts,labels=b$counts, adj=c(0.5, 1))

e<-hist(TotalCharges,xlim=c(0,10000),breaks=8,main='Total Charges freq histo',ylab='freq',xlab='Total Charges',col=3 )
text(e$mids,e$counts,labels=e$counts, adj=c(0.5,1))

t<-hist(MonthlyCharges,xlim=c(0,120),breaks=8, main='Monthly charges freq histo',ylab='freq',xlab='Monthly charges',col=5)
text(t$mids,t$counts,labels=t$counts, adj=c(0.5, 1))

# Seems from the freq histograms that the large cluster of customers have high total charges (2904 customers). 
# A large cluster of customers have a low teneure count (1970 customers).
# Monthly charges seem to be less skewed , but its interesting to see two extremely low clusters.
#-----------------------------------------------------------------
library(dplyr)

churn_data %>% ggplot(aes(x=MonthlyCharges,fill=Churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('green','blue'))+labs(title='Monthly Charges density split churn vs non churn' )

churn_data %>% ggplot(aes(x=tenure,fill=Churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('pink','purple'))+labs(title='tenure density split churn vs non churn' )


#------------------------------------------------------------------
#Exploratory data analysis 
#install.packages("dplyr")
#install.packages("miscset")

library(ggplot2)

#Gender
ggplot(churn_data) +
  geom_bar(aes(x = churn_data$gender, fill = churn_data$Churn), position = "dodge")



#senior citizen 
ggplot(churn_data) +
  geom_bar(aes(x = churn_data$SeniorCitizen, fill = churn_data$Churn), position = "dodge")



#Partner
ggplot(churn_data) +
  geom_bar(aes(x=churn_data$Partner, fill = churn_data$Churn), position = "dodge")



#dependents
ggplot(churn_data) +
  geom_bar(aes_string(x = churn_data$Dependents, fill=churn_data$Churn), position = "dodge")
------------------------------------------------------------------
#Handling Missing values
any(is.na(churn_data))
list_na <- colnames(churn_data)[ apply(churn_data, 2, anyNA) ]
cat ("Column which has null value :" , list_na)
#plot missing values
#install.packages("Amelia")
library(Amelia)
missmap(churn_data,col=c("Yellow","Red")) #shows missing datapoints

#Mean imputation
#install.packages("Hmisc")

library(Hmisc)
churn_data$TotalCharges <- impute(churn_data$TotalCharges,mean)
any(is.na(churn_data))
#churn_data <- na.omit(churn_data)






#----------------------Data Wrangling ---------------------------------------------

#Data manipulation
#install.packages("plyr")
library(plyr)
cols_revalue1 <- c(10:15)
for(i in 1:ncol(churn_data[,cols_revalue1])) {
  churn_data[,cols_revalue1][,i] <- as.factor(mapvalues
                                              (churn_data[,cols_revalue1][,i], from =c("No internet service"),to=c("No")))
}

churn_data$MultipleLines <- as.factor(mapvalues(churn_data$MultipleLines,
                                                from=c("No phone service"),
                                                to=c("No")))

group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
churn_data$tenure_group <- sapply(churn_data$tenure,group_tenure)
churn_data$tenure_group <- as.factor(churn_data$tenure_group)


churn_data$SeniorCitizen <- as.factor(mapvalues(churn_data$SeniorCitizen,
                                                from=c("0","1"),
                                                to=c("No", "Yes")))

#Finding correlation between variables
numeric_var <- sapply(churn_data, is.numeric)
corr_matrix <- cor(churn_data[,numeric_var])

#install.packages("corrplot")
library(corrplot)
corrplot(corr_matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

churn_data$TotalCharges <- NULL
churn_data$customerID <- NULL
churn_data$tenure <- NULL



#kfold cross validation
#install.packages("caret")
library(caret)
set.seed(5)
intraining<-createDataPartition(y=churn_data$Churn,p=0.50,list=FALSE)
train_data<-churn_data[intraining,]
test_data<-churn_data[-intraining,]
test_churn<-test_data$Churn
test_data<-test_data[,-18]
#nrow(test_data)
nrow(train_data)


#---------------------------------------------------------------------------------
#Logistic Regression using validation set approach
LogModel <- glm(train_data$Churn ~ .,family=binomial(link="logit"),data=train_data)
print(summary(LogModel))
fit_prob<-predict(LogModel,test_data,type = "response")
# converting probabilities to "Yes" and "No"
glm_pred = rep("No", length(fit_prob))
glm_pred[fit_prob > 0.5] = "Yes"
glm_pred <- as.factor(glm_pred)
#confusion matrix
library(e1071)
a = confusionMatrix(glm_pred, test_churn, positive = "Yes")
glm_accuracy <-a$overall[c(1,3,4)]
glm_accuracy

#install.packages("ROCR")
library(ROCR)
#prediction object from ROCR
pred <- prediction(fit_prob, test_churn)
# plotting ROC curve
prf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(prf)
#plotting auc
# AUC value
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#log Ratio
#exp(confint(LogModel))


#---------------------------------------------Random Forest ---------------------------------------------
library(randomForest)
fit <- randomForest(as.factor(train_data$Churn) ~ train_data$gender+train_data$SeniorCitizen+train_data$Partner+train_data$Dependents+train_data$PhoneService+train_data$MultipleLines+train_data$InternetService+train_data$OnlineSecurity+train_data$StreamingTV+train_data$StreamingMovies+train_data$Contract+train_data$PaperlessBilling+train_data$PaymentMethod+train_data$MonthlyCharges,
                    data=train_data,
                    importance=TRUE,
                    ntree=2000)
summary(fit)
rf_accuracy <- 100-22.43
rf_accuracy


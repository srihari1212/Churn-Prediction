churn_data<-read.csv("C:/Users/Venkat/Documents/semester 8/statistical learning lab/Package/WA_Fn-UseC_-Telco-Customer-Churn.csv", header=T, na.strings=c("","NA"))
attach(churn_data)
#churn_data[,18]
#-----------------------------------------------------------------
#Data overview :
#install.packages("plyr")
head(churn_data)
cat ("Rows     : "  , dim(churn_data)[1])
cat ("\nColumns  : " ,  dim(churn_data)[2] ,"\n")
print("Features : " )
print(names(churn_data))
sapply(churn_data, class)
#Handling Missing values
any(is.na(churn_data))
list_na <- colnames(churn_data)[ apply(churn_data, 2, anyNA) ]
cat ("Column which has null value :" , list_na)
#plot missing values
#install.packages("Amelia")


#Mean imputation
#install.packages("Hmisc")
#install.packages("mice")
library(mice)
library(Hmisc)
library(plyr)
library(ggplot2)
#library(Hmisc)
class(churn_data$TotalCharges)
churn_data$TotalCharges <- impute(churn_data$TotalCharges,mean)
any(is.na(churn_data))
#churn_data <- na.omit(churn_data)
#-------------------------------------------------------------------

#Data manipulation or data wrangling 
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

#install.packages("caret")
#install.packages("corrplot")
#install.packages("e1071")
#install.packages("randomForest")
library(corrplot)
library(e1071)
library(caret)
library(randomForest)
corrplot(corr_matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

churn_data$TotalCharges <- NULL
churn_data$customerID <- NULL
churn_data$tenure <- NULL
#install.packages("e1071")
library(e1071)
#install.packages("randomForest")
library(randomForest)
library(caret)
set.seed(5)
intraining<-createDataPartition(y=churn_data$Churn,p=0.70,list=FALSE)
train_data<-churn_data[intraining,]
names(train_data)
test_data<-churn_data[-intraining,]
test_data$Churn
test_churn<-test_data$Churn


test_data$Churn = test_churn
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  savePredictions = 'final',
  summaryFunction = twoClassSummary,
  classProbs = T)



predictors<-c("gender","SeniorCitizen","Partner","Dependents","PhoneService","MultipleLines","InternetService","OnlineSecurity","OnlineBackup","DeviceProtection","TechSupport","StreamingTV","StreamingMovies","Contract","PaperlessBilling","PaymentMethod","MonthlyCharges",  "tenure_group")
outcomeName<-'Churn'


#--------------------------Random Forest--------------------------------------
model_rf<-train(train_data[,predictors],train_data[,outcomeName],method='rf',trControl=fitControl,tuneLength=3)
predict_rf<-predict(object = model_rf,test_data[,predictors])

rf_cm<-confusionMatrix(test_churn,predict_rf)
rf_accuracy <-rf_cm$overall[c(1,3,4)]


#-------------------------Logistic Regression ----------------------------------------------

model_glm<-train(train_data[,predictors],train_data[,outcomeName],method='glm',trControl=fitControl,tuneLength=3)
predict_glm<-predict(object = model_glm,test_data[,predictors])
#test_data$churn
glm_cm<-confusionMatrix(test_churn,predict_glm)
glm_accuracy <-glm_cm$overall[c(1,3,4)]

#------------------------------GBM--------------------------------------------------
model_gbm<-train(train_data[,predictors],train_data[,outcomeName],method='gbm',trControl=fitControl,tuneLength=3)
predict_gbm<-predict(object = model_gbm,test_data[,predictors])
gbm_cm<-confusionMatrix(test_churn,predict_gbm)
gbm_accuracy <-gbm_cm$overall[c(1,3,4)]

#-------------------------Ridge and lasso Penalization---------------------------------------



myGrid <- expand.grid(
  alpha = 0:1,
  lambda = seq(0.0001, 0.1, length = 10)
)

set.seed(42)
model <- train(
  Churn ~., churn_data,
  method = "glmnet",
  tuneGrid = myGrid, 
  trControl =fitControl
  )


library(e1071)
library(caret)
predict_rl<-predict(object = model,test_data[,predictors])
lr = confusionMatrix(test_churn,predict_rl)
lraccuracy <-lr$overall[c(1,3,4)]

#-----------------------------Elastic net regression---------------------------------------------
glmnet_model <- train(Churn ~., churn_data, 
                      metric = "ROC",
                      method = "glmnet",
                      trControl = fitControl,
                      preProcess = c("center","scale")
)

plot(glmnet_model)
glmnet_model$bestTune$alpha
glmnet_pred <- predict(glmnet_model, newdata = test_data)

glmnetcm <- confusionMatrix(glmnet_pred, test_churn)
glmnetaccuracy <- glmnetcm$overall[c(1,3,4)]
glmnetcm 

#----------------------------------KNN------------------------------------------------
knn_model <- train(Churn ~ ., data = churn_data, 
                   method = "knn", trControl = fitControl,
                   preProcess = c("center","scale"), tuneLength = 10, metric = "ROC")
#prediction
knn_pred <- predict(knn_model, newdata = test_data)
knncm <- confusionMatrix(knn_pred, test_churn)
knnaccuracy <- knncm$overall[c(1,3,4)]

#---------------------------------SVM------------------------------------------------
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 1))
#install.packages("kernlab")
library(kernlab)
svm_linear_model <- train(Churn ~., data = churn_data, method = "svmLinear",
                          trControl= fitControl,
                          preProcess = c("center", "scale"),
                          tuneLength = 6,
                          tuneGrid = grid,
                          metric = "ROC")

#svm_linear_model
plot(svm_linear_model, main = "Cross validation to determine cost parameter")
svm_linear_pred <- predict(svm_linear_model, newdata = test_data)
svmcm <- confusionMatrix(svm_linear_pred, test_churn)
svm_linearaccuracy <- svmcm$overall[c(1,3,4)]

#-------------------------------------------------------------------------------------


#__________________________________Ensemble 1) Averaging_______________________________
test_data$rf_prob<-predict(object = model_rf,test_data[,predictors],type = "prob")

test_data$glm_prob<-predict(object = model_glm,test_data[,predictors],type = "prob")

test_data$lassoR<-predict(object = model,test_data[,predictors],type = "prob")

test_data$Elastic<- predict(glmnet_model, newdata = test_data,type = "prob")

test_data$gbm_prob<-predict(object = model_gbm,test_data[,predictors],type = "prob")

test_data$Knn_prob<-predict(knn_model, newdata = test_data , type = "prob")

svm_linear_pred$svm_prob <- predict(svm_linear_model, newdata = test_data , type = "prob")

test_data$pred_avg<-(test_data$Knn_prob$Yes + test_data$gbm_prob$Yes + test_data$rf_prob$Yes  + test_data$glm_prob$Yes +test_data$lassoR$Yes +test_data$Elastic$Yes + svm_linear_pred$svm_prob$Yes)/7

test_data$pred_avg<-as.factor(ifelse(test_data$pred_avg>0.5,'Yes','No'))
length(test_churn)
length(test_data$pred_avg)
#test_data$pred_avg
lra = confusionMatrix(test_data$pred_avg,test_churn)
lraccuracy <-lr$overall[c(1,3,4)]
lraccuracy
averaging_accuracy = lraccuracy



#------------------------------------Weighted Average----------------------------------------------
# 
# test_data$pred_avg_weighted<-((test_data$gbm_prob$Yes*0.30) + (test_data$rf_prob$Yes*0.25)  + (test_data$glm_prob$Yes*0.75) +(test_data$lassoR$Yes*0.77) +(test_data$Elastic$Yes*0.75))/4
# test_data$pred_avg_weighted
# test_data$pred_avg_weighted<-as.factor(ifelse(test_data$pred_avg_weighted>0.5,'Yes','No'))
# test_data$pred_avg_weighted
# lra = confusionMatrix(test_data$pred_avg_weighted,test_churn)
# lraccuracy <-lr$overall[c(1,3,4)]
# weighted_avg_accuracy = lraccuracy

#--------------------------------------Accuarcy------------------------------------------------------
models <- c("Logistic", "Random Forest", "Elastic Net", "Ridge Lasso","gradient Boosting","Averaging")
#install.packages("dplyr")
library(dplyr)
accuracysummary <- bind_rows(Logistic = lraccuracy, RandomForest = rf_accuracy,ElasticNet = glmnetaccuracy, Ridge_Lasso = glm_accuracy , Gradient_Boosting = gbm_accuracy , Averaging = averaging_accuracy )
library(tibble)
accuracysummary2 <- add_column(accuracysummary, "Model" = models, .before = "Accuracy")
accuracysummary2

library(ggthemes)
library(ggplot2)
ggplot(accuracysummary2, aes(x = Model, y = Accuracy)) + geom_bar(stat = "identity") + 
  geom_errorbar(width = 0.2, aes(ymin = AccuracyLower, ymax = AccuracyUpper), color = "black") +
  coord_cartesian(ylim = c(0.7, 0.85)) +
  labs(y = "Accuracy %", x = "Model", title = "Model Prediction Accuracy with 95% CI") +
  theme_minimal()


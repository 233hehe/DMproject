source("Cleaning.R")
set.seed(1)
library(arules)
library(arulesViz)
library(glmnet)
library(caret)
library(e1071)
library(ROCR)
library(class)
#
cat("so our baseline model is like randomly,successful rate is 0.2427")
sample <- sample(1:nrow(c6), size = 0.7*nrow(c6))
#============================================Association Rule================================
ARcompany<-c6[,c(1:12)]
ARcompany<-data.frame(apply(ARcompany,2,factor))
rules<-apriori(ARcompany,parameter = list(supp=0.1,conf=0.1),
               appearance = list(lhs=c('successful=1'),default='lhs'),
               control = list(verbose=F))
rules<-sort(rules,by=c("confidence","lift"),decreasing = TRUE)
summary(rules)
#inspect(rules[1:5])
#plot(rules[1:5], method="graph")
#===========================================HUGE DATASETS TOTALLY FAILURE=====================
#==========================================LINEAR AND LOGISTIC REGRESSION====================
lrCompany<- c6[,c(1,3:8,10:12)]
lrCompany$Headquarters.Location<-as.factor(lrCompany$Headquarters.Location)
linear<-lm(formula = successful~.,data=lrCompany)
summary(linear)

seed_investment = 50000
seed_equity = .025
cat("when you invest wrong, we got all seed investment gone")
fpc = seed_investment
cat("when you miss a good oppotunity")
fnc = median(c6$Total.Funding.Amount[c6$successful==1])*seed_equity
#
train<-lrCompany[sample,-1]
test<-lrCompany[-sample,-1]
logistic<-glm(formula = successful~.,data=train)
summary(logistic)
Actual <- test$successful
predicted.probability<- predict(logistic, newdata=test, type = "response")
Predicted <- ifelse(predicted.probability > 0.376, 1, 0)
#
confusion <- table(Actual, Predicted)
confusion
Accuracy <- (confusion[1,1]+confusion[2,2])/sum(confusion)
sensitivity <-(confusion[2,2])/sum(confusion[2,])
specificity <-(confusion[1,1])/sum(confusion[1,])
er <- 1-Accuracy
PPV <- (confusion[2,2])/sum(confusion[,2])
NPV <- (confusion[1,1])/sum(confusion[,1])
#
pred <- prediction(predicted.probability, test$successful)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col="orange",main="Startup Classfication")

pred2 <- prediction(predicted.probability, test$successful)
perf2 <- performance(pred2,"cost",cost.fp=1,cost.fn=1.4285)
plot(perf, col="orange",main="Startup Classfication")
cutoff<- perf2@x.values[[1]][(which.min(perf2@y.values[[1]]))]

#=============================================KNN====================================
normal <- function(x){ return ((x - min(x)) / (max(x)-min(x))) }
knnCompany<-c6[,c(4,6:10,12:108)]
knnCompany$Number.of.Employees<-NULL
knnCompany$Last.Funding.Type<-NULL
knnCompany<-data.frame(apply(knnCompany,2,normal))
knnCompany[(apply(knnCompany,2,sumna)!=0)]<-NULL
#knnCompany[,6:115]<-data.frame(apply(knnCompany[,6:115],2,as.factor))
inTrain <- sample(nrow(knnCompany), 0.7*nrow(knnCompany), replace=FALSE)
knntrain <- data.frame(knnCompany[inTrain,])
knnvalidation <- data.frame(knnCompany[-inTrain,])

kmax <- 5
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
train_input <- as.matrix(knntrain[,-8])
train_output <- as.vector(knntrain[,8])
validate_input <- as.matrix(knnvalidation[,-8])
#
for (i in 6:10){
  knnpred <- knn(train_input, train_input,train_output, k=i)
  knnpred2 <- knn(train_input, validate_input,train_output, k=i)
  # The confusion matrix for training data is:
  CM1 <- table(knnpred, knntrain$successful)
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(knnpred2, knnvalidation$successful)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}
z <- which.min(ER2)
cat("Minimum Validation Error k:", z,"\n")
# Scoring at optimal k
zpred <- knn(train_input, train_input,train_output, k=z)
zpred2 <- knn(train_input, validate_input,train_output, k=z)
#c
zCM1 <- table(zpred, knntrain$successful)
zCM2 <- table(zpred2, knnvalidation$successful)
zCM1
zCM2
zER1 <- (zCM1[1,2]+zCM1[2,1])/sum(zCM1)
zER2 <- (zCM2[1,2]+zCM2[2,1])/sum(zCM2)
zER1
zER2

zpred2prob <- knn(train_input, validate_input,train_output, k=z,prob = T)
knn.probability <- attr(zpred2prob, "prob") 
knn.probability <- ifelse(zpred2prob == 1, knn.probability, 1-knn.probability)
pred_knn <- prediction(knn.probability, knnvalidation$successful)
perf_knn <- performance(pred_knn, "tpr", "fpr")

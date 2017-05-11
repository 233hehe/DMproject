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
ARcompany<-c6[,c(1:13)]
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
lrCompany<- c6[,c(4:9,11:13)]
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
train<-lrCompany[sample,]
test<-lrCompany[-sample,]
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
knnCompany<-c6[,c(4,6:10,12:97)]
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
for (i in 1:kmax){
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
plot(perf_knn,add = T)


c4 <- c3 %>%
  select(Company.Name,Headquarters.Location,Category.Groups,Categories,Founded.Date,Closed.Date,Company.Length,
         Number.of.Employees,Number.of.Founders, Number.of.Articles,Number.of.Investors,Number.of.Lead.Investors,
         Number.of.Funding.Rounds,Last.Funding.Date,Last.Funding.Type,Last.Funding.Amount,Last.Equity.Funding.Amount,
         Total.Equity.Funding.Amount,Total.Funding.Amount,successful)
#convert to dummy category
col = as.array(1:7947)
for (i in 1:length(categorygrouplist)){
  for (n in 1:nrow(c4)){
    col[n]<-ifelse(grepl(categorygrouplist[i],c4$Category.Groups[n]),1,0)
  }
  names=str_replace_all(string=categorygrouplist[i], pattern=" ", repl="")
  c4<-as.data.frame(cbind(c4,col))
  colnames(c4)[ncol(c4)] <- names
}
tmp = c4$Headquarters.Location


c4$Headquarters.Location = tmp
c4$Headquarters.Location = strsplit(c4$Headquarters.Location,",")

for(i in 1:nrow(c4)){
  c4$Headquarters.Location[i] = str_replace_all(c4$Headquarters.Location[[i]][3],pattern = " ",repl = "")
  
}
c4$Headquarters.Location = as.factor(as.character(c4$Headquarters.Location))

rf = c6[,c(-3,-2,-1,-9)]
# library(dummies)
# rf <- dummy.data.frame(rf,names = "Headquarters.Location", sep = "_")
# rf$successful = as.factor(rf$successful)
# rf[(is.na(rf$Number.of.Employees)),"Number.of.Employees"] = "1-10"
# rf[(is.na(rf$Number.of.Founders)),"Number.of.Founders"] = mean(rf$Number.of.Founders)
# rf[(is.na(rf$Number.of.Articles)),"Number.of.Articles"] = mean(rf[!(is.na(rf$Number.of.Articles)),"Number.of.Articles"])
# rf[(is.na(rf$Number.of.Investors)),"Number.of.Investors"] = mean(rf[!(is.na(rf$Number.of.Investors)),"Number.of.Investors"])
# rf[(is.na(rf$Number.of.Lead.Investors)),"Number.of.Lead.Investors"] = mean(rf[!(is.na(rf$Number.of.Lead.Investors)),"Number.of.Lead.Investors"])
# rf[(is.na(rf$Last.Equity.Funding.Amount)),"Last.Equity.Funding.Amount"] = mean(rf[!(is.na(rf$Last.Equity.Funding.Amount)),"Last.Equity.Funding.Amount"])
# rf[(is.na(rf$Last.Funding.Amount)),"Last.Funding.Amount"] = mean(rf[!(is.na(rf$Last.Funding.Amount)),"Last.Funding.Amount"])
# rf[(is.na(rf$Total.Funding.Amount)),"Total.Funding.Amount"] = mean(rf[!(is.na(rf$Total.Funding.Amount)),"Total.Funding.Amount"])
# rf[(is.na(rf$Total.Equity.Funding.Amount)),"Total.Equity.Funding.Amount"] = mean(rf[!(is.na(rf$Total.Equity.Funding.Amount)),"Total.Equity.Funding.Amount"])
# #rf[(is.na(rf$Number.of.Funding.Rounds)),"Number.of.Funding.Rounds"] = mean(rf[!(is.na(rf$Number.of.Funding.Rounds)),"Number.of.Funding.Rounds"])
library(randomForest)
#colnames(rf) = gsub(x=colnames(rf),pattern = " ",replacement = "")
model = randomForest(successful~., data=rf, importance = TRUE, ntree = 1000, proximity=TRUE)
library(ROCR)
predictions=as.vector(model$votes[,2])
pred=prediction(predictions,rf$successful)
perf_AUC=performance(pred,"auc") #Calculate the AUC value
AUC=perf_AUC@y.values[[1]]
perf_ROC=performance(pred,"tpr","fpr") #plot the actual ROC curve
plot(perf_ROC, main="ROC plot",colorize =T)
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))

plot(model, log="y")
varImpPlot(model)

library(e1071)
# Can handle both categorical and numeric input
# but output must be categorical
naivedata = rf
naivedata$Headquarters.Location = c4$Headquarters.Location
naive <- naiveBayes(successful~., data=naivedata)
pre_bayes <- predict(naive, newdata = naivedata[,-10],type = "raw")
pre_b <- prediction(pre_bayes[,2],naivedata$successful)
per_bayes_p <- performance(pre_b,"tpr","fpr")
plot(per_bayes_p,color ="blue",add=T)

naive_t = table(rf$successful,prediction,dnn=list('actual','predicted'))
(naive_t[1,2]/sum(naive_t[1,]))
(naive_t[2,1]/sum(naive_t[2,]))
((naive_t[1,1]+naive_t[2,2])/sum(naive_t))


library(gbm)
boostdata = naivedata
tmp = levels(boostdata$Headquarters.Location)
levels(boostdata$Headquarters.Location) = c(tmp,"others")
boostdata[(is.na(boostdata$Headquarters.Location)),"Headquarters.Location"] = "others"
boostdata$successful<-ifelse(boostdata$successful==1,1,0)
boost=gbm(successful~.,data=boostdata,distribution="bernoulli",n.trees=5000,interaction.depth=4)
summary(boost)
yhat.boost=predict(boost,newdata=boostdata,n.trees=5000,type="response")
pre_boost <- prediction(yhat.boost,boostdata$successful)
per_boost_p <- performance(pre_boost,"tpr","fpr")

plot(perf_ROC, main="ROC plot",colorize =T)
text(0.5,0.5,paste("AUC = ",format(AUC, digits=5, scientific=FALSE)))
plot(per_bayes_p,col ="blue",add=T)
plot(per_boost_p,col ="red",add=T)

pred_boost <- prediction(yhat.boost, boostdata$successful)
perf_boost <- performance(pred_boost,"cost",cost.fp=1,cost.fn=1.4285)
plot(perf_boost, col="orange",main="Startup Classfication")
boost_cutoff<- perf_boost@x.values[[1]][(which.min(perf_boost@y.values[[1]]))]
class_boost = ifelse(yhat.boost>boost_cutoff,1,0)
boost_table = table(boostdata$successful,class_boost,dnn=list('actual','predicted'))
(boost_table)

pre_b <- prediction(pre_bayes[,2],naivedata$successful)
perf_b <- performance(pre_b,"cost",cost.fp=1,cost.fn=1.4285)
plot(perf_b, col="orange",main="Startup Classfication")
naive_cutoff<- perf_b@x.values[[1]][(which.min(perf_b@y.values[[1]]))]
class_naive = ifelse(pre_bayes[,2]>boost_cutoff,1,0)
naive_table = table(naivedata$successful,class_naive,dnn=list('actual','predicted'))
(naive_table)


pre_rf <- prediction(as.vector(model$votes[,2]),rf$successful)
perf_rf <- performance(pre_rf,"cost",cost.fp=1,cost.fn=1.4285)
plot(perf_rf, col="orange",main="Startup Classfication")
rf_cutoff<- perf_rf@x.values[[1]][(which.min(perf_rf@y.values[[1]]))]
class_rf = ifelse(model$votes[,2]>rf_cutoff,1,0)
rf_table = table(rf$successful,class_rf,dnn=list('actual','predicted'))
(rf_table)



source("main data cleaning.R")
set.seed(1)
library(arules)
library(arulesViz)
library(glmnet)

#
cat("so our baseline model is like randomly,successful rate is 0.2427")
sample <- sample(1:nrow(c6), size = 0.7*nrow(c6))
#============================================Association Rule================================
ARcompany<-c6[,c(1,2,4:10,15,1370)]
ARcompany<-data.frame(apply(ARcompany,2,factor))
rules<-apriori(ARcompany,parameter = list(supp=0.1,conf=0.1),
               appearance = list(lhs=c('successful=1'),default='lhs'),
               control = list(verbose=F))
rules<-sort(rules,by=c("confidence","lift"),decreasing = TRUE)
inspect(rules[1:5])
summary(rules)
plot(rules[1:5], method="graph")
#===========================================HUGE DATASETS TOTALLY FAILURE=====================
#==========================================LINEAR AND LOGISTIC REGRESSION====================
lrCompany<- c6[,c(1,3:1369)]
lrCompany$Headquarters.Location<-as.factor(lrCompany$Headquarters.Location)
successful
linear<-lm(formula = successful~.,data=lrCompany)
summary(linear)
#
train <- c6[sample, ]
test  <- c6[-sample, ]
apply(lrCompany,2,sumna)
logistic<-glm(formula = successful~.,data=lrtrain)
summary(logistic)
Actual_test <- lrtest$successful
predicted.probability_test <- predict(logistic, newdata=lrtest, type = "response")
Predicted_test <- ifelse(predicted.probability_test > 0.5, 1, 0)
#
confusion_test <- table(Actual_test, Predicted_test)
rownames(confusion_test) <- c("For","Against")
colnames(confusion_test) <- c("For","Against")
confusion_test
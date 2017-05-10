source("main data cleaning.R")
library(arules)
library(arulesViz)
str(c4)
apply(c4,2,sumna)
ARcompany<-c4[,26:1028]
ARcompany<-data.frame(apply(ARcompany,2,factor))
str(ARcompany)
rules<-apriori(ARcompany,parameter = list(supp=0.5,conf=0.5,maxlen=3,maxtime=10),
               appearance = list(lhs=c('successful=1'),default='lhs'),
               control = list(verbose=F))
rules<-sort(rules,by=c("confidence","lift"),decreasing = TRUE)
inspect(rules[1:5])
summary(rules)
plot(rules[1:5], method="graph")

#
lrCompany<- c5[,c(2,6:)]
library(glmnet)
linear<-lm(successful~.,data=lrCompany)


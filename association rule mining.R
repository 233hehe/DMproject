source("main data cleaning.R")
library(arules)
library(arulesViz)
str(c4)
apply(c4,2,sumna)
c5<-c4[,26:1299]
c5<-data.frame(apply(c5,2,factor))
str(c5)
rules<-apriori(c5,parameter = list(supp=0.05,conf=1,maxlen=3,maxtime=100),
               appearance = list(lhs=c('successful=1'),default='lhs'),
               control = list(verbose=F))
rules<-sort(rules,by=c("confidence","lift"),decreasing = TRUE)
inspect(rules[1:5])
summary(rules)
plot(rules[1:5], method="graph")

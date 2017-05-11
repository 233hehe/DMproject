# #read large DataSet, later on, we use full datasets.
# Tbl <-list.files(pattern="*.csv") %>%
#   map_df(~read_csv(., col_types = cols(.default = "c")))
# 
# write.csv(Tbl,"c.csv")

library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
library(imputeMissings)
library(tidyverse)
library(caret)

#function
sumna<-function(x){
  sum(is.na(x))
}

#read data and clear out useless column
company <- read.csv("crunchbase.csv",stringsAsFactors = FALSE,na.strings = c(""," ","NA","Nan"))
c1<-company
c1$X<-NULL
c1$Company.Name.URL<-NULL
c1$Website<-NULL
c1$Stock.Symbol.URL<-NULL
c1$Crunchbase.Rank<-NULL
c1$Trend.Score..30.Days.<-NULL
c1$Trend.Score..7.Days.<-NULL
c1$Trend.Score..90.Days.<-NULL
#convert to correct data type
c2<-c1
c2$Description<-NULL
c2$Status<-as.factor(c2$Status)
c2$Closed.Date<-as.Date(c2$Closed.Date,format = "%m/%d/%Y")
c2$Founded.Date<-as.Date(c2$Founded.Date,format = "%m/%d/%Y")
c2$Number.of.Articles<-as.numeric(gsub(x=c2$Number.of.Articles,replacement = "",pattern = ","))
c2$Number.of.Employees<-as.factor(c2$Number.of.Employees)
c2$Number.of.Founders<-as.numeric(gsub(x=c2$Number.of.Articles,replacement = "",pattern = ","))
c2$Number.of.Founders[is.na(c2$Number.of.Founders)]<-0
c2$Number.of.Funding.Rounds<-as.numeric(gsub(c2$Number.of.Funding.Rounds,replacement = "",pattern = ","))
c2$Number.of.Funding.Rounds[is.na(c2$Number.of.Funding.Rounds)]<-0
#
c2$Last.Funding.Date<-as.Date(c2$Last.Funding.Date,format="%m/%d/%Y")
c2$Last.Funding.Amount<-as.numeric(gsub(x=c2$Last.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
c2$Last.Funding.Amount[is.na(c2$Last.Funding.Amount)]<-0
c2$Last.Funding.Type<-as.factor(c2$Last.Funding.Type)
c2$Last.Equity.Funding.Amount<-as.numeric(gsub(x=c2$Last.Equity.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
c2$Last.Equity.Funding.Amount[is.na(c2$Last.Equity.Funding.Amount)]<-0
#
c2$Total.Equity.Funding.Amount<-as.numeric(gsub(x=c2$Total.Equity.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
c2$Total.Equity.Funding.Amount[is.na(c2$Total.Equity.Funding.Amount)]<-0
c2$Total.Funding.Amount<-as.numeric(gsub(x=c2$Total.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
c2$Total.Funding.Amount[is.na(c2$Total.Funding.Amount)]<-0
#
c2$IPO.Date<-as.Date(c2$IPO.Date,format="%m/%d/%Y")
c3<-c2
#Create More Useful Column
apply(c3,2,sumna)
c3$Company.Length<-ifelse(is.na(c3$Closed.Date),Sys.Date()-c3$Founded.Date, c3$Closed.Date-c3$Founded.Date)/365
#
categorylist<-na.omit(unique(as.vector(str_split(c3$Categories,",",simplify = TRUE))))
categorygrouplist<-na.omit(unique(as.vector(str_split(c3$Category.Groups,",",simplify=TRUE))))
#DEFINE SUCCESSFUL IS GOING TO IPO OR FOUNDING ROUNDS >=3 OR ACURIED
for (i in 1:nrow(c3)){
  if(!is.na(c3$IPO.Date[i])){
    c3$successful[i]=1
  }else if(!is.na(c3$Number.of.Funding.Rounds[i])&c3$Number.of.Funding.Rounds[i]>=3){
    c3$successful[i]=1
  }else if(c3$Status[i]=="Was Acquired"){
    c3$successful[i]=1
  }else{
    c3$successful[i]=0
  }
}
#rearrange column
c4 <- c3 %>%
  select(Company.Name,Headquarters.Location,Category.Groups,Categories,Founded.Date,Closed.Date,Company.Length,
         Number.of.Employees,Number.of.Founders, Number.of.Articles,Number.of.Investors,Number.of.Lead.Investors,
         Number.of.Funding.Rounds,Last.Funding.Date,Last.Funding.Type,Last.Funding.Amount,Last.Equity.Funding.Amount,
         Total.Equity.Funding.Amount,Total.Funding.Amount,Stock.Exchange,Stock.Symbol,IPO.Date,Valuation.at.IPO,Money.Raised.at.IPO,
         Status,successful)
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

rf = c4[,c(8:12,16:19,26:109)]
library(dummies)
rf <- dummy.data.frame(rf,names = "Headquarters.Location", sep = "_")
rf$successful = as.factor(rf$successful)
rf[(is.na(rf$Number.of.Employees)),"Number.of.Employees"] = "1-10"
rf[(is.na(rf$Number.of.Founders)),"Number.of.Founders"] = mean(rf$Number.of.Founders)
rf[(is.na(rf$Number.of.Articles)),"Number.of.Articles"] = mean(rf[!(is.na(rf$Number.of.Articles)),"Number.of.Articles"])
rf[(is.na(rf$Number.of.Investors)),"Number.of.Investors"] = mean(rf[!(is.na(rf$Number.of.Investors)),"Number.of.Investors"])
rf[(is.na(rf$Number.of.Lead.Investors)),"Number.of.Lead.Investors"] = mean(rf[!(is.na(rf$Number.of.Lead.Investors)),"Number.of.Lead.Investors"])
rf[(is.na(rf$Last.Equity.Funding.Amount)),"Last.Equity.Funding.Amount"] = mean(rf[!(is.na(rf$Last.Equity.Funding.Amount)),"Last.Equity.Funding.Amount"])
rf[(is.na(rf$Last.Funding.Amount)),"Last.Funding.Amount"] = mean(rf[!(is.na(rf$Last.Funding.Amount)),"Last.Funding.Amount"])
rf[(is.na(rf$Total.Funding.Amount)),"Total.Funding.Amount"] = mean(rf[!(is.na(rf$Total.Funding.Amount)),"Total.Funding.Amount"])
rf[(is.na(rf$Total.Equity.Funding.Amount)),"Total.Equity.Funding.Amount"] = mean(rf[!(is.na(rf$Total.Equity.Funding.Amount)),"Total.Equity.Funding.Amount"])
#rf[(is.na(rf$Number.of.Funding.Rounds)),"Number.of.Funding.Rounds"] = mean(rf[!(is.na(rf$Number.of.Funding.Rounds)),"Number.of.Funding.Rounds"])
library(randomForest)
model = randomForest(successful~., data=rf, importance = TRUE, ntree = 10001, proximity=TRUE, sampsize=sampsizes)
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


# #read large DataSet, later on, we use full datasets.
# Tbl <-list.files(pattern="*.csv") %>%
#   map_df(~read_csv(., col_types = cols(.default = "c")))
# 
# write.csv(Tbl,"c.csv")

library(stringr)
library(dplyr)
library(data.table)
library(imputeMissings)
library(tidyverse)
library(caret)

#function
sumna<-function(x){
  sum(is.na(x))
}

#read data and clear out useless column
company <- read.csv("c.csv",stringsAsFactors = FALSE,na.strings = c(""," ","NA","Nan"),
                    fileEncoding="latin1")
c1<-company
c1$X<-NULL
c1$Company.Name.URL<-NULL
c1$Website<-NULL
c1$Stock.Symbol.URL<-NULL
c1$Stock.Symbol<-NULL
c1$Stock.Exchange<-NULL
c1$Stock.Exchanges<-NULL
c1$Crunchbase.Rank<-NULL
c1$Trend.Score..30.Days.<-NULL
c1$Trend.Score..7.Days.<-NULL
c1$Trend.Score..90.Days.<-NULL
#convert to correct data type
c2<-c1
c2$Description<-NULL
apply(c2,2,sumna)
c2$Headquarters.Location<-(str_split(c2$Headquarters.Location,",",simplify = TRUE)[,3])
c2$Status<-as.factor(c2$Status)
c2$Closed.Date<-as.Date(c2$Closed.Date,format = "%m/%d/%Y")
c2$Founded.Date<-as.Date(c2$Founded.Date,format = "%m/%d/%Y")
#
c2$Number.of.Articles<-as.numeric(gsub(x=c2$Number.of.Articles,replacement = "",pattern = ","))
c2$Number.of.Employees<-as.factor(c2$Number.of.Employees)
c2$Number.of.Founders<-as.numeric(gsub(x=c2$Number.of.Articles,replacement = "",pattern = ","))
c2$Number.of.Funding.Rounds<-as.numeric(gsub(c2$Number.of.Funding.Rounds,replacement = "",pattern = ","))
#
c2$Last.Funding.Date<-as.Date(c2$Last.Funding.Date,format="%m/%d/%Y")
c2$Last.Funding.Amount<-as.numeric(gsub(x=c2$Last.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
c2$Last.Funding.Type<-as.factor(c2$Last.Funding.Type)
c2$Last.Equity.Funding.Amount<-as.numeric(gsub(x=c2$Last.Equity.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
#
c2$Total.Equity.Funding.Amount<-as.numeric(gsub(x=c2$Total.Equity.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
c2$Total.Funding.Amount<-as.numeric(gsub(x=c2$Total.Funding.Amount,replacement = "",pattern = "(\\$|,)"))
#
c2$IPO.Date<-as.Date(c2$IPO.Date,format="%m/%d/%Y")
c3<-c2
#Create More Useful Column
apply(c3,2,sumna)
c3$Company.Length<-ifelse(is.na(c3$Closed.Date),Sys.Date()-c3$Founded.Date, c3$Closed.Date-c3$Founded.Date)/365
#
categorylist<-na.omit(unique(as.vector(str_split(c3$Categories,",",simplify = TRUE))))
categorygrouplist<-na.omit(unique(as.vector(str_split(c3$Category.Groups,",",simplify=TRUE))))
categorygrouplist<-gsub(" ","",categorygrouplist)
categorygrouplist = unique(categorygrouplist)
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
         Total.Equity.Funding.Amount,Total.Funding.Amount,successful)
#convert category to dummy category
col<-vector()
for (i in 1:length(categorylist)){
  for (n in 1:nrow(c4)){
    col[n]<-ifelse(grepl(categorylist[i],c4$Categories[n]),1,0)
  }
   names<-categorylist[i]
   c4<-as.data.frame(cbind(c4,col))
   colnames(c4)[ncol(c4)] <- names
}
c4$Category.Groups<-str_split(c4$Categories,",",simplify = TRUE)[,1]
c4$Categories<-str_split(c4$Categories,",",simplify = TRUE)[,1]
#convert category group to dummy category
c5<-c3 %>%
  select(Company.Name,Headquarters.Location,Category.Groups,Categories,Founded.Date,Closed.Date,Company.Length,
         Number.of.Employees,Number.of.Founders, Number.of.Articles,Number.of.Investors,Number.of.Lead.Investors,
         Number.of.Funding.Rounds,Last.Funding.Date,Last.Funding.Type,Last.Funding.Amount,Last.Equity.Funding.Amount,
         Total.Equity.Funding.Amount,Total.Funding.Amount,IPO.Date,Valuation.at.IPO,Money.Raised.at.IPO,
         Status,successful)

col<-vector()
for (i in 1:length(categorygrouplist)){
  for (n in 1:nrow(c5)){
    col[n]<-ifelse(grepl(categorygrouplist[i],c5$Category.Groups[n]),1,0)
  }
  names<-categorygrouplist[i]
  c5<-as.data.frame(cbind(c5,col))
  colnames(c5)[ncol(c5)] <- names
}
#c5$Category.Groups<-str_split(c5$Categories,",",simplify = TRUE)[,1]
#c5$Categories<-str_split(c5$Categories,",",simplify = TRUE)[,1]
#After we create more useful column we drop the useless now!!!
c6<-c5
colnames(c6)<-gsub(x=colnames(c6),pattern = "X.",replacement = "")
c6$Var.650<-NULL
c6$Var.73<-NULL
c6$X2<-NULL
c6$Company.Name<-NULL
c6$Founded.Date<-NULL
c6$Closed.Date<-NULL
c6$Last.Funding.Date<-NULL
c6$IPO.Date<-NULL
c6$Valuation.at.IPO<-NULL
c6$Money.Raised.at.IPO<-NULL
c6$Status<-NULL 

#Handling Missing Data
apply(c6[1:15],2,sumna)
c6$successful[which(is.na(c6$Company.Length))]
c6<-c6[!is.na(c6$Company.Length),]
c6$successful[which(is.na(c6$Last.Funding.Type))]
c6<-c6[!is.na(c6$Last.Funding.Type),]
c6$Number.of.Articles[is.na(c6$Number.of.Articles)]<-median(na.omit(c6$Number.of.Articles))
c6$Number.of.Investors[is.na(c6$Number.of.Investors)]<-median(na.omit(c6$Number.of.Investors))
c6$Number.of.Lead.Investors[is.na(c6$Number.of.Lead.Investors)]<-0
c6<-c6[!is.na(c6$Total.Funding.Amount),]
c6$Last.Equity.Funding.Amount<-NULL
c6$Total.Equity.Funding.Amount<-NULL
c6$Number.of.Founders[is.na(c6$Number.of.Founders)]<-median(na.omit(c6$Number.of.Founders))
#swrite.csv(c6,"Final-Cleaning.csv")

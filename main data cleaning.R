library(stringr)
library(dplyr)
library(ggplot2)
#function
sumna<-function(x){
  sum(is.na(x))
}
  
#read data and clear out useless column
company <- read.csv("crunchbase.csv",stringsAsFactors = FALSE,na.strings = c(""," ","NA","Nan"))
c1<-company
c1$X<-NULL
c1$Company.Name.URL<-NULL
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
c2$Number.of.Founders[is.na(c2$Number.of.Founders)]<-0
c2$Number.of.Funding.Rounds[is.na(c2$Number.of.Funding.Roundsis.na)]<-0
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
write.csv(x = c2,file = "cleaning-1.csv",na ="",fileEncoding ="utf-8")
c3<-c2
#Create More Useful Column
apply(c3,2,sumna)
c3$Company.Length<-ifelse(is.na(c3$Closed.Date),Sys.Date()-c3$Founded.Date, c3$Closed.Date-c3$Founded.Date)/365
#
unique(as.vector(str_split(c3$Categories,",",simplify = TRUE)))
a<-data.frame(str_split(c3$Categories,",",simplify = TRUE),stringsAsFactors = FALSE)
#DEFINE SUCCESSFUL IS GOING TO IPO OR FOUNDING ROUNDS >=3
c3$successful<-ifelse(!is.na(c3$IPO.Date)|c3$Total.Funding.Amount>=3|c3$Status=="Was Acquired",1,0)
#rearrange column
c4 <- c3 %>%
  select(Company.Name,Headquarters.Location,Category.Groups,Categories,Founded.Date,Closed.Date,Company.Length,
         Number.of.Employees,Number.of.Founders, Number.of.Articles,Number.of.Investors,Number.of.Lead.Investors,
         Number.of.Funding.Rounds,Last.Funding.Date,Last.Funding.Type,Last.Funding.Amount,Last.Equity.Funding.Amount,
         Total.Equity.Funding.Amount,Total.Funding.Amount,Stock.Exchange,Stock.Symbol,IPO.Date,Valuation.at.IPO,Money.Raised.at.IPO,
         Status,successful)
#exploratory Analysis
qplot(c3$Number.of.Funding.Rounds)
table(c3$Status)
table(c3$Number.of.Funding.Rounds)


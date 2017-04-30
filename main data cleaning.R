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
#
write.csv(x = c2,file = "cleaning-1.csv",na ="",fileEncoding ="utf-8")
c3<-c2


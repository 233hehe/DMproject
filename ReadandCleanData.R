#Download First Datasets.
download.file(url='https://raw.githubusercontent.com/notpeter/crunchbase-data/master/acquisitions.csv',
              destfile="C:/Users/willi/Desktop/Github/DMproject/acquisitions.csv")
acqusition<-read.csv(file = "C:/Users/willi/Desktop/GitHub/acquisitions.csv",
                     header = T)
download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/additions.csv",
              destfile = "C:/Users/willi/Desktop/Github/DMproject/additions.csv")
addtion<-read.csv(file="C:/Users/willi/Desktop/Github/DMproject/additions.csv",header = T)
download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/companies.csv",
              destfile="C:/Users/willi/Desktop/Github/DMproject/companies.csv")
company<-read.csv(file="C:/Users/willi/Desktop/Github/DMproject/companies.csv",header=T)
download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/investments.csv",
              destfile = "C:/Users/willi/Desktop/Github/DMproject/investments.csv")
investment<-read.csv(file="C:/Users/willi/Desktop/Github/DMproject/investments.csv",header=T)
download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/rounds.csv",
              destfile = "C:/Users/willi/Desktop/Github/DMproject/rounds.csv")
round<-read.csv(file="C:/Users/willi/Desktop/Github/DMproject/rounds.csv",header=T)

#variable names
variable<-c(names(acqusition),names(addtion),names(company),names(investment),names(round))
variable<-unique(variable)
variable
#==============================================previous part is the sup data set===============================
#==============================================below part is the main data set==================================
#Cruchbase Database
library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)
#read data
download.file(url="https://raw.githubusercontent.com/233hehe/DMproject/master/crunchbase.csv",
              destfile = "c.csv")
company=read.csv("c.csv",stringsAsFactors = FALSE,na.strings=c("NA","NaN"))
#delete useless column
c1=company
c1$Company.Name.URL=NULL
c1$X=NULL
#Convert to Correct Date format
c2<-c1
c2$Status<-as.factor(c2$Status)
c2$Closed.Date=as.Date(c2$Closed.Date,format = "%m/%d/%Y")
c2$Founded.Date=as.Date(c2$Founded.Date,format="%m/%d/%Y")
c2$Number.of.Articles=as.numeric(gsub(x=c2$Number.of.Articles,pattern = ",",replacement = ""))

#
c2$Company.Length=ifelse(is.na(c2$Closed.Date),Sys.Date()-c2$Founded.Date,c2$Closed.Date-c2$Founded.Date)/365

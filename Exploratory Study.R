source("main data cleaning.R")

#exploratory Analysis
cat("funding Rounds >=3 ") 
sum(c4$Number.of.Funding.Rounds>=3,na.rm=TRUE)
cat("was acquired")
sum(c4$Status=="Was Acquired")
cat("went to IPO")
sum(!is.na(c4$IPO.Date))
#successful analysis
qplot(Total.Funding.Amount,data=c4,main="founding rounds distribution",drv=c4$Number.of.Employees)
table(c4$Number.of.Funding.Rounds)
cat("successful rate")
table(c4$successful)[2]/nrow(c4)

ggplot(data=c4,aes(x=Category,y=Number.of.Funding.Rounds))+geom_bar(stat="identity")
ggplot(data=c4,aes(x=Category.Groups,y=Total.Funding.Amount))+geom_bar(stat="identity")

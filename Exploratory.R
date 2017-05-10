source("main data cleaning.R")

#successful analysis
cat("funding Rounds >=3 ") 
sum(c4$Number.of.Funding.Rounds>=3,na.rm=TRUE)
cat("was acquired")
sum(c4$Status=="Was Acquired")
cat("went to IPO")
sum(!is.na(c4$IPO.Date))
table(c4$Number.of.Funding.Rounds)
cat("successful rate")
table(c4$successful)[2]/nrow(c4)

#exploratory Analysis
ggplot(data=c5,aes(Number.of.Funding.Rounds))+
  geom_histogram(binwidth=0.5,
                 col="red", 
                 fill="green", 
                 alpha = .2)+
  labs(title="Successful FundingRounds Distribution")+
  xlim(c(0,22))

ggplot(data=c4,aes(x=Category.Groups,y=Total.Funding.Amount))+geom_bar(stat="identity")

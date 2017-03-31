download.file(url='https://raw.githubusercontent.com/notpeter/crunchbase-data/master/acquisitions.csv',destfile="1.csv")
acqusition<-read.csv(file = "1.csv",header = T)
names(acqusition)

download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/additions.csv",destfile = "addition.csv")
addtion<-read.csv(file="addition.csv",header = T)
names(addtion)

download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/companies.csv",destfile="companyname.csv")
company<-read.csv(file="companyname.csv",header=T)
names(company)

download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/investments.csv",destfile = "investment.csv")
investment<-read.csv(file="investment.csv",header=T)
names(investment)

download.file(url="https://raw.githubusercontent.com/notpeter/crunchbase-data/master/rounds.csv",destfile = "round.csv")
round<-read.csv(file="round.csv",header=T)
names(round)
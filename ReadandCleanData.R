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

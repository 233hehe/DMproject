data = read.csv("cleaning-1.csv")
data$X = NULL
founddate = strsplit(as.character(data$Founded.Date),"-")
foundyear = as.integer(sapply(founddate, "[", 1)) - 2012
data$foundyear = foundyear
firstpart = data[data$Status=="IPO"|data$Status=="Was Acquired",]
secondpart = data[(!is.na(data$Number.of.Funding.Rounds))&(data$Number.of.Funding.Rounds>2),]
thirdpart = data[((!is.na(data$Number.of.Funding.Rounds))&(data$Number.of.Funding.Rounds==2))&((!is.na(data$foundyear))&(data$foundyear>=0)),]
newdata = rbind(firstpart,secondpart,thirdpart)
newdata = unique(newdata[,1:25])
write.csv(x = newdata, file = "cleaning-2.csv")

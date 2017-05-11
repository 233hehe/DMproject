data = read.csv("cleaning-1.csv")
data$X = NULL
founddate = strsplit(as.character(data$Founded.Date),"-")
foundyear = as.integer(sapply(founddate, "[", 1)) - 2012
data$foundyear = foundyear
data$sucess = 0
firstpart = data[data$Status=="IPO"|data$Status=="Was Acquired",]
secondpart = data[(!is.na(data$Number.of.Funding.Rounds))&(data$Number.of.Funding.Rounds>2),]
thirdpart = data[((!is.na(data$Number.of.Funding.Rounds))&(data$Number.of.Funding.Rounds==2))&((!is.na(data$foundyear))&(data$foundyear>=0)),]
newdata = rbind(firstpart,secondpart,thirdpart)
newdata = unique(newdata[,1:25])
newdata$sucess = 1
fail = data[newdata,]
write.csv(x = newdata, file = "cleaning-2.csv")

data[data$Status=="IPO"|data$Status=="Was Acquired","sucess"]=1
data[(!is.na(data$Number.of.Funding.Rounds))&(data$Number.of.Funding.Rounds>2),"sucess"] = 1
data[((!is.na(data$Number.of.Funding.Rounds))&(data$Number.of.Funding.Rounds==2))&((!is.na(data$foundyear))&(data$foundyear>=0)),"sucess"]=1
suc = data[data$sucess==1,]
fail = data[data$sucess == 0 ,]

library(randomForest)
rf <- randomForest(sucess~Status, data=data, ntree=1000, keep.forest=FALSE,importance=TRUE)
plot(rf, log="y")
varImpPlot(rf)
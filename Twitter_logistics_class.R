setwd("/Users/GreenTea/Desktop")
mydata <- read.csv("twitter.csv",stringsAsFactors = F)
mydata<-mydata[which(mydata$followers!=0 & mydata$friends!=0),]
mydata$followers<-log(mydata$followers)
mydata$friends<-log(mydata$friends)
mydata$total_tweets<-log(mydata$total_tweets)
cors<-cor(mydata[-1])
#logistic model experts,friends and mentioned vs. three hashtag groups
set.seed(123)
traindata<-subset(mydata,utype<7)
traindata$utype[traindata$utype %in% c(4,5,6)]<-0
traindata$utype[traindata$utype %in% c(1,2,3)]<-1
table(traindata$utype)
trainmodel<-glm(utype~.,family=binomial, data=traindata)
summary(trainmodel)

#logistic model experts vs. friends (not for class but you can practice this later)
set.seed(123)
traindata<-subset(mydata,utype<3)
traindata$utype[traindata$utype == 2]<-0
table(traindata$utype)
trainmodel<-glm(utype~.,family=binomial, data=traindata)
summary(trainmodel)

#prediction (here we are using the same set for training and testing but it's just practice. Normally you should partition first or use a different data set.)
traindata$rankP <- predict(trainmodel, traindata, type = "response")
write.csv(traindata,file="pred.csv")


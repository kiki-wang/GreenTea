#Set way to read files and load packages
setwd("/Users/GreenTea/Desktop/Predictive")
library(ggplot2)
library(data.table)
library(arules)
library(arulesViz)
library(caret)
library(lattice)
library(forecast)
library(TTR)

#merge sales and items data in 201424
Items_All_201424<-read.csv("Items_All_201424.csv",sep = ",",quote = "/",na.strings = "NA", header = TRUE, row.names = NULL, fileEncoding = "UTF-16LE")
Sales_Week_201424<-read.csv("Sales_Week_201424.csv",sep = ",",quote = "/",na.strings = "NA", header = TRUE, row.names = NULL, fileEncoding = "UTF-16LE")
merge_201424<-merge(Items_All_201424,Sales_Week_201424,by="UPC")

#read all the items data and rbind
items <- list.files(pattern = "Items")
files2 <- as.data.frame(rbindlist(lapply(items, read.csv, sep = ",", quote="/", na.strings="NA", header = TRUE, row.names = NULL, fileEncoding = "UTF-16LE")))
head <- head(files2, n=100)
sales<-list.files(pattern = "Sales")

#Read customer data in 2015 and subset it into Trend dataset
cluster1<-fread("4clusters1.csv",sep = ",")
cluster2<-fread("4clusters2.csv",sep = ",")
cluster3<-fread("4clusters3.csv",sep = ",")
cluster<-rbind(cluster1,cluster2,cluster3)
cluster<-na.omit(cluster)
cluster_trend<-subset(cluster,cluster$Division.Description=="TREND")
cluster_trend$Clusters<-NULL

#Find the class-level association rules in whole dataset
merge<-read.csv("merge.csv")
rules_all<-data.frame(merge$PreferredCustomerNumber,merge$Class.Description)
names(rules_all)[1]<-"ID"
names(rules_all)[2]<-"Class"
dummy_class_all <- dummyVars(~ID+Class, data=rules_all, fullRank=FALSE)
rules_all<-na.omit(rules_all)
rm(merge)
rules_1<-rules_all[1:1,]
rules_1<-as.data.frame(predict(dummy_class_all,rules_1))
rules_total<-rules_1
rm(rules_1)
rules_1<-rules_all[2:10000,]
rules_1<-as.data.frame(predict(dummy_class_all,rules_1))
rules_total<-rbind(rules_total,rules_1)
write.csv(rules_total,"rules_total")
rm(rules_1)
rules_2<-rules_all[10001:50000,]
rules_2<-as.data.frame(predict(dummy_class_all,rules_2))
rules_total<-rbind(rules_total,rules_2)
write.csv(rules_total,"rules_total")
rm(rules_2)
rules_3<-rules_all[50001:150000,]
rules_3<-as.data.frame(predict(dummy_class_all,rules_3))
rules_total<-rbind(rules_total,rules_3)
write.csv(rules_total,"rules_total")
rm(rules_3)
rules_4<-rules_all[150001:250000,]
rules_4<-as.data.frame(predict(dummy_class_all,rules_4))
rules_total<-rbind(rules_total,rules_4)
write.csv(rules_total,"rules_total")
rm(rules_4)
rules_5<-rules_all[250001:350000,]
rules_5<-as.data.frame(predict(dummy_class_all,rules_5))
rules_total<-rbind(rules_total,rules_5)
write.csv(rules_total,"rules_total")
rm(rules_5)
rules_6<-rules_all[350001:450000,]
rules_6<-as.data.frame(predict(dummy_class_all,rules_6))
rules_total<-rbind(rules_total,rules_6)
write.csv(rules_total,"rules_total")
rm(rules_6)
rules_7<-rules_all[450001:700000,]
rules_7<-as.data.frame(predict(dummy_class_all,rules_7))
rules_total<-rbind(rules_total,rules_7)
write.csv(rules_total,"rules_total")
rm(rules_7)
rules_8<-rules_all[700001:800000,]
rules_8<-as.data.frame(predict(dummy_class_all,rules_8))
rules_total<-rbind(rules_total,rules_8)
write.csv(rules_total,"rules_total")
rm(rules_8)
rules_total$X<-NULL
rules_9<-rules_all[800001:800002,]
rules_9<-as.data.frame(predict(dummy_class_all,rules_9))
write.csv(rules_9,"rules_9")
rules_9<-read.csv("rules_9")
rules_9$X<-NULL
rules_total<-rbind(rules_total,rules_9)
write.csv(rules_total,"rules_total")
rules_10<-rules_all[800003:1000000,]
rules_10<-as.data.frame(predict(dummy_class_all,rules_10))
write.csv(rules_10,"rules_10")
rules_10<-read.csv("rules_10")
rules_10$X<-NULL
rules_total<-rbind(rules_total,rules_10)
write.csv(rules_total,"rules_total")
rm(rules_9,rules_10)
rules_11<-rules_all[1000001:1050000,]
rules_11<-as.data.frame(predict(dummy_class_all,rules_11))
write.csv(rules_11,"rules_11")
rules_11<-read.csv("rules_11")
rules_11$X<-NULL
rules_total<-rbind(rules_total,rules_11)
write.csv(rules_total,"rules_total")
rm(rules_11)
rules_12<-rules_all[1050001:1300000,]
rules_12<-as.data.frame(predict(dummy_class_all,rules_12))
write.csv(rules_12,"rules_12")
rules_12<-read.csv("rules_12")
rules_12$X<-NULL
rules_total<-rbind(rules_total,rules_12)
write.csv(rules_total,"rules_total")
rm(rules_12)
rules_13<-rules_all[1300001:1327064,]
rules_13<-as.data.frame(predict(dummy_class_all,rules_13))
write.csv(rules_13,"rules_13")
rules_13<-read.csv("rules_13")
rules_13$X<-NULL
rules_total<-rbind(rules_total,rules_13)
write.csv(rules_total,"rules_total")
rules_total<-fread("rules_total",header = TRUE)
rules_total$V1<-NULL
rules_apriori_class_all<-aggregate(rules_total,by=list(rules_total$ID),FUN=sum)
rm(rules_total)
rules_apriori_class_all$Group.1<-rules_apriori_class_all$ID<-NULL
for (i in 1:1500) {
  rules_apriori_class_all[[i]][rules_apriori_class_all[[i]]>0]<-1
}
rules_apriori_class_all_matrix<-as.matrix(rules_apriori_class_all)
rules_apriori_class_all_result<-apriori(rules_apriori_class_all_matrix,parameter = list(supp=0.01,conf=0.98))
inspect(rules_apriori_class_all_result[1:200])
plot(rules_apriori_class_all_result, method="grouped")


#Find the class-level association rules in Trend dataset
cluster_trend$PreferredCustomerNumber<-as.numeric(cluster_trend$PreferredCustomerNumber)
trend_apriori_class<-data.frame(cluster_trend$PreferredCustomerNumber,cluster_trend$Class.Description)
names(trend_apriori_class)[1]<-"ID"
names(trend_apriori_class)[2]<-"Class"
dummy_trend_class <- dummyVars(~ID+Class, data=trend_apriori_class, fullRank=FALSE)
trend_apriori_class<-na.omit(trend_apriori_class)
trend_apriori_class<-as.data.frame(predict(dummy_trend_class,trend_apriori_class))
trend_apriori_class<-aggregate(trend_apriori_class,by=list(trend_apriori_class$ID),FUN=sum)
trend_apriori_class[[1]]<-trend_apriori_class[[2]]<-NULL
for (i in 1:172) {
  trend_apriori_class[[i]][trend_apriori_class[[i]]>0]<-1
}
rules_trend_class_matrix<-as.matrix(trend_apriori_class)
rules_trend_apriori_class<-apriori(rules_trend_class_matrix,parameter = list(supp=0.0009,conf=0.5))
inspect(rules_trend_apriori_class)
plot (rules_trend_apriori_class,method="grouped")

#Find the department-level association rules in Trend dataset
trend_apriori_department<-data.frame(cluster_trend$PreferredCustomerNumber,cluster_trend$Department.Description)
names(trend_apriori_department)[1]<-"ID"
names(trend_apriori_department)[2]<-"Department"
dummy_trend_department<- dummyVars(~ID+Department, data=trend_apriori_department, fullRank=FALSE)
trend_apriori_department<-na.omit(trend_apriori_department)
rules_department<-as.data.frame(predict(dummy_trend_department,trend_apriori_department))
rules_department<-aggregate(rules_department,by=list(rules_department$ID),FUN=sum)
rules_department[[1]]<-rules_department[[2]]<-NULL
for (i in 1:21) {
  rules_department[[i]][rules_department[[i]]>0]<-1
}
rules_department_matrix<-as.matrix(rules_department)
rules_department_apriori<-apriori(rules_department_matrix,parameter = list(supp=0.002,conf=0.7))
inspect(rules_department_apriori)
plot(rules_department_apriori,method="grouped")

#Draw the sales graph of T-BATMAN
cluster_BATMAN<-subset(cluster_trend,cluster_trend$Item.Short.Description=="T-BATMAN")
BATMAN_sales<-data.frame(cluster_BATMAN$Net_Sales_Amt,cluster_BATMAN$Transaction_date)
BATMAN_sales<-aggregate(BATMAN_sales$cluster_BATMAN.Net_Sales_Amt,by=list(cluster_BATMAN$Transaction_date),FUN=sum)
BATMAN_sales$time<-strptime(BATMAN_sales$Group.1,format = "%d-%B-%y")
BATMAN_sales$Group.1<-NULL
batman<-ggplot(BATMAN_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the sales graph of T-JURASSI
cluster_JURASSI<-subset(cluster_trend,cluster_trend$Item.Short.Description=="T-JURASSI")
JURASSI_sales<-data.frame(cluster_JURASSI$Net_Sales_Amt,cluster_JURASSI$Transaction_date)
JURASSI_sales<-aggregate(JURASSI_sales$cluster_JURASSI.Net_Sales_Amt,by=list(cluster_JURASSI$Transaction_date),FUN=sum)
JURASSI_sales$time<-strptime(JURASSI_sales$Group.1,format = "%d-%B-%y")
JURASSI_sales$Group.1<-NULL
ggplot(JURASSI_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the sales graph of T-DEADPOO
cluster_DEADPOO<-subset(cluster_trend,cluster_trend$Item.Short.Description=="T-DEADPOO")
DEADPOO_sales<-data.frame(cluster_DEADPOO$Net_Sales_Amt,cluster_DEADPOO$Transaction_date)
DEADPOO_sales<-aggregate(DEADPOO_sales$cluster_DEADPOO.Net_Sales_Amt,by=list(cluster_DEADPOO$Transaction_date),FUN=sum)
DEADPOO_sales$time<-strptime(DEADPOO_sales$Group.1,format = "%d-%B-%y")
DEADPOO_sales$Group.1<-NULL
ggplot(DEADPOO_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the sales graph of T-JURASSI and T-BATMAN
jurassi_batman<-merge(JURASSI_sales,BATMAN_sales,by="time",all=TRUE)
g <- ggplot(jurassi_batman, aes(jurassi_batman$time))
g <- g + geom_line(aes(y=jurassi_batman$x.x), colour="red")
g <- g + geom_line(aes(y=jurassi_batman$x.y), colour="blue")
g

#Draw the sales graph of Trend dataset
TREND_sales<-data.frame(cluster_trend$Net_Sales_Amt,cluster_trend$Transaction_date)
TREND_sales<-aggregate(TREND_sales$cluster_trend.Net_Sales_Amt,by=list(TREND_sales$cluster_trend.Transaction_date),FUN=sum)
TREND_sales$time<-strptime(TREND_sales$Group.1,format = "%d-%B-%y")
TREND_sales$Group.1<-NULL
ggplot(TREND_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Add 2 columns, one is the shopping number, one is percentage of shopping in holidays(Nov and Dec)
cluster_trend$buy<-1
cluster_trend$buy_1<-0
value_1<-"Nov"
value_2<-"Dec"
cluster_trend$Nov<-grepl(value_1, cluster_trend$Transaction_date)
cluster_trend$Dec<-grepl(value_2, cluster_trend$Transaction_date)
cluster_trend$buy_1[cluster_trend$Nov=="TRUE"]<-1
cluster_trend$buy_1[cluster_trend$Dec=="TRUE"]<-1
cluster_gift<-data.frame(cluster_trend$PreferredCustomerNumber,cluster_trend$buy,cluster_trend$buy_1)
cluster_gift<-aggregate(cluster_gift,by=list(cluster_gift$cluster_trend.PreferredCustomerNumber),FUN=sum)
names(cluster_gift)[2]<-"ID"
names(cluster_gift)[3]<-"number"
names(cluster_gift)[4]<-"gift number"
cluster_gift$Group.1<-NULL
cluster_gift$ratio<-cluster_gift$`gift number`/cluster_gift$number
cluster_gift$`gift number`<-NULL
write.csv(cluster_gift,"cluster_gift.csv")

#Get the Trend dataset of the whole data
data2014<-fread("Total_2014.csv",select=c(3,11,12,20,22),header = TRUE)
data2014$sales<-data2014$Net.Sls.Qty*data2014$Ext.Net.Sls.Amt
data2014$Net.Sls.Qty<-data2014$Ext.Net.Sls.Amt<-NULL
trend_2014<-subset(data2014,data2014$Division.Description =="TREND")
trend_2014$Transaction.Date<-as.Date(trend_2014$Transaction.Date)
data2015<-fread("Merge2015.csv",header=TRUE)
data2015$sales<-data2015$Net.Sls.Qty*data2015$Ext.Net.Sls.Amt
data2015$V1<-data2015$UPC<-data2015$Net.Sls.Qty<-data2015$Ext.Net.Sls.Amt<-NULL
data2015$items.Division.Description<-NULL
names(data2015)[1]<-"Date"
trend_2014$Division.Description<-NULL
data2015$Date<-as.Date(data2015$Date)
names(trend_2014)[1]<-"Date"
names(data2015)[2]<-"Item.Short.Description"
Trend<-rbind(trend_2014,data2015)

#set the different category in Trend dataset
value_1<-"SOCK"
value_2<-"POP"
value_3<-"CANDY"
value_4<-"SHOPKINS"
value_5<-"POKEMON"
sock<-subset(Trend,grepl(value_1,Trend$Item.Short.Description)=="TRUE")
candy<-subset(Trend,grepl(value_2, Trend$Item.Short.Description)=="TRUE"|grepl(value_3, Trend$Item.Short.Description)=="TRUE"|Trend$Item.Short.Description=="KASUGAI"|Trend$Item.Short.Description=="KITKAT"|Trend$Item.Short.Description=="HARIBO"|Trend$Item.Short.Description=="GLICO POC")
collect<-subset(Trend,grepl(value_4, Trend$Item.Short.Description)=="TRUE"|grepl(value_5, Trend$Item.Short.Description)=="TRUE")
soda<-subset(Trend,Trend$Item.Short.Description=="KAS"|Trend$Item.Short.Description=="SANGARIA")

#Plot the candy sales
candy$Item.Short.Description<-candy$Division.Description<-NULL
candy<-aggregate(candy$sales,by=list(candy$Date),FUN=sum)
names(candy)[1]<-"Date"
names(candy)[2]<-"sales"
ggplot(candy,aes(Date,sales))+geom_line()+xlab("")+ylab("sales")

#Plot the sock sales
sock$Item.Short.Description<-sock$Division.Description<-NULL
sock<-aggregate(sock$sales,by=list(sock$Date),FUN=sum)
names(sock)[1]<-"Date"
names(sock)[2]<-"sales"
ggplot(sock,aes(Date,sales))+geom_line()+xlab("")+ylab("sales")

#Plot the collective toys sales
collect$Item.Short.Description<-collect$Division.Description<-NULL
collect<-aggregate(collect$sales,by=list(collect$Date),FUN=sum)
names(collect)[1]<-"Date"
names(collect)[2]<-"sales"
ggplot(collect,aes(Date,sales))+geom_line()+xlab("")+ylab("sales")

#Plot the soda sales
soda$Item.Short.Description<-soda$Division.Description<-NULL
soda<-aggregate(soda$sales,by=list(soda$Date),FUN=sum)
names(soda)[1]<-"Date"
names(soda)[2]<-"sales"
ggplot(soda,aes(Date,sales))+geom_line()+xlab("")+ylab("sales")

#Time Series analysis for candy
candy_ts<-ts(candy$sales,start = c(2014,7),end = c(2016,1),frequency = 525)
plot.ts(candy_ts)
fit_candy<-ets(candy$sales)
forecast(fit_candy,5)
plot(forecast(fit_candy,5))
fit_arima_candy<-auto.arima(candy$sales,ic="bic")
forecast(fit_arima_candy,50)
plot(forecast(fit_arima_candy,25))
sma_candy<-SMA(candy_ts,n=3)
plot.ts(sma_candy)
HoltWinters_candy<-HoltWinters(candy_ts,gamma = FALSE)
HoltWinters_candy_for<-forecast.HoltWinters(HoltWinters_candy,h=196)
plot.forecast(HoltWinters_candy_for)
acf(HoltWinters_candy_for$residuals,lag.max = 20)
Box.test(HoltWinters_candy_for$residuals,lag= 20,type="Ljung-Box")

#Time Series analysis for sock
sock_ts<-ts(sock$sales,start = c(2014,7),end = c(2016,1),frequency = 525)
plot.ts(sock_ts)
fit_sock<-ets(sock$sales)
forecast(fit_sock,5)
plot(forecast(fit_sock,5))
fit_arima_sock<-auto.arima(sock$sales,ic="bic")
forecast(fit_arima_sock,50)
plot(forecast(fit_arima_sock,7))
sma_sock<-SMA(sock_ts,n=3)
plot.ts(sma_sock)
HoltWinters_sock<-HoltWinters(sock_ts,gamma = FALSE)
HoltWinters_sock_for<-forecast.HoltWinters(HoltWinters_sock,h=196)
plot.forecast(HoltWinters_sock_for)
acf(HoltWinters_sock_for$residuals,lag.max = 20)
Box.test(HoltWinters_sock_for$residuals,lag= 20,type="Ljung-Box")

#Time Series analysis for collective toys
collect_ts<-ts(collect$sales,start = c(2014,7),end = c(2016,1),frequency = 525)
plot.ts(collect_ts)
fit_sock<-ets(collect$sales)
forecast(fit_collect,5)
plot(forecast(fit_collect,5))
fit_arima_collect<-auto.arima(collect$sales,ic="bic")
forecast(fit_arima_collect,50)
plot(forecast(fit_arima_collect,50))
sma_collect<-SMA(collect_ts,n=3)
plot.ts(sma_collect)
HoltWinters_collect<-HoltWinters(collect_ts,gamma = FALSE)
HoltWinters_collect_for<-forecast.HoltWinters(HoltWinters_collect,h=196)
plot.forecast(HoltWinters_collect_for)
acf(HoltWinters_collect_for$residuals,lag.max = 20)
Box.test(HoltWinters_collect_for$residuals,lag= 20,type="Ljung-Box")

#Time Series analysis for soda
soda_ts<-ts(soda$sales,start = c(2014,7),end = c(2016,1),frequency = 525)
plot.ts(soda_ts)
fit_soda<-ets(soda$sales)
forecast(fit_soda,5)
plot(forecast(fit_soda,5))
fit_arima_soda<-auto.arima(soda$sales,ic="bic")
forecast(fit_arima_soda,50)
plot(forecast(fit_arima_soda,7))
sma_soda<-SMA(soda_ts,n=3)
plot.ts(sma_soda)
HoltWinters_soda<-HoltWinters(soda_ts,gamma = FALSE)
HoltWinters_soda_for<-forecast.HoltWinters(HoltWinters_soda,h=196)
plot.forecast(HoltWinters_soda_for)
acf(HoltWinters_soda_for$residuals,lag.max = 20)
Box.test(HoltWinters_soda_for$residuals,lag= 20,type="Ljung-Box")

#Read the cluster dataset and do the analysis separately
cluster_menglu<-read.csv("zhiye.csv")
cluster_menglu_1<-subset(cluster_menglu,cluster_menglu$Clusters=="cluster-1")
cluster_menglu_2<-subset(cluster_menglu,cluster_menglu$Clusters=="cluster-2")
cluster_menglu_3<-subset(cluster_menglu,cluster_menglu$Clusters=="cluster-3")

#Association rules for cluster-1
cluster_menglu_1$PreferredCustomerNumber<-as.numeric(cluster_menglu_1$PreferredCustomerNumber)
cluster_menglu_1_rules<-data.frame(cluster_menglu_1$PreferredCustomerNumber,cluster_menglu_1$Item.Short.Description)
names(cluster_menglu_1_rules)[1]<-"ID"
names(cluster_menglu_1_rules)[2]<-"Item"
dummy_cluster_1 <- dummyVars(~ID+Item, data=cluster_menglu_1_rules, fullRank=FALSE)
cluster_menglu_1_rules<-na.omit(cluster_menglu_1_rules)
cluster_menglu_1_rules<-as.data.frame(predict(dummy_cluster_1 ,cluster_menglu_1_rules))
cluster_menglu_1_rules<-aggregate(cluster_menglu_1_rules,by=list(cluster_menglu_1_rules$ID),FUN=sum)
names(cluster_menglu_1_rules)
cluster_menglu_1_rules$ID<-cluster_menglu_1_rules$Group.1<-NULL

for (i in 1:3724) {
  cluster_menglu_1_rules[[i]][cluster_menglu_1_rules[[i]]>0]<-1
}
cluster_menglu_1_rules_matrix<-as.matrix(cluster_menglu_1_rules)
rules_cluster_apriori_class_menglu_1<-apriori(cluster_menglu_1_rules_matrix,parameter = list(supp=0.0002,conf=0.5))
inspect(rules_cluster_apriori_class_menglu_1)
plot (rules_cluster_apriori_class_menglu_1,method="grouped")

#Association rules for cluster-2
cluster_menglu_2$PreferredCustomerNumber<-as.numeric(cluster_menglu_2$PreferredCustomerNumber)
cluster_menglu_2_rules<-data.frame(cluster_menglu_2$PreferredCustomerNumber,cluster_menglu_2$Class.Description)
names(cluster_menglu_2_rules)[1]<-"ID"
names(cluster_menglu_2_rules)[2]<-"Class"
dummy_cluster_2 <- dummyVars(~ID+Class, data=cluster_menglu_2_rules, fullRank=FALSE)
cluster_menglu_2_rules<-na.omit(cluster_menglu_2_rules)
cluster_menglu_2_rules<-as.data.frame(predict(dummy_cluster_2 ,cluster_menglu_2_rules))
cluster_menglu_2_rules<-aggregate(cluster_menglu_2_rules,by=list(cluster_menglu_2_rules$ID),FUN=sum)
cluster_menglu_2_rules[[1]]<-cluster_menglu_2_rules[[2]]<-NULL
for (i in 1:172) {
  cluster_menglu_2_rules[[i]][cluster_menglu_2_rules[[i]]>0]<-1
}
cluster_menglu_2_rules_matrix<-as.matrix(cluster_menglu_2_rules)
rules_cluster_apriori_class_menglu_2<-apriori(cluster_menglu_2_rules_matrix,parameter = list(supp=0.0003,conf=0.8))
inspect(rules_cluster_apriori_class_menglu_2)
plot (rules_cluster_apriori_class_menglu_2,method="grouped")

#Association rules for cluster-3
cluster_menglu_3$PreferredCustomerNumber<-as.numeric(cluster_menglu_3$PreferredCustomerNumber)
cluster_menglu_3_rules<-data.frame(cluster_menglu_3$PreferredCustomerNumber,cluster_menglu_3$Class.Description)
names(cluster_menglu_3_rules)[1]<-"ID"
names(cluster_menglu_3_rules)[2]<-"Class"
dummy_cluster_3 <- dummyVars(~ID+Class, data=cluster_menglu_3_rules, fullRank=FALSE)
cluster_menglu_3_rules<-na.omit(cluster_menglu_3_rules)
cluster_menglu_3_rules<-as.data.frame(predict(dummy_cluster_3 ,cluster_menglu_3_rules))
cluster_menglu_3_rules<-aggregate(cluster_menglu_3_rules,by=list(cluster_menglu_3_rules$ID),FUN=sum)
cluster_menglu_3_rules[[1]]<-cluster_menglu_3_rules[[2]]<-NULL
for (i in 1:172) {
  cluster_menglu_3_rules[[i]][cluster_menglu_3_rules[[i]]>0]<-1
}
cluster_menglu_3_rules_matrix<-as.matrix(cluster_menglu_3_rules)
rules_cluster_apriori_class_menglu_3<-apriori(cluster_menglu_3_rules_matrix,parameter = list(supp=0.0002,conf=0.6))
inspect(rules_cluster_apriori_class_menglu_3)
plot (rules_cluster_apriori_class_menglu_3,method="graph")
plot(rules_cluster_apriori_class_menglu_3)


#Draw the sales graph of cluster-1
cluster_1_sales<-data.frame(cluster_menglu_1$Net_Sales_Amt,cluster_menglu_1$Transaction_date)
cluster_1_sales<-aggregate(cluster_1_sales$cluster_menglu_1.Net_Sales_Amt,by=list(cluster_1_sales$cluster_menglu_1.Transaction_date),FUN=sum)
cluster_1_sales$time<-strptime(cluster_1_sales$Group.1,format = "%d-%B-%y")
cluster_1_sales$Group.1<-NULL
ggplot(cluster_1_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the sales graph of cluster-2
cluster_2_sales<-data.frame(cluster_menglu_2$Net_Sales_Amt,cluster_menglu_2$Transaction_date)
cluster_2_sales<-aggregate(cluster_2_sales$cluster_menglu_2.Net_Sales_Amt,by=list(cluster_2_sales$cluster_menglu_2.Transaction_date),FUN=sum)
cluster_2_sales$time<-strptime(cluster_2_sales$Group.1,format = "%d-%B-%y")
cluster_2_sales$Group.1<-NULL
ggplot(cluster_2_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the sales graph of cluster-3
cluster_3_sales<-data.frame(cluster_menglu_3$Net_Sales_Amt,cluster_menglu_3$Transaction_date)
cluster_3_sales<-aggregate(cluster_3_sales$cluster_menglu_3.Net_Sales_Amt,by=list(cluster_3_sales$cluster_menglu_3.Transaction_date),FUN=sum)
cluster_3_sales$time<-strptime(cluster_3_sales$Group.1,format = "%d-%B-%y")
cluster_3_sales$Group.1<-NULL
ggplot(cluster_3_sales,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the average sales graph of cluster-1
cluster_1_sales_mean<-data.frame(cluster_menglu_1$Net_Sales_Amt,cluster_menglu_1$Transaction_date)
cluster_1_sales_mean<-aggregate(cluster_1_sales_mean$cluster_menglu_1.Net_Sales_Amt,by=list(cluster_1_sales_mean$cluster_menglu_1.Transaction_date),FUN=mean)
cluster_1_sales_mean$time<-strptime(cluster_1_sales_mean$Group.1,format = "%d-%B-%y")
cluster_1_sales$Group.1<-NULL
ggplot(cluster_1_sales_mean,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the average sales graph of cluster-2
cluster_2_sales_mean<-data.frame(cluster_menglu_2$Net_Sales_Amt,cluster_menglu_2$Transaction_date)
cluster_2_sales_mean<-aggregate(cluster_2_sales_mean$cluster_menglu_2.Net_Sales_Amt,by=list(cluster_2_sales_mean$cluster_menglu_2.Transaction_date),FUN=mean)
cluster_2_sales_mean$time<-strptime(cluster_2_sales_mean$Group.1,format = "%d-%B-%y")
cluster_2_sales_mean$Group.1<-NULL
ggplot(cluster_2_sales_mean,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Draw the average sales graph of cluster-3
cluster_3_sales_mean<-data.frame(cluster_menglu_3$Net_Sales_Amt,cluster_menglu_3$Transaction_date)
cluster_3_sales_mean<-aggregate(cluster_3_sales_mean$cluster_menglu_3.Net_Sales_Amt,by=list(cluster_3_sales_mean$cluster_menglu_3.Transaction_date),FUN=mean)
cluster_3_sales_mean$time<-strptime(cluster_3_sales_mean$Group.1,format = "%d-%B-%y")
cluster_3_sales_mean$Group.1<-NULL
ggplot(cluster_3_sales_mean,aes(time,x))+geom_line()+xlab("")+ylab("sales")

#Plot the total sales of cluster 1,2,3
cluster_3_sales$time<-as.Date(cluster_3_sales$time) 
cluster_2_sales$time<-as.Date(cluster_2_sales$time) 
cluster_1_sales$time<-as.Date(cluster_1_sales$time) 
cluster_123_total<-merge(cluster_1_sales,cluster_2_sales,by="time",all=TRUE)
cluster_123_total<-merge(cluster_123_total,cluster_3_sales,by="time",all=TRUE)
names(cluster_123_total)[[2]]<-"cluster-1"
names(cluster_123_total)[[3]]<-"cluster-2"
names(cluster_123_total)[[4]]<-"cluster-3"
k <- ggplot(cluster_123_total,aes(cluster_123_total$time))
k <- k + geom_line(aes(y=cluster_123_total$`cluster-1`),color="red")
k <- k + geom_line(aes(y=cluster_123_total$`cluster-2`), colour="blue")
k <- k + geom_line(aes(y=cluster_123_total$`cluster-3`), colour="yellow")
k

#Plot the mean sales of cluster 1,2,3
cluster_3_sales_mean$time<-as.Date(cluster_3_sales_mean$time) 
cluster_2_sales_mean$time<-as.Date(cluster_2_sales_mean$time) 
cluster_1_sales_mean$time<-as.Date(cluster_1_sales_mean$time) 
cluster_123_mean$Group.1<-NULL
cluster_123_mean<-merge(cluster_1_sales_mean,cluster_2_sales_mean,by="time",all=TRUE)
cluster_123_mean<-merge(cluster_123_mean,cluster_3_sales_mean,by="time",all=TRUE)
names(cluster_123_mean)[[2]]<-"cluster-1"
names(cluster_123_mean)[[3]]<-"cluster-2"
names(cluster_123_mean)[[4]]<-"cluster-3"
ave <- ggplot(cluster_123_mean,aes(cluster_123_mean$time))
ave <- ave + geom_line(aes(y=cluster_123_mean$`cluster-1`),color="red")
ave <- ave + geom_line(aes(y=cluster_123_mean$`cluster-2`), colour="blue")
ave <- ave + geom_line(aes(y=cluster_123_mean$`cluster-3`), colour="yellow")
ave




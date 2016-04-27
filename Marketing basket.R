setwd("/Users/GreenTea/Desktop/RPI Data")
install.packages("data.table")
install.packages("lattice")
install.packages("bit64")
install.packages("caret")
install.packages("Hmisc")
library(Hmisc)
library(arules)
library(bit64)
library(caret)
library(ggplot2)
library(data.table)
library(lattice)
#merge sales and items data in 201424
Items_All_201424<-read.csv("Items_All_201424.csv",sep = ",",quote = "/",na.strings = "NA", header = TRUE, row.names = NULL, fileEncoding = "UTF-16LE")
Sales_Week_201424<-read.csv("Sales_Week_201424.csv",sep = ",",quote = "/",na.strings = "NA", header = TRUE, row.names = NULL, fileEncoding = "UTF-16LE")
merge_201424<-merge(Items_All_201424,Sales_Week_201424,by="UPC")


#set dummy variable of Class.Description for Apriori
dummy_201424 <- dummyVars(~Transaction.Number+Class.Description, data=merge_201424, fullRank=FALSE)
mb201424<- as.data.frame(predict(dummy_201424,merge_201424))
y<-aggregate(mb201424, by=list(mb201424$Transaction.Number), FUN=sum)

rules <- apriori(mb201424, parameter = list(supp = 0.1, conf = 0.8, target = "Class.Description.ADULT DVD"))
mb201424<-as.character(mb201424)
#Sample apriori, but has error when changed to facotr
mb2014<-mb201424[1:20]
rules <- apriori(mb2014, parameter = list(supp = 0, conf = 0.3, target = "Class.Description.ACTION ADVENTURE DVD"))
mb2014<-as.factor(mb2014)
#read all the items data and rbind
items <- list.files(pattern = "Items")

items<-read.csv("Item_All_Unique.csv")
sales<- as.data.frame(rbindlist(lapply(sale, read.csv, sep = ",", quote="/", na.strings="NA", header = TRUE, row.names = NULL, fileEncoding = "UTF-16LE")))
head <- head(files2, n=100)
sale<-list.files(pattern = "Sales_Week_2014")
total_2015<-merge(sales,items,by="UPC",all.x=TRUE)
items<-as.data.frame(items)
describe(total_2015$Department.Description)
sort(table(total_2015$Division.Description), decreasing = TRUE)
write.csv(total_2015, file = "MyData.csv")
total_2015<-read.csv("Total_2015.csv")
sort(table(total_2015$Department.Description), decreasing = TRUE)

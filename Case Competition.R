#Data Preparation
setwd("/Users/GreenTea/Desktop/Case Competition")
data<- read.csv("station-data.csv", header = TRUE, sep = ",")
status<- read.csv("station-status.csv", header = TRUE, sep = ",")
citi201307<- read.csv("2013-07 - Citi Bike trip data.csv", header = TRUE, sep = ",")
onstreet<- read.csv("TIMS_DCPBikeCounts_ON_Street_2014.csv", header = TRUE, sep = ",")
offstreet<- read.csv("TIMS_DCPBikeCounts_OFF_Street_2014.csv", header = TRUE, sep = ",")
merge<-merge(data, status, by.data = "id", by.status = "id", all = TRUE)
sum<-aggregate(status$availableDocks, by=list(status$id), FUN=sum)
names(sum)[1] <- "id"
mergesum<-merge(sum, data, by.sum = "id", by.data = "id", all = TRUE)
write.csv(mergesum, file = "sum.csv")
d <- data.frame(mergesum$id,mergesum$x)
mean<-mean(d$mergesum.x)
max<-max(d$mergesum.x)
min<-min(d$mergesum.x)
median<-median(d$mergesum.x)
var<-var(d$mergesum.x)
hist(d$mergesum.x)
library(rgdal)
library(sp)
meanofdocker<-aggregate(status$availableDocks, by=list(status$id), FUN=mean)
hist(meanofdocker$x)
sumofdocker<-sum(meanofdocker$x)
d <- dist(meanofdocker, method = "euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)
rect.hclust(fit, k=5, border="red")
#Google Map API
ds <- read.csv("station-status.csv",stringsAsFactor = F)
ds <- ds[order(ds$id,ds$lastCommunicationTime),]
ds2<-read.csv("subway&college.csv",stringsAsFactor = F)
api.key <- "AIzaSyB3lAjlc4DDETZSSdGxVYXRat7FI4tWiCg"
ple <- function(id1,id2) {
  ds.i <- ds2[ds2$id == id1,]
  ds.i2 <- ds2[ds2$id == id2,]
  url.i <- "https://maps.googleapis.com/maps/api/directions/json?origin="
  url.i <- paste(url.i,ds.i[,"latitude"],sep = "")
  url.i <- paste(url.i,ds.i[,"longitude"],sep = ",")
  url.i <- paste(url.i,"&destination=",ds.i2[,"latitude"],sep = "")
  url.i <- paste(url.i,ds.i2[,"longitude"],sep = ",")
  url.i <- paste(url.i,"&mode=bicycling&key=",api.key,sep = "")
  url.content <- readLines(url.i)
  x <- url.content[grep("distance",url.content)+1]
  distance <- unlist(lapply(strsplit(x,"\""),function(x) x[4]))
  y <- url.content[grep("distance",url.content)+2]
  value <- unlist(lapply(strsplit(y,"\""),function(x) x[3]))
  value <- as.numeric(unlist(lapply(strsplit(value,":"),function(x) x[2])))
  result <- data.frame(origin_id = id1,destination_id = id2,distance = distance, value = value )
  return(result)
}
y<-id1.ds
y<-aggregate(y$value, by=list(y$origin_id,y$destination_id), FUN=sum)
for(i in 4:100){
  id1 <- ds2[1,1] 
  ids <- ds2$id[420:781] 
  ids <- ids[!ids %in% id1
             id1.ds <- numeric()
             for(id2 in ids) {
               id1.ds <- rbind(id1.ds,ple(id1,id2))}
             id1.ds<-aggregate(id1.ds$value, by=list(id1.ds$origin_id,id1.ds$destination_id), FUN=sum)
             y<-rbind2(y,id1.ds)
             ｝
             


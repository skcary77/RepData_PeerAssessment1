#loading and processing the data
data <- read.table(unz("activity.zip", "activity.csv"), 
                   header=TRUE, sep=",",stringsAsFactors=FALSE,
                   colClasses = c("numeric","Date","numeric"))

# What is mean total number of steps taken per day?
#1 histogram
#load the plyr library
library(plyr)
#first remove all of the NAs
daySums <- data[!is.na(data$steps),]
#calculate sum for each day.
daySums <- ddply(daySums,~date,summarize,sum=sum(steps))
#now when we plot the histogram any NAs will be removed, leaving a total of 53 out of 61 days
with(daySums,hist(sum,col="red",main="Total Steps Taken Each Day",xlab="Total Steps",labels=TRUE))
#calculate the mean and median of this
mean(daySums$sum)
median(daySums$sum)

#what is average daily activity pattern
intAvg <- data[!is.na(data$steps),]
intAvg <- ddply(intAvg,~interval,summarize,average=mean(steps))
#plot
with(intAvg,plot(interval,average,type="l",
                 main="Average Steps Taken Per Interval" ,ylab="Average Steps",xlab="Interval"))
#find the max
intAvg$interval[which(intAvg$average == max(intAvg$average))]
maxRow <- which(intAvg$average == max(intAvg$average))
points(intAvg$interval[maxRow],intAvg$average[maxRow],pch=20)


#Inputting Missing Values
#1 calculate and report the number of missing values in the dataset
sum(is.na(data$steps))


imputedData <- data
#devise a strategy for filling in the missing NAs
#find all the NAs
#or else subset all the NAs, then use sapply, the rbind those and order by date and interval
for(i in row.names(imputedData[is.na(imputedData$steps),])){
        imputedData[i,"steps"] <- intAvg[which(imputedData[i,"interval"] == intAvg$interval),"average"]
        
}

#other method-MUCH FASTER
NAs <- data[is.na(data$steps),]
returnIntAvg <- function(interval){
        avg <- intAvg[which(interval == intAvg$interval),"average"]
        avg      
}
imputedData[is.na(imputedData$steps),"steps"] <- sapply(imputedData[is.na(imputedData$steps),"interval"],returnIntAvg)


#4 make histogram and report mean and median
imputedSums <- ddply(imputedData,~date,summarize,sum=sum(steps))
with(imputedSums,hist(sum,col="red",main="Total Steps Taken Each Day",xlab="Total Steps",labels=TRUE))
#calculate the mean and median of this
mean(imputedSums$sum)
median(imputedSums$sum)

summaryTable <- data.frame("data" = c(mean(daySums$sum),median(daySums$sum)), 
                           "imputedData" = c(mean(imputedSums$sum),median(imputedSums$sum)),
                           row.names=c("mean","median"))


#create dayType factor variable
imputedData$dayType <- weekdays(imputedData$date)
#create convertDays function
convertDays <- function(day){
        if(day == "Saturday" | day == "Sunday"){
                day <- "weekend"
        } else{
                day <- "weekday"
        }
}
imputedData$dayType <- sapply(imputedData$dayType,convertDays)
imputedData$dayType <- as.factor(imputedData$dayType)

#calculate weekday/weekend averages
imputedAvg <- ddply(imputedData,c("interval","dayType"),summarize,average=mean(steps))
#plot chart
library(lattice)
xyplot(average~interval | dayType, data = imputedAvg, type="l",layout=c(1,2))


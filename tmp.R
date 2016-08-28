setwd('/Users/julienbalmont/Dev/datasciencecoursera/RepData_PeerAssessment1/')
unzip("./activity.zip")
activityData <- read.csv("./activity.csv")
stepsPerDay <- aggregate(steps ~ date, activityData, sum, na.rm=TRUE)
meanStepsPerDay <- mean(stepsPerDay$steps)
medianStepsPerDay <- median(stepsPerDay$steps)
stepsPerInterval<-aggregate(steps~interval, data=activityData, mean, na.rm=TRUE)
intervalWithMaxNbSteps <- stepsPerInterval[which.max(stepsPerInterval$steps),]$interval
totalValuesMissings <- sum(is.na(activityData$steps))
meanStepsPerDay <- aggregate(steps ~ date, data=activityData, mean, na.rm=TRUE)

getMeanStepsForDay <- function (particularDate) {
    meanStepsPerDay[meanStepsPerDay$date == particularDate,]$steps
}

activityDataNoNA <- activityData
nrows = nrow(activityDataNoNA)
for(line in 1:nrows) {
    if(is.na(activityDataNoNA[line,]$steps)) {
        activityDataNoNA[line,]$steps <- getMeanStepsForDay(activityDataNoNA[line,]$date)
    }
}

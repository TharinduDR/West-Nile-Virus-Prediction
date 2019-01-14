library(plyr)
library(ggplot2)

train = read.csv("Data/train.csv",stringsAsFactors=FALSE)
test = read.csv("Data/test.csv" ,stringsAsFactors=FALSE)
submission = read.csv("Data/submission.csv" ,stringsAsFactors=FALSE)

train <- merge(test,submission)


train$Date = as.Date(train$Date)
lt = as.POSIXlt(train$Date)
woy = floor((lt$yday - lt$wday + 7) / 7)

# implicit model of the data collection
hits = lt$wday == 1 & lt$year == 113 & woy == 27
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 111 & woy == 28
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 111 & woy == 30
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 111 & woy == 37
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 4 & lt$year == 111 & woy == 35
train$Date[hits] = train$Date[hits] + 1
hits = lt$wday == 2 & lt$year == 109 & woy == 22
train$Date[hits] = train$Date[hits] - 4
hits = lt$wday == 1 & lt$year == 109 & woy == 24
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 109 & woy == 25
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 109 & woy == 26
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 109 & woy == 27
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 109 & woy == 28
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 1 & lt$year == 109 & woy == 30
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 2 & lt$year == 109 & woy == 34
train$Date[hits] = train$Date[hits] - 4
hits = lt$wday == 1 & lt$year == 109 & woy == 37
train$Date[hits] = train$Date[hits] - 3
hits = lt$wday == 5 & lt$year == 107 & woy == 25
train$Date[hits] = train$Date[hits] + 3

train$Date = as.POSIXlt(train$Date)
train$Year = as.factor(train$Date$year+1900)
train$Week = floor((train$Date$yday - train$Date$wday + 7) / 7)
ftrain = ddply(train,.(Week,Year),summarize,WnvCount=sum(WnvPresent))
ftrain = ftrain[order(ftrain$Year,ftrain$Week),]

ggplot(ftrain,aes(x=Week,y=WnvCount,colour=Year))+ geom_line() 


ftrain$WnvCount2 = filter(ftrain$WnvCount,c(0.25,0.5,0.25))
ftrain$WnvCount2[is.na(ftrain$WnvCount2)] = ftrain$WnvCount[is.na(ftrain$WnvCount2)]
ftrain$WnvCount = ftrain$WnvCount2
ymax = ddply(ftrain,.(Year),summarize,yearMax=max(WnvCount))
ftrain = merge(ftrain,ymax,by="Year")
ftrain$WnvCountScaled = ftrain$WnvCount / ftrain$yearMax

ggplot(ftrain,aes(x=Week,y=WnvCountScaled))+ geom_line() + facet_wrap(~Year,ncol=2)
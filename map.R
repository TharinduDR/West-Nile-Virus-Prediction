library(readr)
library(ggmap)
library(geosphere)
library(scales)
library(lubridate)

distance <- function(longitude, latitude) {
  
  #Euclidian distances aren't accurate because we are on a sphere
  #dist1 <- sqrt((stations[1,]$Latitude-latitude)^2+(stations[1,]$Longitude-longitude)^2)
  #dist2 <- sqrt((stations[2,]$Latitude-latitude)^2+(stations[2,]$Longitude-longitude)^2)
  
  #Instead, let's use distHaversine from geosphere
  #Haversine distance : http://en.wikipedia.org/wiki/Haversine_formula
  
  dist1 <- distHaversine(c(stations[1,]$Longitude,stations[1,]$Latitude),c(longitude,latitude))
  dist2 <- distHaversine(c(stations[2,]$Longitude,stations[2,]$Latitude),c(longitude,latitude))
  
  
  if(dist1<dist2){
    return(1)
  }
  return(2)
}

mapdata <- readRDS("Data/mapdata_copyright_openstreetmap_contributors.rds")
train <- read.csv("Data/train.csv")

#Station 1: CHICAGO O'HARE INTERNATIONAL AIRPORT Lat: 41.995 Lon: -87.933 Elev: 662 ft. above sea level
#Station 2: CHICAGO MIDWAY INTL ARPT Lat: 41.786 Lon: -87.752 Elev: 612 ft. above sea level

stations<-data.frame(c(1,2),c(41.995,41.786),c(-87.933,-87.752))
names(stations)<-c("Station","Latitude","Longitude")

train$Station<-mapply(distance,train$Longitude,train$Latitude)

#Let's make a plot to see if everything is OK
pl <- ggmap(mapdata)+geom_point(data=train,aes(x=Longitude,y=Latitude,color=factor(Station)))+geom_point(data=stations,aes(x=Longitude, y=Latitude,colour=Station),colour=c("darkred","deepskyblue"),size=4)

ggsave("rplot2.jpg", pl, dpi = 100, width = 10, height = 15, units="in")


weather <- read.csv("Data/weather.csv")

train <- merge(train, weather)

train$Tavg <- as.integer(train$Tavg)

wnvpresent <- aggregate(train$WnvPresent, by=list(Date=train$Date), FUN=sum)
names(wnvpresent)[names(wnvpresent)=="x"] <- "WnvCount"

temperature <- aggregate(train$Tavg, by=list(Date=train$Date), FUN=mean)
names(temperature)[names(temperature)=="x"] <- "AvgTemp"

train$AvgSpeed <- as.numeric(paste(train$AvgSpeed))
AvgSpeed <- aggregate(train$AvgSpeed, by=list(Date=train$Date), FUN=mean)
names(AvgSpeed)[names(AvgSpeed)=="x"] <- "AvgSpeed"

train$WetBulb <- as.numeric(train$WetBulb)
WetBulb <- aggregate(train$WetBulb, by=list(Date=train$Date), FUN=mean)
names(WetBulb)[names(WetBulb)=="x"] <- "WetBulb"

DewPoint <- aggregate(train$DewPoint, by = list(Date = train$Date), FUN = mean)
names(DewPoint)[names(DewPoint)=="x"] <- "DewPoint"


graph <- merge(wnvpresent,temperature)
graph <- merge(graph,WetBulb)
graph <- merge(graph, DewPoint)
graph <- merge(graph, AvgSpeed)

graph$Date <- as.Date(graph$Date, format = "%Y-%m-%d")
graph$Year <- as.numeric(format(graph$Date, "%Y"))
graph$Daynumber <- yday(graph$Date)

ggplot(graph,aes(x = Daynumber,y = AvgTemp, group = 1)) +
  geom_point(aes(size = WnvCount), colour = "red", position = "jitter")  +
  ggtitle ("Daily average temperature") +
  xlab("Date") +  ylab ("Average Temperature ( ºF )") +  facet_wrap(~Year,ncol=2)


ggplot(graph,aes(x = Daynumber,y = WetBulb, group = 1)) +
  geom_point(aes(size = WnvCount), colour = "red", position = "jitter")  +
  ggtitle ("Daily average WetBulb level") +
  xlab("Date") +  ylab ("WetBulb Level") +  facet_wrap(~Year,ncol=2)

ggplot(graph,aes(x = Daynumber,y = DewPoint, group = 1)) +
  geom_point(aes(size = WnvCount), colour = "red", position = "jitter")  +
  ggtitle ("Daily average Dew Point") +
  xlab("Date") +  ylab ("Dew Point Level") +  facet_wrap(~Year,ncol=2)



ggplot(graph,aes(x = Daynumber,y = DewPoint, group = 1)) +
  geom_point(aes(size = WnvCount), colour = "red", position = "jitter")  +
  ggtitle ("Daily average Dew Point") +
  xlab("Date") +  ylab ("Dew Point Level") +  facet_wrap(~Year,ncol=2)

ggplot(graph,aes(x = Daynumber,y = AvgSpeed, group = 1)) +
  geom_point(aes(size = WnvCount), colour = "red", position = "jitter")  +
  ggtitle ("Daily average Wind speed") +
  xlab("Date") +  ylab ("Average Speed") +  facet_wrap(~Year,ncol=2)


train$ThunderStorm <- grepl("TS", train$CodeSum)
prop.table(table(train$ThunderStorm, train$WnvPresent),1)

train$Fog <- grepl("FG", train$CodeSum)
prop.table(table(train$WnvPresent, train$Fog),1)



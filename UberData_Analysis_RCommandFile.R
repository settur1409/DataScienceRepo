library(ggplot2)
library(lubridate)

#Reading data from csv
uber_data = read.csv("Uber Request Data.csv", na.strings = c("", " ", NA), stringsAsFactors = F)

#Aligning time formats for request timestamp
reqTimeRowList = grep("[A-P]M", uber_data[,5])
strptime(uber_data[reqTimeRowList,5], format = "%d/%m/%y %I:%M %p")
uber_data[reqTimeRowList,5] = format(strptime(uber_data[reqTimeRowList,5], format = "%d/%m/%y %I:%M %p"), "%c")
uber_data[-reqTimeRowList,7] = format(strptime(uber_data[-reqTimeRowList,5], format = "%d-%m-%Y %I:%M:%S"), "%c")
uber_data[-reqTimeRowList,7] = format(strptime(uber_data[-reqTimeRowList,5], format = "%d-%m-%Y %H:%M:%S"), "%c")
uber_data[-reqTimeRowList,5] = uber_data[-reqTimeRowList,7]

#Aligning time formats for drop timestamp
DropTimeRowList = which(is.na(strptime(successTrips[,6], format = "%d/%m/%Y %H:%M")) == FALSE)
successTrips[DropTimeRowList,8] = format(strptime(successTrips[DropTimeRowList,6], format = "%d/%m/%Y %H:%M"), "%c")
successTrips[-DropTimeRowList,8] = format(strptime(successTrips[-DropTimeRowList,6], format = "%d-%m-%Y %I:%M:%S"), "%c")
successTrips[-DropTimeRowList,8] = format(strptime(successTrips[-DropTimeRowList,6], format = "%d-%m-%Y %H:%M:%S"), "%c")
successTrips[,6] = successTrips[,8]

#subsetting data for ease of analysis
successTrips = subset(uber_data, Status == "Trip Completed")
CancelledTrips = subset(uber_data, Status == "Cancelled")
noCabs_data = subset(uber_data, Status == "No Cars Available")
noCabs_data_asPick_from_Airport = subset(noCabs_data, Pickup.point == "Airport")

#noCabs_data$NewDate = as.Date(noCabs_data$Request.timestamp, format = "%a %b %d %H:%M:%S %Y")
#noCabs_data$weekday = weekdays(noCabs_data$NewDate)

## Visualisations using ggplot2
ggplot(data = noCabs_data_asPick_from_Airport, aes(x= factor(weekday), fill = weekday)) + 
  ggtitle("Exploring cab non-availability from airport based on weekday") + 
  stat_count()


#ggplot(data = uber_data, aes(x = factor(Pickup.point), fill = Status))  + 
#  ggtitle("Summary of Cab's availability based on pickup point") + 
#  geom_bar()

ggplot(data = uber_data, aes(x = factor(Pickup.point), fill = Status))  + 
  ggtitle("Summary of Cab's availability based on pickup point") + 
  geom_bar( position = position_dodge())

##Converting timestamp to POSIXlt format to get hour details
uber_data$hour = as.POSIXlt(strptime(uber_data$Request.timestamp, "%a %b %d %H:%M:%S %Y"))$hour
CancelledTrips$hour = as.POSIXlt(strptime(CancelledTrips$Request.timestamp, "%a %b %d %H:%M:%S %Y"))$hour
noCabs_data$hour = as.POSIXlt(strptime(noCabs_data$Request.timestamp, "%a %b %d %H:%M:%S %Y"))$hour

##Retriving weekday out of requested timestamp
uber_data$weekday = format(strptime(uber_data$Request.timestamp, "%a %b %d %H:%M:%S %Y"), "%a")

## Visualisations using ggplot2
ggplot(data = uber_data, aes(x = factor(hour), fill = Status)) + 
  ggtitle("Cab status - hourly based") + 
  geom_bar()

ggplot(data = uber_data, aes(x = factor(weekday), fill = Status)) + 
  ggtitle("Cab availablity highlighting status - weekday based") + 
  geom_bar(position = position_dodge())

ggplot(data = noCabs_data, aes(x = factor(hour), fill = Pickup.point)) + 
  ggtitle("Cab non-availablity highlighting pickup point - hourly based") +
  geom_bar(position = position_dodge())


#exploring timegap in sucesstrips
successTrips$NewReqTimestamp = as.POSIXlt(successTrips$Request.timestamp, format = "%a %b %d %I:%M:%S %Y")
successTrips$NewReqTimestamp = as.POSIXlt(successTrips$Request.timestamp, format = "%a %b %d %H:%M:%S %Y")
successTrips$NewDropTimestamp = as.POSIXlt(successTrips$Drop.timestamp, format = "%a %b %d %H:%M:%S %Y")
successTrips$timeGap = successTrips$NewDropTimestamp - successTrips$NewReqTimestamp
successTrips$timeGap = round(successTrips$timeGap, digits = 0)
successTrips$ReqHour = (successTrips$NewReqTimestamp)$hour
successTrips$weekday = weekdays(successTrips$NewReqTimestamp)

#Visuslising time gap with requested hour
ggplot(data = successTrips, aes(x = ReqHour, y = timeGap, col = weekday)) + 
  ggtitle("request hour to time taken to complete trip - weekday based") + 
  stat_smooth(size=2)

#Identifying the gap due to cancelledtrips
ggplot(data = CancelledTrips, aes(x = factor(hour), fill = Pickup.point)) + 
  ggtitle("Cancelled Cab highlighting pickup point - hourly based") +
  geom_bar(position = position_dodge())


CancelledTrips$Request_id = CancelledTrips$Request.id
CancelledTrips$Driver_id = CancelledTrips$Driver.id
## top-3 driver ids having high cancellations.
sqldf::sqldf("select Driver_id, count(Request_id) from CancelledTrips group by Driver_id order by count(Request_id) desc limit 3")
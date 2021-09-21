install.packages("plyr")
library(plyr)
library(lubridate)
library(tidyverse)
library(geosphere)
library(janitor)
library(sqldf)

May2020 <- read.csv("202005.csv")
Jun2020 <- read.csv("202006.csv")
Jul2020 <- read.csv("202007.csv")
Aug2020 <- read.csv("202008.csv")
Sep2020 <- read.csv("202009.csv")
Oct2020 <- read.csv("202010.csv")
Nov2020 <- read.csv("202011.csv")
Dec2020 <- read.csv("202012.csv")
Jan2021 <- read.csv("202101.csv")
Feb2021 <- read.csv("202102.csv")
Mar2021 <- read.csv("202103.csv")
Apr2021 <- read.csv("202104.csv")


bike_data <- rbind(May2020, Jun2020, Jul2020, Aug2020, Sep2020, Oct2020, Nov2020, Dec2020, Jan2021, Feb2021, Mar2021, Apr2021)

#cleaning the rows
bike_data <- janitor::remove_empty(bike_data, which = c("cols"))
bike_data <- janitor::remove_empty(bike_data, which = c("rows"))


#changing the time format so the columns are easier to work with
bike_data <- bike_data %>%
  mutate(started_at = as.POSIXct(started_at,format = "%Y-%m-%d %H:%M:%S"),
         ended_at = as.POSIXct(ended_at,format = "%Y-%m-%d %H:%M:%S"))
bike_data$duration <- bike_data$ended_at - bike_data$started_at
bike_data$duration_in_minutes <- round(bike_data$duration/60, digits = 0)


#removing the word secs from the conversion I made into minutes but the word mins isn't replacing it.
bike_data$duration_in_minutes <- gsub('secs', 'mins', bike_data$duration_in_minutes)
bike_data$duration_in_minutes <- round(bike_data$duration_in_minutes, digits = 0)

#grabbing the distance
bike_data$distance <- distHaversine(cbind(bike_data$start_lng, bike_data$start_lat), cbind(lag(bike_data$end_lng), lag(bike_data$end_lat)))
#conversion to miles
bike_data$distance_miles <- round(bike_data$distance * 0.000621, digits = 2)

#separating the columns of date and time to make them easier to work with
bike_data <- separate(bike_data, started_at, c("start_date", "start_time"), sep = " ")
bike_data <- separate(bike_data, ended_at, c("end_date", "end_time"), sep = " ")
bike_data$day <- weekdays(as.Date(bike_data$start_date))
bike_data$month <- months(as.Date(bike_data$start_date))

#converting the time into something I can round down.
bike_data$start_time <- as.POSIXct(bike_data$start_time, format = "%H:%M:%S")
bike_data$start_time <- round_date(bike_data$start_time, '1 hour')
bike_data <- separate(bike_data, start_time, c("start_date", "start_time"), sep = " ")
#converting to 12hr
bike_data$start_time <- format(strptime(bike_data$start_time, '%H:%M:%S'), '%I:%M %p')

#rounding distance
bike_data$distance_miles <- round(bike_data$distance_miles, digits = 0)

colnames(bike_data)

#selecting only the columns that need to be analyzed
Cyclistic_Data <- sqldf("SELECT ride_id, rideable_type, start_time, start_station_name, start_station_id, end_station_name, end_station_id, member_casual, duration_in_minutes, distance_miles, day, month FROM bike_data")

#further cleaning
Cyclistic_Data <- sqldf("SELECT * FROM Cyclistic_Data WHERE distance_miles != 0 AND duration_in_minutes <= 720")
Cyclistic_Data$duration_in_minutes <- as.integer(Cyclistic_Data$duration_in_minutes)
Cyclistic_Data$duration_in_minutes <- round(Cyclistic_Data$duration_in_minutes, digits = 0)
Cyclistic_Data$distance_miles <- round(Cyclistic_Data$distance_miles)

#basic information
sqldf("SELECT AVG(distance_miles), AVG(duration_in_minutes) 
      FROM Cyclistic_Data 
      WHERE member_casual != 'casual'")

sqldf("SELECT AVG(distance_miles), AVG(duration_in_miles) 
      FROM Cyclistic_Data 
      WHERE member_casual = 'casual'")


sqldf("SELECT COUNT(start_station_name) AS Starting_Rides, start_station_name, 
      end_station_name 
      FROM Cyclistic_Data 
      WHERE member_casual = 'casual' 
      Group By start_station_name 
      Order By Starting_Rides DESC
      LIMIT 10")

sqldf("SELECT COUNT(start_station_name) AS Starting_Rides, start_station_name, 
      end_station_name 
      FROM Cyclistic_Data 
      WHERE member_casual != 'casual' 
      Group By start_station_name 
      Order By Starting_Rides DESC
      LIMIT 10")



write.csv(Cyclistic_Data,"~/Desktop/R Projects/CyclisticData.csv", row.names = FALSE)

#Graphing

d <- ggplot(data = Cyclistic_Data)


#graph to map out user preferences
d + geom_bar(mapping = aes(x = rideable_type, fill = member_casual), 
             stat = "count", position = position_dodge()) + 
  labs(x = "Type of Bike", y = "# of Rides", fill = "User") + 
  ggtitle("Preferred mode of Travel") + theme(plot.title = element_text(hjust = 0.5))

#graph to show monthly rider
d + geom_bar(mapping = aes(x = month, fill = member_casual),
  stat = "count", position = position_dodge()) +
  labs(x = "Month", y = "# of Rides", fill = "User") + 
  ggtitle("Monthly Count of Riders") + theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_discrete(limits = month.name)


#formatting the days so that they are ordered when I graph them 
Cyclistic_Data$formatted_dates <- factor(Cyclistic_Data$day, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#graphing number of rides a day
ggplot(data = Cyclistic_Data) + geom_histogram(mapping = aes(x = formatted_dates, fill = member_casual), 
  stat = "count", position = position_dodge()) +
  labs(x = "Day", y = "# of Rides", fill = "User") + 
  ggtitle("Daily Count of Riders") + theme(plot.title = element_text(hjust = 0.5))


#creating 24hr time block to better order the time
Cyclistic_Data$formatted_24hr <- format(strptime(Cyclistic_Data$start_time, "%I:%M %p"), format="%H:%M:%S")

ggplot(data = Cyclistic_Data) + geom_bar(mapping = aes(x = formatted_24hr, fill = member_casual), 
  stat = "count", position = position_dodge()) +
  labs(x = "Time of Day", y = "# of Rides", fill = "User") + 
  ggtitle("Hourly Count of Riders") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))

ggplot(data = Cyclistic_Data) + geom_line(mapping = aes(x = distance_miles , color = member_casual), 
  stat = "count", position = position_dodge()) +
  labs(x = "Distance Traveled (miles)", y = "# of Rides", fill = "User") +
  ggtitle("Distance Traveled by Users") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x=element_text(angle=45,hjust=1))



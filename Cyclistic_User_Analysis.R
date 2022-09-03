library(plyr)
library(dplyr)
library(readr)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)

# create a list with all the .csv files in folder
folder <- "directory"
setwd(folder)
file_list <- list.files(path= folder,
           pattern="*.csv")

# create a data frame out of all .csv files in folder
data <- ldply(file_list, read_csv)

# check the data
summary(data)

# check for missing values
sapply(data, function(x) sum(is.na(x)))

# remove rows with missing values
data <- data[complete.cases(data),]

# check if ride_id is unique
length(unique(data[["ride_id"]])) == nrow(data)

# calculate the ride lengths 
data <- data %>% mutate(ride_length = hms::as_hms(difftime(ended_at,started_at)))

# calculate the ride lengths as numeric
data$ride_length_sec <- as.numeric(data$ended_at - data$started_at)

# calculate the ride length in minutes
data$ride_length_min <- data$ride_length_sec/60

# calculate the hour the ride started at
data$started_at_hour <- as.numeric(format(data$started_at, '%H'))

# calculate the month each ride started
data <- data %>% mutate(month=month((started_at), label=TRUE))

# calculate the day of the week that each ride started
data <- data %>% mutate(weekday= wday((started_at), label = TRUE)) 

# check the station the rides start at
length(unique(data$start_station_name))
length(unique(data$start_station_id))

# 9 station names are not correct
# have to decide whether to remove or replace them

# list of all start station id with more than one name
data[!duplicated(data[ , c("start_station_name")]), ] %>% add_count(start_station_id) %>% filter(n>1) %>% arrange(start_station_name) %>% 
  select("start_station_name", "start_station_id")


data[!duplicated(data[ , c("start_station_name", "start_station_id")]), ] %>% add_count(start_station_id) %>% filter(n>1)
 

data %>% filter(start_station_id=="TA1305000039") %>% group_by(start_station_name) %>% summarise(n=n())
data %>% filter(start_station_id=="TA1305000039", start_station_name=="Marshfield Ave & Cortland St") %>% group_by(start_station_name) %>% summarise(n=n())
data %>% filter(start_station_id=="TA1305000039", start_station_name=="Elston Ave & Cortland St") %>% summarise(count(data$start_lat))




# create a data frame with unique stations ids and name
stations <- data[c("start_station_name","start_station_id")] %>% group_by(start_station_name) %>% mutate(freq = n()) %>% arrange(desc(freq)) 
stations <- stations[!duplicated(stations$start_station_id),1:2]

 
stations[!duplicated(stations[ , c("start_station_name", "start_station_id")]), ] %>% add_count(start_station_id) %>% filter(n>1)

stations %>% group_by(start_station_name, start_station_id) %>% summarise(n=n())

# data frame containing the correct start_station_id
merged_data <- left_join(data, stations, by="start_station_id")

head(merged_data)
head(data)
merged %>% filter(start_station_name.x != start_station_name.y) %>% group_by(start_station_name.x) %>% summarise(n=n())

#raplace the values in the big data frame
data$start_station_name <- replace(data$start_station_name, data$ride_id==merged_data$ride_id, merged_data$start_station_name.y)

# check for inconsistent start station id
data[!duplicated(data[ , c("start_station_id")]), ] %>% add_count(start_station_name) %>% filter(n>1) %>% arrange(start_station_name) %>% 
  select("start_station_name", "start_station_id") 

# for station named Lakefront Trail & Bryn Mawr Ave, we'll use id KA1504000152
data %>% filter(start_station_name=="Lakefront Trail & Bryn Mawr Ave") %>% group_by(start_station_id) %>% summarise(n=n())
data$start_station_id <- replace(data$start_station_id, data$start_station_id=="15576","KA1504000152")

# for station named Loomis St & 89th St, we'll use id 201022
data %>% filter(start_station_name=="Loomis St & 89th St") %>% group_by(start_station_id) %>% summarise(n=n())
data$start_station_id <- replace(data$start_station_id, data$start_station_id=="20102","201022")

# in depth analysis of ride_length column
# check how many rides under or equal 0 sec are
data %>% summarise(rides_below_0=sum(ride_length<=0))

# rows with ride duration below 0 are being disposed
data <- filter(data, ride_length>0)

# there is a big difference between the 3rd quartile and the max value in the ride_length_sec
boxplot(data$ride_length_sec,
        main = "Ride duration in seconds")
     
# compute outliers
min(boxplot.stats(data$ride_length_sec)$out)

# calculate upper inner fence
fivenum(data$ride_length_sec)[4] + 1.5*IQR(data$ride_length_sec, type=2)

#both methods indicate outlier everything over 2596 seconds
data <- filter(data, ride_length<2596)

# type of members vs month v1
ggplot(data) +
  geom_bar(mapping = aes(x=member_casual, fill=member_casual)) +
  facet_wrap(~month) +
  labs(title = "Number of Monthly Rides for Each Type of Member",
      y = "Number of Rides")

# type of members vs month v2 
# this makes more sense
ggplot(data) +
  geom_bar(mapping = aes(x=month, fill=member_casual)) +
  labs(title = "Number of Monthly Rides for Each Type of Member",
       y = "Number of Rides") +
  theme(plot.title = element_text(hjust = 0.5))

head(data)

# type of members vs weekday
ggplot(data) +
  geom_bar(mapping = aes(x=weekday, fill=member_casual)) +
  labs(title = "Number of Daily Rides for Each Type of Member",
       y = "Number of Rides") +
  theme(plot.title = element_text(hjust = 0.5))

# trip duration per weekday
ggplot(data) +
  geom_bar(mapping = aes(x=ride_length_min, fill=member_casual)) +
  labs(title = "Length of Daily Rides for Each Type of Member",
       y = "Number of Rides") +
  facet_wrap(~weekday) +
  theme(plot.title = element_text(hjust = 0.5))

# number of rides per hour and per weekday
ggplot(data) +
  geom_bar(mapping = aes(x=started_at_hour, fill=member_casual)) +
  facet_wrap(~weekday) +
  labs(title = "Number of Hourly Rides for Each Type of Member",
       y = "Number of Rides") +
  theme(plot.title = element_text(hjust = 0.5))

#delete the 16th column
data <- subset(data, select = -c(16))

# export database to set directory
write.csv(data, "directory", row.names = TRUE)



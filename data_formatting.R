#Date of creation: 11/04/2021
# Bikeshare Project 
#Data cleansing and managing
#This scripts creates a data frame, potentially for each month, that has the number of bikes 
#that start and end trips either by people or delivered by the bikeshare platform vans.

library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)
library(purrr)

#Import data
bikes_june19 <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/201906-capitalbikeshare-tripdata.csv")

#Create standard format time information
bikes_june19$hour <- hour(bikes_june19$Start.date)
bikes_june19$weekday <- weekdays(as.Date(bikes_june19$Start.date))
bikes_june19$month <- month(bikes_june19$Start.date)
bikes_june19$day <-  day(bikes_june19$Start.date)
bikes_june19$Start.date <- ymd_hms(bikes_june19$Start.date)
bikes_june19$End.date <- ymd_hms(bikes_june19$End.date)

#Create a column that tells if the bike starts at the same station it ended
bikes_june19 <- bikes_june19 %>% 
  arrange(Bike.number, Start.date) %>% 
  group_by(Bike.number) %>% 
  mutate(Previous.end.station.number = lag(End.station.number),
         Previous.end.station = lag(End.station)) %>% 
  group_by(Bike.number) %>% 
  mutate(start.equals.end = ifelse(Start.station.number == Previous.end.station.number,1,0))

#extract the rows that don't match starting station with where the bikes ended its last trip
bikes_vans <- bikes_june19 %>% 
  filter(start.equals.end == 0)


# create a function that fills the "missing trip" which was actually a reshuffling


fill_trip_data <- function(row){
  
  New.start.date = ymd_hms(row$Start.date) - seconds(40)
  New.end.date = ymd_hms(row$Start.date) - seconds(30)
  
  station.to.van <- data.frame(Duration = 0,
                               Start.date = New.start.date,
                               End.date = New.end.date,
                               Start.station.number = row$Previous.end.station.number,
                               Start.station = row$Previous.end.station,
                               End.station.number = 0,
                               End.station = "Van",
                               Bike.number = row$Bike.number, 
                               Member.type = row$Member.type,
                               hour = row$hour,
                               weekday = row$weekday,
                               month = row$month,
                               day = row$day,
                               Previous.end.station.number = row$Previous.end.station.number,
                               Previous.end.station = row$Previous.end.station,
                               start.equals.end = 1)
  
  New.start.date = ymd_hms(row$Start.date) - seconds(20)
  New.end.date = ymd_hms(row$Start.date) - seconds(10)
  
  van.to.station <- data.frame(Duration = 0,
                               Start.date = New.start.date,
                               End.date = New.end.date,
                               Start.station.number = 0,
                               Start.station = "Van",
                               End.station.number = row$Start.station.number,
                               End.station = row$Start.station,
                               Bike.number = row$Bike.number, 
                               Member.type = row$Member.type,
                               hour = hour(New.start.date),
                               weekday = row$weekday,
                               month = row$month,
                               day = row$day,
                               Previous.end.station.number = 0,
                               Previous.end.station = "Van",
                               start.equals.end = 1)
  
  result <- bind_rows(station.to.van, van.to.station)
  
  return(result)
}


#the function is applied for the whole list of "missing trips"
bikes_vans_result <- lapply(list(bikes_vans), fill_trip_data)[[1]]

#the missing trips are added to the original data
bikes_june19 <- bikes_june19 %>%
  bind_rows(bikes_vans_result)


#the number of trips ending and starting at each station for month, day and houris calculated
bikes_summary <- bikes_june19 %>% 
  filter(Start.station.number !=0 & End.station.number !=0) %>% 
  group_by(Station = Start.station.number, Station.name = Start.station, month, day, hour) %>% 
  summarise(n.starts = n()) %>% 
  full_join(bikes_june19 %>% 
              filter(End.station.number !=0 & Start.station.number !=0) %>% 
              group_by(Station = End.station.number, Station.name = Start.station,  month, day, hour) %>% 
              summarise(n.ends = n()),
            by = c("Station","Station.name", "month", "day", "hour"))

#the number of bikes taken by van at each station for month, day and houris calculated
bikes_van_summary <- bikes_june19 %>% 
  filter(Start.station.number !=0 & End.station.number ==0) %>% 
  group_by(Station = Start.station.number, Station.name = Start.station,  month, day, hour) %>% 
  summarise(n.to.van = n()) %>% 
  full_join(bikes_june19 %>% 
              filter(End.station.number !=0 & Start.station.number ==0) %>% 
              group_by(Station = End.station.number, Station.name = Start.station, month, day, hour) %>% 
              summarise(n.from.van = n()),
            by = c("Station","Station.name", "month", "day", "hour"))

#the two former data sets are joined 
bikes_summary <- bikes_summary %>% 
  full_join(bikes_van_summary,
             by = c("Station", "Station.name", "month", "day", "hour"))

#the activity for each station is calculated,
#this is the sum of total trips whether starting or ending there
bikes_summary_temp <-bikes_summary %>% 
  group_by(Station) %>% 
  summarise(n.starts = sum(n.starts, na.rm = F),
            n.ends = sum(n.ends, na.rm = T)) %>% 
  mutate(n.starts = ifelse(is.na(n.starts),0,n.starts),
         n.ends = ifelse(is.na(n.ends), 0, n.ends)) %>% 
  mutate(activity = n.starts + n.ends) %>% 
  select(Station, activity)

#a classification for each station is created based on their activity
bikes_summary_temp$activity_clasif <- ifelse(bikes_summary_temp$activity < unname(quantile(bikes_summary_temp$activity, 0.33)),
                                             1,
                                             ifelse(bikes_summary_temp$activity < unname(quantile(bikes_summary_temp$activity, 0.67)),
                                                    2,
                                                    3))


bikes_summary <- bikes_summary %>% 
  left_join(bikes_summary_temp)

write.csv(bikes_summary, 
          "/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/trips_summary.csv")


bikes_endofmay <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/bikes_end_of_may.csv")
bikes_endofmay <- bikes_endofmay[,-1]

bikes_summary <- bikes_summary %>% 
  bind_rows(bikes_endofmay)

bikes_summary <- bikes_summary %>% 
  arrange(Station, month, day, hour)

bikes_summary$n.starts <- ifelse(is.na(bikes_summary$n.starts), 0, bikes_summary$n.starts)
bikes_summary$n.ends <- ifelse(is.na(bikes_summary$n.ends ), 0, bikes_summary$n.ends )
bikes_summary$n.from.van <- ifelse(is.na(bikes_summary$n.from.van), 0, bikes_summary$n.from.van)
bikes_summary$n.to.van <- ifelse(is.na(bikes_summary$n.to.van), 0, bikes_summary$n.to.van)

bikes_summary$balance <- bikes_summary$n.ends - 
  bikes_summary$n.to.van - 
  bikes_summary$n.starts +
  bikes_summary$n.from.van

bikes_summary$balance <- ifelse(bikes_summary$day == 0, bikes_summary$n, bikes_summary$balance)

bikes_summary <- bikes_summary %>% 
  group_by(Station) %>% 
  mutate(n = cumsum(balance))

stations_info <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Capital_Bike_Share_Locations.csv")


bikes_summary <- bikes_summary %>% 
  left_join(stations_info %>% 
              select(NAME, CAPACITY),
              by = c("Station" = "NAME"))

write.csv(bikes_summary, 
          "/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/trips_summary.csv")


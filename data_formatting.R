#Date of creation: 11/04/2021
# Bikeshare Project 
#Data cleansing and managing
#This scripts creates a data frame, potentially for each month, that has the number of bikes 
#that start and end trips either by people or delivered by the bikshare platform vans.

library(dplyr)
library(dtplyr)
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



bikes_vans_result <- lapply(list(bikes_vans), fill_trip_data)[[1]]

bikes_june19 <- bikes_june19 %>%
  bind_rows(bikes_vans_result)


bikes_summary <- bikes_june19 %>% 
  filter(Start.station.number !=0 & End.station.number !=0) %>% 
  group_by(Station = Start.station.number,  month, day, hour) %>% 
  summarise(n.starts = n()) %>% 
  left_join(bikes_june19 %>% 
              filter(End.station.number !=0 & Start.station.number !=0) %>% 
              group_by(Station = End.station.number,  month, day, hour) %>% 
              summarise(n.ends = n()),
            by = c("Station", "month", "day", "hour"))

bikes_van_summary <- bikes_june19 %>% 
  filter(Start.station.number !=0 & End.station.number ==0) %>% 
  group_by(Station = Start.station.number,  month, day, hour) %>% 
  summarise(n.to.van = n()) %>% 
  left_join(bikes_june19 %>% 
              filter(End.station.number !=0 & Start.station.number ==0) %>% 
              group_by(Station = End.station.number,  month, day, hour) %>% 
              summarise(n.from.van = n()),
            by = c("Station", "month", "day", "hour"))

bikes_summary <- bikes_summary %>% 
  bind_rows(bikes_van_summary)

write.csv(bikes_summary, 
          "/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/trips_summary.csv")

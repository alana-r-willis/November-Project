#Date of creation: 11/04/2021
# Bikeshare Project 
#Starting point for june 2019
#this script creates a data set that contains the total amount of bikes that
#were at each station at the end of may 2019

library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)
library(purrr)

bikes_may19 <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/201905-capitalbikeshare-tripdata.csv")

#the following lines of code do the following:
#arrange by end date and bike number (this is not needed but works in case anyone wants to make sure)
#creates the number of trip for each bike during the month
#sets the total number of trips for each bikes so it can filter out the last known position 
bikes_last_position_may <- bikes_may19 %>% 
  arrange(End.date, Bike.number) %>% 
  group_by(Bike.number) %>% 
  mutate(obs = 1:n()) %>% 
  group_by(Bike.number) %>% 
  mutate(max_obs = max(obs)) %>% 
  filter(max_obs ==obs)

#this part only takes the last data set and counts the number of bikes at each station
#at the end of the month based on their last known position
bikes_endofmay <- bikes_last_position_may %>% 
  group_by(Station = End.station.number) %>% 
  tally()

bikes_endofmay$month <- 6
bikes_endofmay$day <- 0
bikes_endofmay$hour <- 0



write.csv(bikes_endofmay,"/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/bikes_end_of_may.csv")

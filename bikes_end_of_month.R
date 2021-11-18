#Date of creation: 11/04/2021
# Bikeshare Project 
#Starting point for june 2019
#this script creates a data set that contains the total amount of bikes that
#were at each station at the end of may 2019

bikes_end_of_month <- function(month_data, save = F){
  
  library(dplyr)
  library(lubridate)
  library(tidyverse)
  
  #the following lines of code do the following:
  #arrange by end date and bike number (this is not needed but works in case anyone wants to make sure)
  #creates the number of trip for each bike during the month
  #sets the total number of trips for each bikes so it can filter out the last known position 
  bikes_last_position <- month_data %>% 
    arrange(End.date, Bike.number) %>% 
    group_by(Bike.number) %>% 
    mutate(obs = 1:n()) %>% 
    group_by(Bike.number) %>% 
    mutate(max_obs = max(obs)) %>% 
    filter(max_obs ==obs)
  
  #this part only takes the last data set and counts the number of bikes at each station
  #at the end of the month based on their last known position
  bikes_endofmonth <- bikes_last_position %>% 
    group_by(Station = End.station.number) %>% 
    tally()
  
  month <- month(as.Date(month_data$Start.date[1]))
  year <- year(as.Date(month_data$Start.date[1]))
  
  bikes_endofmonth$month <- month +1 
  bikes_endofmonth$day <- 0
  bikes_endofmonth$hour <- 0
  
<<<<<<< HEAD
  names(bikes_endofmonth)[names(bikes_endofmonth) == "n"] <- "availability"
  
=======
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
  if(save == T){
  filename <- paste0("bikes_end_of_", month.abb[month], year, ".csv")
  write.csv(bikes_endofmonth,
            paste0("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/", filename))}
<<<<<<< HEAD
  return(bikes_endofmonth)
=======
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
  
}

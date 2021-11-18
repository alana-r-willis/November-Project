

library(dplyr)
library(data.table)
library(lubridate)
library(tidyverse)
library(purrr)
<<<<<<< HEAD
library(janitor)
library(stringi)
library(stringr)
library(tidygeocoder)


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

i = 1
#load data from stations
stations_info <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Capital_Bike_Share_Locations.csv")

stations_info <- stations_info %>% 
  reverse_geocode(lat = LATITUDE,
                  long = LONGITUDE,
                  method = 'osm',
                  address = address_found,
                  full_results = TRUE) 

write.csv(stations_info[,1:44],"/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Capital_Bike_Share_Locations_withzip.csv",
                           row.names = F)
#select the desired months and years to be looped
for(year in c(2018:2019)){
  for(month in c(1:12)){
    
    print(paste0(year, "-", month))
    
    #download directly from the aws server
    #some files need a different treatment to be loaded
=======

i = 1

stations_info <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Capital_Bike_Share_Locations.csv")
for(year in c(2018:2019)){
  for(month in c(1:12)){
    
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    temp <- tempfile()
    download.file(paste0("https://s3.amazonaws.com/capitalbikeshare-data/",
                         year,
                         ifelse(nchar(month) == 1, paste0(0,month), month) ,
                         "-capitalbikeshare-tripdata.zip"),temp)
    if(year == 2019 & month == 7){
      data <- fread(unz(temp, paste0(year,
                                     ifelse(nchar(month) == 1, paste0(0,month), month) ,
                                     "-capitalbikeshare-tripdata")))
    }
<<<<<<< HEAD
    else if(year == 2018 & month == 1){   
      data <- read_csv(unz(temp, paste0(year,
                                        ifelse(nchar(month) == 1, paste0(0,month), month) ,
                                        "_capitalbikeshare_tripdata.csv")))}
    else{
      data <- read_csv(unz(temp, paste0(year,
                                        ifelse(nchar(month) == 1, paste0(0,month), month) ,
                                        "-capitalbikeshare-tripdata.csv")))}
    
    unlink(temp)
    
    names(data) <- str_replace_all(names(data), " ", ".")
    
    #for the first observation, we need to create a starting point, which helps determine
    #how many bikes are at each station at the start of the next month
    if(i == 1){
      starting_point <- bikes_end_of_month(data)
      i = i+1
      next
    }
    
=======
    data <- read_csv(unz(temp, paste0(year,
                                   ifelse(nchar(month) == 1, paste0(0,month), month) ,
                                   "-capitalbikeshare-tripdata.csv")))
    unlink(temp)
    
    if(i == 1){
      starting_point <- bikes_end_of_month(data)
      next
    }
    
    
    #Import data
    # data <- read.csv("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/201906-capitalbikeshare-tripdata.csv")
    
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    #Create standard format time information
    data$hour <- hour(data$Start.date)
    data$weekday <- weekdays(as.Date(data$Start.date))
    data$month <- month(data$Start.date)
    data$day <-  day(data$Start.date)
    data$Start.date <- ymd_hms(data$Start.date)
    data$End.date <- ymd_hms(data$End.date)
    
    #Create a column that tells if the bike starts at the same station it ended
    data <- data %>% 
      arrange(Bike.number, Start.date) %>% 
      group_by(Bike.number) %>% 
      mutate(Previous.end.station.number = lag(End.station.number),
             Previous.end.station = lag(End.station)) %>% 
      group_by(Bike.number) %>% 
      mutate(start.equals.end = ifelse(Start.station.number == Previous.end.station.number,1,0))
    
    #extract the rows that don't match starting station with where the bikes ended its last trip
    bikes_vans <- data %>% 
      filter(start.equals.end == 0)
    
    
<<<<<<< HEAD
    #the function that fills the vans reshuffling is applied for the whole list of "missing trips"
=======
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
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    bikes_vans_result <- lapply(list(bikes_vans), fill_trip_data)[[1]]
    
    #the missing trips are added to the original data
    data <- data %>%
      bind_rows(bikes_vans_result)
    
    
<<<<<<< HEAD
    #the number of trips ending and starting at each station for month, day and hour is calculated
    bikes_summary <- data %>% 
      filter(Start.station.number !=0 & End.station.number !=0) %>% 
      group_by(Station = Start.station.number, Station.name = Start.station, month, day, hour, Member.type.starts = Member.type) %>% 
      summarise(n.starts = n()) %>% 
      pivot_wider(names_from = Member.type.starts, values_from = n.starts) %>% 
      rename(Member.starts = Member, Casual.starts = Casual) %>% 
      full_join(data %>% 
                  filter(End.station.number !=0 & Start.station.number !=0) %>% 
                  group_by(Station = End.station.number, Station.name = End.station,  month, day, hour, Member.type.ends = Member.type) %>% 
                  summarise(n.ends = n())%>% 
                  pivot_wider(names_from = Member.type.ends, values_from = n.ends)%>% 
                  rename(Member.ends = Member, Casual.ends = Casual) ,
                by = c("Station","Station.name", "month", "day", "hour")) %>% 
      replace_na(list(Casual.starts = 0,
                      Casual.ends = 0,
                      Member.starts = 0,
                      Member.ends = 0)) %>% 
      mutate(n.starts = Casual.starts + Member.starts,
             n.ends = Casual.ends+ Member.ends)
=======
    #the number of trips ending and starting at each station for month, day and houris calculated
    bikes_summary <- data %>% 
      filter(Start.station.number !=0 & End.station.number !=0) %>% 
      group_by(Station = Start.station.number, Station.name = Start.station, month, day, hour) %>% 
      summarise(n.starts = n()) %>% 
      full_join(data %>% 
                  filter(End.station.number !=0 & Start.station.number !=0) %>% 
                  group_by(Station = End.station.number, Station.name = End.station,  month, day, hour) %>% 
                  summarise(n.ends = n()),
                by = c("Station","Station.name", "month", "day", "hour"))
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    
    #the number of bikes taken by van at each station for month, day and houris calculated
    bikes_van_summary <- data %>% 
      filter(Start.station.number !=0 & End.station.number ==0) %>% 
      group_by(Station = Start.station.number, Station.name = Start.station,  month, day, hour) %>% 
      summarise(n.to.van = n()) %>% 
      full_join(data %>% 
                  filter(End.station.number !=0 & Start.station.number ==0) %>% 
                  group_by(Station = End.station.number, Station.name = End.station, month, day, hour) %>% 
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
    
    # write.csv(bikes_summary, 
    #           "/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/trips_summary.csv")
    
    
    bikes_end_of_prev <- starting_point
    
    bikes_summary <- bikes_summary %>% 
      bind_rows(bikes_end_of_prev)
    
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
    
<<<<<<< HEAD
    bikes_summary$balance <- ifelse(bikes_summary$day == 0, bikes_summary$availability, bikes_summary$balance)
    
    bikes_summary <- bikes_summary %>% 
      group_by(Station) %>% 
      mutate(availability = cumsum(balance))
=======
    bikes_summary$balance <- ifelse(bikes_summary$day == 0, bikes_summary$n, bikes_summary$balance)
    
    bikes_summary <- bikes_summary %>% 
      group_by(Station) %>% 
      mutate(n = cumsum(balance))
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    
    
    bikes_summary <- bikes_summary %>% 
      left_join(stations_info %>% 
<<<<<<< HEAD
                  select(NAME, capacity = CAPACITY, zip = postcode),
=======
                  select(NAME, CAPACITY),
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
                by = c("Station.name" = "NAME"))
    
    bikes_summary <-  bikes_summary %>% 
      filter(day !=0)
    
    bikes_summary$weekday <- weekdays(as.Date(paste0("2019/", bikes_summary$month, "/", bikes_summary$day)))
    
<<<<<<< HEAD
    # names(bikes_summary)[names(bikes_summary) == "n"] <- "availability"
=======
    names(bikes_summary)[names(bikes_summary) == "n"] <- "availability"
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    
    bikes_last <- bikes_summary %>% 
      group_by(Station) %>% 
      summarise(last(availability))
    
    
    write.csv(bikes_summary, 
<<<<<<< HEAD
              paste0("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Processed/All/trips_summary",
                     month,
                     year,
                     ".csv"))
    bikes_summary %>% 
      filter(activity_clasif !=1) %>%
      mutate(availability = ifelse(availability <0, 0, availability)) %>% 
      write.csv(paste0("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Processed/Filtered/trips_summary_filtered",
                       month, 
                       year, 
                       ".csv"),
=======
              paste0("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Processed/All/",
                     month,
                     year,
                     "trips_summary.csv"))
    bikes_summary %>% 
      filter(activity_clasif !=1) %>%
      mutate(availability = ifelse(availability <0, 0, availability)) %>% 
      write.csv(paste0("/Users/Stefano_1/Documents/CMU/Perspectives/Project/Data/Processed/Filtered/",
                       month, 
                       year, 
                       "trips_summary_filtered.csv"),
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
                row.names = F)
    
    starting_point <- bikes_summary %>% 
      group_by(Station) %>% 
      mutate(obs = 1:n()) %>% 
      group_by(Station) %>% 
      mutate(max = max(obs)) %>% 
      filter(obs == max) %>% 
      mutate(month = month + 1,
             day = 0,
             hour = 0) %>% 
<<<<<<< HEAD
      select(month, day, hour, availability, capacity) %>% 
      mutate(capacity = ifelse(is.na(capacity), median(stations_info$capacity, na.rm = T), capacity)) %>% 
      mutate(availability = ifelse(availability > capacity, capacity,
                                   ifelse(availability < 0, 0, availability))) %>% 
      select(-capacity)
=======
      select(month, day, hour, balance)
>>>>>>> f8af3be77fbcbdb1c50c8c42e4dcb9d8f9ed18cb
    
    i = i+1
  }
}


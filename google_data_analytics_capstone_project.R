#loading library
library("tidyverse")
library("lubridate")
library("hms")
library("hydroTSM")
library("data.table")
#set the main folder as working file
setwd("D:/Others/Google Data Analytics Certification/Project/Dataset/google_capstone_project")

#creating data frame for each month
jan01_df <- read_csv("202201-divvy-tripdata.csv") 
feb02_df <- read_csv("202202-divvy-tripdata.csv") 
mar03_df <- read_csv("202203-divvy-tripdata.csv")
apr04_df <- read_csv("202204-divvy-tripdata.csv")
may05_df <- read_csv("202205-divvy-tripdata.csv") 
jun06_df <- read_csv("202206-divvy-tripdata.csv") 
jul07_df <- read_csv("202207-divvy-tripdata.csv")
aug08_df <- read_csv("202208-divvy-tripdata.csv") 
sep09_df <- read_csv("202209-divvy-publictripdata.csv") 
oct10_df <- read_csv("202210-divvy-tripdata.csv")
nov11_df <- read_csv("202211-divvy-tripdata.csv") 
dec12_df <- read_csv("202212-divvy-tripdata.csv")

#combing all months cyclistic data
cyclistic_all_months <- rbind(jan01_df,feb02_df,mar03_df,apr04_df,
                              may05_df,jun06_df,jul07_df,aug08_df,
                              sep09_df,oct10_df,nov11_df,dec12_df)

#removing seperate datasets 
remove(jan01_df,feb02_df,mar03_df,apr04_df,
       may05_df,jun06_df,jul07_df,aug08_df,
       sep09_df,oct10_df,nov11_df,dec12_df)

#checking dataset
str(cyclistic_all_months)
head(cyclistic_all_months)
colnames(cyclistic_all_months)
glimpse(cyclistic_all_months)
summary(cyclistic_all_months)

#removing ride length equal to zero or less than zero
cyclistic_all_months<- cyclistic_all_months[!(cyclistic_all_months$ride_length <=0),]
#making new columns
#Converting STARTED AT & END AT INTO POSIXct
cyclistic_all_months$started_at <- format(as.POSIXct(cyclistic_all_months$started_at,format="%m/%d/%Y %H:%M"))
cyclistic_all_months$ended_at <- format(as.POSIXct(cyclistic_all_months$ended_at,format="%m/%d/%Y %H:%M"))
#day name
cyclistic_all_months$day_name <- format(as.Date(cyclistic_all_months$date), "%A")
#date 
cyclistic_all_months$date <- as.Date(cyclistic_all_months$started_at)
#month date
cyclistic_all_months$month_date <- format(as.Date(cyclistic_all_months$date), "%d")
#month
cyclistic_all_months$month_name <-month(ymd(cyclistic_all_months$date), label = TRUE, abbr = FALSE)
#other way
cyclistic_all_months$month_name <- format(as.Date(cyclistic_all_months$date), "%m")
#time
cyclistic_all_months$started_time <-as_hms(as.POSIXct(cyclistic_all_months$started_at))
cyclistic_all_months$ended_time <-as_hms(as.POSIXct(cyclistic_all_months$ended_at))
#hour
cyclistic_all_months$start_hour <- (hour(cyclistic_all_months$started_at))
cyclistic_all_months$end_hour <- (hour(cyclistic_all_months$ended_at))
#seasons 
cyclistic_all_months$seasons <- time2season(cyclistic_all_months$date,out.fmt = "seasons")


#creating csv excel file
fwrite(cyclistic_all_months,"cyclistic_data_combine.csv")

#Now time for some calculations
#mean for ride length
mean(cyclistic_all_months$ride_length)
#max for ride length
max(cyclistic_all_months$ride_length)
#min for ride length
min(cyclistic_all_months$ride_length)
#median for ride length
median(cyclistic_all_months$ride_length)

#total
#total count for membertype(casual,member)
cyclistic_all_months %>%
  group_by(member_casual) %>%
  summarize(total_count= n())
#total count for ridership type(classic,docked and electric bike)
cyclistic_all_months %>%
  group_by(rideable_type) %>%
  summarize(total_count= n())
#total count for member type and ridership type
cyclistic_all_months %>%
  group_by(member_casual,rideable_type) %>%
  summarize(total_count= n())

#average
#average ride length for member type
cyclistic_all_months %>%
  group_by(member_casual) %>%
  summarize(ride_length_avg = mean(ride_length))
#Other way
aggregate(cyclistic_all_months$ride_length ~ cyclistic_all_months$member_casual, FUN = mean)
#average ride length for ridership type
cyclistic_all_months %>%
  group_by(rideable_type) %>%
  summarize(ride_length_avg = mean(ride_length))
#average ride length for member type and ridership type
cyclistic_all_months %>%
  group_by(member_casual,rideable_type) %>%
  summarize(ride_length_avg = mean(ride_length))

#day
#average ride length for member type by day 
cyclistic_all_months %>%
  group_by(member_casual,day_name) %>%
  summarize(ride_length_avg = mean(ride_length))
#other way
aggregate(cyclistic_all_months$ride_length ~ cyclistic_all_months$member_casual + cyclistic_all_months$day_name, FUN = mean)
#total no of rides for member type by day 
cyclistic_all_months %>%
  group_by(member_casual,day_name) %>%
  summarize(no_of_rides= n())
#average ride length for ridership type  by day 
cyclistic_all_months %>%
  group_by(rideable_type,day_name) %>%
  summarize(ride_length_avg = mean(ride_length)) %>% 
  print(n = 21)
#total no of rides by for ridership type type by day 
cyclistic_all_months %>%
  group_by(rideable_type,day_name) %>%
  summarize(no_of_rides= n()) %>% 
  print(n = 21)
  
#month
#average ride length for member type by month 
cyclistic_all_months %>%
  group_by(member_casual,month_name) %>%
  summarize(ride_length_avg = mean(ride_length))
#total no of rides for member type by month 
cyclistic_all_months %>%
  group_by(member_casual,month_name) %>%
  summarize(no_of_rides= n()) %>% 
  print(n = 24)
#other way
cyclistic_all_months %>%
  group_by(member_casual) %>% 
  count(month_name) %>% 
  print(n = 24)
#average ride length for ridership type by month 
cyclistic_all_months %>%
  group_by(rideable_type,month_name) %>%
  summarize(ride_length_avg = mean(ride_length)) %>% 
  print(n = 36)
#total no of rides  for ridership type by month
cyclistic_all_months %>%
  group_by(rideable_type,month_name) %>%
  summarize(no_of_rides= n()) %>% 
  print(n = 36)

#month date
#average ride length for member type by month date 
cyclistic_all_months %>%
  group_by(member_casual,month_date) %>%
  summarize(ride_length_avg = mean(ride_length)) %>% 
  print(n = 62)
#total no of rides of member type by month date
cyclistic_all_months %>%
  group_by(member_casual,month_date) %>%
  summarize(no_of_rides= n()) %>% 
  print(n = 62)
#average ride length for ridership type by month date
cyclistic_all_months %>%
  group_by(rideable_type,month_date) %>%
  summarize(ride_length_avg = mean(ride_length)) %>% 
  print(n = 93)
#total no of rides by for ridership type type by month date
cyclistic_all_months %>%
  group_by(rideable_type,month_date) %>%
  summarize(no_of_rides= n()) %>% 
  print(n = 93)

#hour
#average ride length for member type by hour 
cyclistic_all_months %>%
  group_by(member_casual,start_hour) %>%
  summarize(ride_length_avg = mean(ride_length)) %>% 
  print(n = 48)
#total no of rides of member type by hour 
cyclistic_all_months %>%
  group_by(member_casual,start_hour) %>%
  summarize(no_of_rides= n()) %>%

#seasons
#average ride length for member type by seasons
cyclistic_all_months %>%
  group_by(member_casual,rideable_type,seasons) %>% 
  summarize(ride_length_avg = mean(ride_length))
#total no of rides for member type by seasons
cyclistic_all_months %>%
  group_by(member_casual,seasons) %>%
  summarize(no_of_rides= n())
#average ride length for ridership type by seasons
cyclistic_all_months %>%
  group_by(rideable_type,seasons) %>%
  summarize(ride_length_avg = mean(ride_length)) %>% 
  print(n = 12)
#total no of rides by for ridership type type by seasons
cyclistic_all_months %>%
  group_by(rideable_type,seasons) %>%
  summarize(no_of_rides= n()) %>% 
  print(n = 12)
#<---------------------------------------------------------THE END-------------------------------------------------------------------->
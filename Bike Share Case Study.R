# load the needed library 
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)

# Read in all the CSV files
tripdata_202201 <- read.csv("Desktop/2022_datasets/202201-tripdata.csv")
tripdata_202202 <- read.csv("Desktop/2022_datasets/202202-tripdata.csv")
tripdata_202203 <- read.csv("Desktop/2022_datasets/202203-tripdata.csv")
tripdata_202204 <- read.csv("Desktop/2022_datasets/202204-tripdata.csv")
tripdata_202205 <- read.csv("Desktop/2022_datasets/202205-tripdata.csv")
tripdata_202206 <- read.csv("Desktop/2022_datasets/202206-tripdata.csv")
tripdata_202207 <- read.csv("Desktop/2022_datasets/202207-tripdata.csv")
tripdata_202208 <- read.csv("Desktop/2022_datasets/202208-tripdata.csv")
tripdata_202209 <- read.csv("Desktop/2022_datasets/202209-tripdata.csv")
tripdata_202210 <- read.csv("Desktop/2022_datasets/202210-tripdata.csv")
tripdata_202211 <- read.csv("Desktop/2022_datasets/202211-tripdata.csv")
tripdata_202212 <- read.csv("Desktop/2022_datasets/202212-tripdata.csv")

# Inspect the dataframes and look for incongruencies
str(tripdata_202201)
str(tripdata_202202)
str(tripdata_202203)
str(tripdata_202204)
str(tripdata_202205)
str(tripdata_202206)
str(tripdata_202207)
str(tripdata_202208)
str(tripdata_202209)
str(tripdata_202210)
str(tripdata_202211)
str(tripdata_202212)
# All datasets have identical column names and formatting types 
# and nothing needs to be transformed

# Combine all the individual dataframes into one 
tripdata_2022 <- rbind(tripdata_202201, tripdata_202202, tripdata_202203, tripdata_202204,
                       tripdata_202205, tripdata_202206, tripdata_202207, tripdata_202208, 
                       tripdata_202209, tripdata_202210, tripdata_202211, tripdata_202212)

# Calculate the total number of rows for the individual dataframes
rowtotal <- sum(
  nrow(tripdata_202201),
  nrow(tripdata_202202), 
  nrow(tripdata_202203), 
  nrow(tripdata_202204), 
  nrow(tripdata_202205), 
  nrow(tripdata_202206), 
  nrow(tripdata_202207), 
  nrow(tripdata_202208),
  nrow(tripdata_202209), 
  nrow(tripdata_202210),
  nrow(tripdata_202211), 
  nrow(tripdata_202212))

print(rowtotal)

# To confirm the total number of rows for the combined dataframe
print(nrow(tripdata_2022))

# To remove duplicates
tripdata_2022 <- tripdata_2022[!duplicated(tripdata_2022$ride_id), ]

# Remove NA records
tripdata_2022 <- drop_na(tripdata_2022)

# Clean and remove any spaces and parentheses
tripdata_2022 <- clean_names(tripdata_2022)

# Format the date and time to individual column and add one column for day of week
tripdata_2022$date <- as.Date(tripdata_2022$started_at)
tripdata_2022$month <- format(as.Date(tripdata_2022$date), "%b")
tripdata_2022$day <- format(as.Date(tripdata_2022$date), "%d")
tripdata_2022$year <- format(as.Date(tripdata_2022$date), "%Y")
tripdata_2022$day_of_week <- format(as.Date(tripdata_2022$date), "%A")

## Add a column to determine the length of ride
tripdata_2022_v2 <- mutate(tripdata_2022, ride_length = difftime(ended_at, started_at, units = "mins"))
str(tripdata_2022)

## Filter out trips with a ride length less than 0
tripdata_2022_v3 <- tripdata_2022_v2[!tripdata_2022_v2$ride_length <0,]
glimpse(tripdata_2022_v3)

# Get the average,mean,standard deviation,median, 
# minimum and maximum ride length for casual riders and annual members
trip_stats <- tripdata_2022_v3 %>% 
  group_by(member_casual) %>% 
  summarise(average_ride_length = mean(ride_length), standard_deviation = sd(ride_length), 
            median_ride_length = median(ride_length), min_ride_length = min(ride_length), 
            max_ride_length = max(ride_length))
head(trip_stats)

# Fix the order of days of week 
tripdata_2022_v3$day_of_week <- ordered(tripdata_2022_v3$day_of_week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Determine the average ride time by each day for members and casual users
tripdata_2022_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(rider_type_total = n(), average_ride_length = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

# Fix the order of months 
tripdata_2022_v3$month <- ordered(tripdata_2022_v3$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Determine the number of rides each months for members and casual users
popular_month_each_user <- tripdata_2022_v3 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)

View(popular_month_each_user)

## Determine the most popular months during 2022
popular_month <- tripdata_2022_v3 %>% 
  group_by(month) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(-number_of_rides)

View(popular_month)

## Determine the most popular start station for members
popular_start_stations_member <- tripdata_2022_v3 %>% 
  filter(member_casual == 'member') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_member)

## Determine the most popular start station for casual riders
popular_start_stations_casual <- tripdata_2022_v3 %>% 
  filter(member_casual == 'casual') %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_starts = n()) %>% 
  filter(start_station_name != "") %>% 
  arrange(- number_of_starts)

head(popular_start_stations_casual)

# Save the clean data frame as CSV so it can be uploaded to tableau for additional visualisation 
tripdata_2022_v3 %>%
  write.csv("tripdata_2022_updated.csv")

# Share Phase
# Total rides count
tripdata_2022_v3 %>% 
  group_by(member_casual) %>% 
  summarise(total_rider_type = n()) %>% 
  ggplot(aes(x = member_casual, y = total_rider_type, fill = member_casual)) + 
  geom_col(position = "dodge") +  geom_text(aes(label=total_rider_type,vjust=-0.25)) +
  labs(x = "member_casual", y = "Ride Count ", title=("Total number of rides"))

## To get the ride count and average trip duration for days
popular_day_each_rideable_type <- tripdata_2022_v3 %>% 
  group_by(member_casual, day_of_week, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)

## Total daily rides for members and casual riders
ggplot(popular_day_each_rideable_type, aes(x = day_of_week, y=number_of_rides, fill=rideable_type)) +
  geom_col(position = "dodge",width= 0.9) +
  labs(x = "Day", y = "Number Of Rides", title=("Number of Daily Rides for Members and Casual Riders")) +
  facet_wrap(~member_casual)

## To get the average ride length by day
avg_ride_length_by_day <- tripdata_2022_v3 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(average_ride_length_by_day = mean(ride_length))

## Average daily ride length 
ggplot(avg_ride_length_by_day, aes(x = day_of_week, y=average_ride_length_by_day, fill=member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(limits = c(0, 26), breaks = seq(0, 26,5)) +
  labs(x = "Day", y = "Average Ride Length", title=("Average Ride Length By Day"))

## Total ride count in months 
ggplot(popular_month_each_user, aes(x = month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge",width= 0.8) +
  labs(x = "Month", y = "Number Of Rides", title=("Number of Monthly Rides for Members and Casual Riders")) +
  facet_wrap(~member_casual)

## To get the ride count and average trip duration for months
popular_month_each_rideable_type <- tripdata_2022_v3 %>% 
  group_by(member_casual, month, rideable_type) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month)

## Total monthly rides for members and casual riders for each bike types
ggplot(popular_month_each_rideable_type, aes(x = month, y=number_of_rides, fill=member_casual)) +
  geom_col(position = "dodge",width= 0.8) +
  labs(x = "Day", y = "Number Of Rides", title=("Number of Monthly Rides for Each Type of Bikes")) +
  facet_wrap(~rideable_type)

# Average trip duration in days
ggplot(avg_ride_length_by_day, aes(x = day_of_week, y=average_ride_length_by_day, group=member_casual,colour=member_casual)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 26), breaks = seq(0, 26,5)) +
  labs(x = "Day", y = "Average Ride Length", title=("Average Daily Trip Duration for Members and Casual Riders"))

# Average trip duration per months
ggplot(avg_ride_length_by_month, aes(x = month, y=average_ride_length_by_month, group=member_casual,colour=member_casual)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 26), breaks = seq(0, 26,5)) +
  labs(x = "Month", y = "Average Ride Length", title=("Average Monthly Trip Duration for Members and Casual Riders"))




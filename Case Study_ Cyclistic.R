## first load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(conflicted)

## read the datasets in variable names 

q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

## look at the colunms of the datasets

head(q1_2019)
colnames(q1_2019)
head(q1_2020)
colnames(q1_2020)
colnames(q1_2019)

## match the colunm names with both datasets

q1_2019 <- rename(q1_2019, 
                  ride_id = trip_id, started_at = start_time, ended_at = end_time,
                  start_station_name = from_station_name, end_station_name = to_station_name,
                  member_casual = usertype, start_station_id = from_station_id, 
                  end_station_id = to_station_id, rideable_type = bikeid)

## check if they are matched

colnames(q1_2019)
colnames(q1_2020)

## inspect dataset and look for inconsistence between the data

str(q1_2019)
str(q1_2020)

## have to match data types between datasets

q1_2019 <- q1_2019 %>% 
  mutate(ride_id = as.character(ride_id), 
         rideable_type = as.character(rideable_type) )

## combine datasets

all_trips <- bind_rows(q1_2019, q1_2020)

## remove colunms that aren't present in both

all_trips <- all_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng, gender, tripduration, birthyear))

## check if removed and the new table

colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summarise(all_trips)

## data cleaning

## memer_casual there should only be two types of answers not four, suscriber will
## be member and customer will be casual
all_trips <- all_trips %>% 
  mutate(member_casual = recode(member_casual,"Subscriber" = "member", "Customer" = "casual"))

## using table function to check number of each category
table(all_trips$member_casual)


##want to add columns for the indivdual trips grouped into weeks, months and years

all_trips$date <- as.Date(all_trips$started_at)

all_trips$month <- format(as.Date(all_trips$date), "%m")

all_trips$day <- format(as.Date(all_trips$date), "%d")

all_trips$year <- format(as.Date(all_trips$date), "%Y")

all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")



## make calculation for ride_length (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)

View(all_trips)
str(all_trips)

## check if factor
is.factor(all_trips$ride_length)
##check if num
is.numeric((all_trips$ride_length))
##check what class
class(all_trips$ride_length)
##since its difftime convert to num
all_trips$ride_length <- as.numeric(all_trips$ride_length)
class(all_trips$ride_length)
## now it is numeric

##remove Bad data, when start_station_name is HQ QR and where ride_length < 0 is bad data
all_trips_v2 = all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0), ]

## analysis on data
mean(all_trips_v2$ride_length) # average
median(all_trips_v2$ride_length) # midpoint 
min(all_trips_v2$ride_length) # minimum length
max(all_trips_v2$ride_length) # maximum length
View(all_trips_v2)


## use aggregate to cacluate functions based on members and casuals
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

## now the same process but including the specific day
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

## to make the results show the correct order of the day

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday",
                                                                       "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

## rerun the previous code
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# analyze rdata by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>% #creates weekday field using wday()
group_by(member_casual, weekday) %>% #groups by usertype and weekday
  summarise(number_of_rides = n(), #calculatesthe number of rides and average duration
  average_duration = mean(ride_length)) %>% #calculates the average duration
  arrange(member_casual, weekday)

## Visuals
## number rides by weekday and userType
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), .groups = 'drop', average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle("Number of Rides by Weekday and User Type") +
  labs(subtitle = "Comparing Casual Riders and Members",
       y = "Number of Rides",
       x = "Day of the Week",
       fill = "User Type") +
  scale_y_continuous(labels = scales::comma) # Format y-axis labels


## chart for average
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), 
            average_duration = mean(ride_length), .groups = 'drop') %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by Weekday and User Type",
       subtitle = "Comparing Casual Riders and Members",
       y = "Average Duration (Seconds)",
       x = "Day of the Week",
       fill = "User Type")


# Extract hour from the started_at column
all_trips_v2 <- all_trips_v2 %>%
  mutate(hour = hour(as.POSIXct(started_at)))

# bike usage by hour of the day
all_trips_v2 %>%
  group_by(member_casual, hour) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = hour, y = number_of_rides, color = member_casual)) +
  geom_line() +
  labs(title = "Bike Usage by Hour of the Day", x = "Hour", y = "Number of Rides") +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +  # Specify breaks for each hour
  scale_x_continuous(labels = function(x) sprintf("%02d:00", x))  # Format labels as HH:00



# bike usage: weekday vs weekend

all_trips_v2 %>%
  mutate(weekday_vs_weekend = if_else(day_of_week %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%
  group_by(member_casual, weekday_vs_weekend) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = weekday_vs_weekend, y = number_of_rides, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bike Usage: Weekday vs. Weekend", x = "", y = "Number of Rides") +
  scale_y_continuous(labels = scales::comma) # This line changes the y-axis labels




# Descriptive statistics for ride lengths by member type
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(
    average_ride_length = mean(ride_length, na.rm = TRUE),
    median_ride_length = median(ride_length, na.rm = TRUE),
    sd_ride_length = sd(ride_length, na.rm = TRUE),
    .groups = 'drop'
  )

View(all_trips_v2)

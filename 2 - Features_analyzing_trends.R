# Loading the filtered training data

path <- './data/curated_train.csv'
taxi_data <- read.csv(path)

# Load the libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# Let's extract the time based features

taxi_data$pickup_month <- strftime(taxi_data$pickup_datetime,'%m')
taxi_data$pickup_day <- strftime(taxi_data$pickup_datetime,'%d')
taxi_data$pickup_hour <- strftime(taxi_data$pickup_datetime, "%H", tz='EST')

# Checking the distribution of the Trip Duration Is different per month, day or hour of the day

ggplot(
  data = taxi_data,
  aes(x = trip_duration, y = pickup_month)
) + geom_boxplot()

taxi_data %>%
  group_by(pickup_month) %>%
  summarize(mean_trip = mean(trip_duration))

# Analysing by Day

ggplot(
  data = taxi_data,
  aes(x = trip_duration, y = pickup_day)
) + geom_boxplot()


mean_day <- taxi_data %>%
  group_by(pickup_day) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_day,
  aes(x=as.integer(pickup_day), y=mean_trip)
) + geom_point()

# Analysing by Hour

ggplot(
  data = taxi_data,
  aes(x = trip_duration, y = pickup_hour)
) + geom_boxplot()

mean_hour <- taxi_data %>%
  group_by(pickup_hour) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_hour,
  aes(x=as.integer(pickup_hour), y=mean_trip)
) + geom_point()

# Is there a difference in the mean between passenger count?

mean_passenger <- taxi_data %>%
  group_by(passenger_count) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_passenger,
  aes(x=as.integer(passenger_count), y=mean_trip)
) + geom_point()

# Influence is minimal, Confirming with a correlation coefficient

cor(
  taxi_data$passenger_count,
  taxi_data$trip_duration
)

# Is there a difference by vendor ID?

mean_vendor <- taxi_data %>%
  group_by(vendor_id) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_vendor,
  aes(x=as.integer(vendor_id), y=mean_trip)
) + geom_point()

# Let's now compute a Euclidean and Manhattan distance between the coordinates 
# this will give us the expected distance between the two points

manhattan_dist <- function(p_lat, p_long, d_lat, d_long){
  
  point_a <- c(p_lat, p_long)
  point_b <- c(d_lat, d_long)
  
  distance <- sum(abs(point_a-point_b))
  return(distance)
}


manhattan_dist(40,50,30,50)

# Apply this function to every value of the dataframe

taxi_data <- cbind(taxi_data, manhattan_distance = mapply(
  manhattan_dist, 
  taxi_data$pickup_longitude, 
  taxi_data$pickup_latitude,
  taxi_data$dropoff_longitude,
  taxi_data$dropoff_latitude
  ))

# Test the first row: 
manhattan_dist(-73.98215,	40.76794, -73.96463, 40.76560)

taxi_data$manhattan_dist <- round(
  taxi_data$manhattan_dist,
  2
)

mean_distance <- taxi_data %>%
  group_by(manhattan_dist) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_distance,
  aes(x=manhattan_dist, y=mean_trip)
) + geom_point()

# Check the correlation
cor(
  taxi_data$manhattan_dist,
  taxi_data$trip_duration
)

# a 0.69 correlation!
# Will the euclidean distance be a bit better?

euclidean_dist <- function(p_lat, p_long, d_lat, d_long){
  
  point_a <- c(p_lat, p_long)
  point_b <- c(d_lat, d_long)
  
  distance <- sqrt(sum((point_a - point_b) ** 2))
  return(distance)
}

taxi_data <- cbind(taxi_data, euclidean_distance = mapply(
  euclidean_dist, 
  taxi_data$pickup_longitude, 
  taxi_data$pickup_latitude,
  taxi_data$dropoff_longitude,
  taxi_data$dropoff_latitude
))


taxi_data$euclidean_distance <- round(
  taxi_data$euclidean_distance,
  2
)

mean_euclidean_dist <- taxi_data %>%
  group_by(euclidean_distance) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_euclidean_dist,
  aes(x=euclidean_distance, y=mean_trip)
) + geom_point()

cor(
  taxi_data$euclidean_distance,
  taxi_data$trip_duration
)

# The euclidean distance has a higher correlation! not by much, though. Keep both of them in the table 
# Final feature is checking the weekday 

taxi_data$pickup_weekday <- strftime(taxi_data$pickup_datetime, "%A", tz='EST')

mean_weekday <- taxi_data %>%
  group_by(pickup_weekday) %>%
  summarize(mean_trip = mean(trip_duration))

ggplot(
  data = mean_weekday,
  aes(x=pickup_weekday, y=mean_trip)
) + geom_point()

# Building a final pipeline for our features

build_features <- function(file_path, manhattan_func,
                           euclidean_func) {
  Sys.setlocale("LC_TIME", "C")
  
  taxi_data <- read.csv(file_path)
  
  # Build Time Based Features
  taxi_data$pickup_month <- strftime(taxi_data$pickup_datetime,'%m')
  taxi_data$pickup_day <- strftime(taxi_data$pickup_datetime,'%d')
  taxi_data$pickup_hour <- strftime(taxi_data$pickup_datetime, "%H", tz='EST')
  taxi_data$pickup_weekday <- strftime(taxi_data$pickup_datetime, "%A", tz='EST')
  
  taxi_data <- cbind(taxi_data, manhattan_distance = mapply(
    manhattan_func, 
    taxi_data$pickup_longitude, 
    taxi_data$pickup_latitude,
    taxi_data$dropoff_longitude,
    taxi_data$dropoff_latitude
  ))
  
  
  taxi_data <- cbind(taxi_data, euclidean_distance = mapply(
    euclidean_dist, 
    taxi_data$pickup_longitude, 
    taxi_data$pickup_latitude,
    taxi_data$dropoff_longitude,
    taxi_data$dropoff_latitude
  ))
  
  return (taxi_data)
}

taxi_data_features <- build_features(
  './data/curated_train.csv',
  manhattan_dist,
  euclidean_dist
)

write.csv(taxi_data_features,'./data/taxi_data_features.csv')
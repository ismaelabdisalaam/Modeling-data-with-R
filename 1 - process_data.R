# The data being used is from the kaggle competition: 
# https://www.kaggle.com/c/nyc-taxi-trip-duration/data

# Predict the duration of a taxi trip given different variables 
# Loading the training data

path <- './data/train.csv'
taxi_data <- read.csv(path)

# The target variable is the duration of the trip in seconds
# Other variables regarding the taxi trip:
# - the datetime of pickup
# - the dropoff datetime
# - the latitude and longitude of the pickup
# - the latitude and longitude of the dropoff
# - No. of passengers
# - Variable that indicates if the vehicle had a connection to the central server at the time of the trip
# - the id of the vendor


# Explore the data

library(ggplot2)
library(dplyr)

ggplot(
  data=taxi_data,
  aes(x=pickup_longitude)
  ) + geom_histogram(bins=30)

summary(taxi_data)

# It's possible that we have some outliers on this variable
# Checking the pickup_longitude with less than -100

taxi_data %>%
  filter(pickup_longitude < -100)

# Checking the trips with more than 10000 seconds of duration

taxi_data %>%
  filter(trip_duration > 10000)

# How many of these trips is there?
  
taxi_data %>%
  filter(trip_duration > 10000) %>%
  count()

long_lat <- taxi_data %>%
  filter(trip_duration > 10000) %>%
  select(pickup_longitude, pickup_latitude,
         dropoff_longitude, dropoff_latitude, trip_duration)

# Exclude these trips from the modelling phase as they might be outliers

# Checking on the variable without outliers

ggplot(
  data=taxi_data,
  aes(x=passenger_count)
) + geom_histogram(bins=30)

# Longitude Analysis

ggplot(
  data=taxi_data,
  aes(x=pickup_longitude)
) + geom_histogram(bins=30)

avg_pl <- mean(taxi_data$pickup_longitude)
stdev_pl <- sd(taxi_data$pickup_longitude)

taxi_data_filter <- taxi_data[
  (taxi_data$pickup_longitude<avg_pl+3*stdev_pl)
  &
  (taxi_data$pickup_longitude>avg_pl-3*stdev_pl),
  ]

# How many rows have been lost?

nrow(taxi_data)-nrow(taxi_data_filter)

ggplot(
  data=taxi_data_filter,
  aes(x=pickup_longitude)
) + geom_histogram(bins=30)

# Going for the latitude of the pickup

ggplot(
  data=taxi_data,
  aes(x=pickup_latitude)
) + geom_histogram(bins=30)

avg_plat <- mean(taxi_data$pickup_latitude)
stdev_plat <- sd(taxi_data$pickup_latitude)

# How many rows do we have now?
nrow(taxi_data_filter)

# - 1458277

taxi_data_filter <- taxi_data_filter[
  (taxi_data_filter$pickup_latitude<avg_plat+3*stdev_plat)
  &
    (taxi_data_filter$pickup_latitude>avg_plat-3*stdev_plat),
  ]

nrow(taxi_data_filter)

# And after filtering - 1424845

ggplot(
  data=taxi_data_filter,
  aes(x=pickup_latitude)
) + geom_histogram(bins=100)

# Let's go for the longitude of the Dropoff
ggplot(
  data=taxi_data,
  aes(x=dropoff_longitude)
) + geom_histogram(bins=30)

avg_dlong <- mean(taxi_data_filter$dropoff_longitude)
stdev_dlong <- sd(taxi_data_filter$dropoff_longitude)

# How many rows do we have now?
nrow(taxi_data_filter)

# - 1424845

taxi_data_filter <- taxi_data_filter[
  (taxi_data_filter$dropoff_longitude<avg_dlong+3*stdev_dlong)
  &
    (taxi_data_filter$dropoff_longitude>avg_dlong-3*stdev_dlong),
  ]

nrow(taxi_data_filter)

# - 1392725

ggplot(
  data=taxi_data_filter,
  aes(x=dropoff_longitude)
) + geom_histogram(bins=100)

# And finally looking at dropoff latitude

avg_dlat <- mean(taxi_data_filter$dropoff_latitude)
stdev_dlat <- sd(taxi_data_filter$dropoff_latitude)


taxi_data_filter <- taxi_data_filter[
  (taxi_data_filter$dropoff_latitude<avg_dlat+3*stdev_dlat)
  &
    (taxi_data_filter$dropoff_latitude>avg_dlat-3*stdev_dlat),
  ]

nrow(taxi_data_filter)

# - 1372422

ggplot(
  data=taxi_data_filter,
  aes(x=dropoff_latitude)
) + geom_histogram(bins=100)

# Now let's check the final target variable and remove weird duration values

avg_duration <- mean(taxi_data_filter$trip_duration)
stdev_duration <- sd(taxi_data_filter$trip_duration)

taxi_data_filter <- taxi_data_filter[
  (taxi_data_filter$trip_duration<avg_duration+3*stdev_duration)
  &
    (taxi_data_filter$trip_duration>avg_duration-3*stdev_duration),
  ]

nrow(taxi_data_filter)

# - 1370538

# Removed around 6% of the original dataset

1-nrow(taxi_data_filter)/nrow(taxi_data)

# Checking if there are NA's in the columns

colSums(is.na(taxi_data_filter))

# No NA's save this file for analysis

write.csv(taxi_data_filter, './data/curated_train.csv')
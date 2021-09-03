path <- './data/test.csv'

# Load the libraries
library(dplyr)
library(caret)

# Building Features for the test set

manhattan_dist <- function(p_lat, p_long, d_lat, d_long){
  
  point_a <- c(p_lat, p_long)
  point_b <- c(d_lat, d_long)
  
  distance <- sum(abs(point_a-point_b))
  return(distance)
}


euclidean_dist <- function(p_lat, p_long, d_lat, d_long){
  
  point_a <- c(p_lat, p_long)
  point_b <- c(d_lat, d_long)
  
  distance <- sqrt(sum((point_a - point_b) ** 2))
  return(distance)
}

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

test_data <- build_features(
  path,   
  manhattan_dist,
  euclidean_dist
  )

# Adding dummy variables to the test set
onehot <- dummyVars(" ~ pickup_weekday", data=test_data)
dummy_variables <- data.frame(predict(onehot, newdata = test_data)) 
test_data <- cbind(test_data, dummy_variables)

test_data <- test_data %>%
  rename(
    pickup_weekday.Friday = pickup_weekdayFriday,
    pickup_weekday.Monday = pickup_weekdayMonday,
    pickup_weekday.Saturday = pickup_weekdaySaturday,
    pickup_weekday.Sunday = pickup_weekdaySunday,
    pickup_weekday.Thursday = pickup_weekdayThursday,
    pickup_weekday.Tuesday = pickup_weekdayTuesday,
    pickup_weekday.Wednesday = pickup_weekdayWednesday
  )

# Convert some columns to integer to match the training dataset

test_data[,c('store_and_fwd_flag',
             'pickup_month','pickup_day',
             'pickup_hour')] <- sapply(
  test_data[,c('store_and_fwd_flag',
           'pickup_month','pickup_day',
           'pickup_hour')], as.integer)

# Loading our Stored Random Forest

rf_model <- readRDS("./models/random_forest_model.rds")

test_data$trip_duration <- predict(
  rf_model,
  test_data
)

# Write our predictions
write.csv(
  test_data[,c('id','trip_duration')],
  './data/predictions.csv',
  row.names=FALSE)

onehot <- dummyVars(" ~ pickup_weekday", data=taxi_data)
dummy_variables <- data.frame(predict(onehot, newdata = taxi_data)) 

taxi_data <- cbind(taxi_data, dummy_variables)

# Build our final table 

taxi_data_modelling <- taxi_data[,c('vendor_id','passenger_count',
                                    'trip_duration','pickup_month',
                                    'pickup_day','pickup_hour',
                                    'manhattan_distance', 'euclidean_distance',
                                    'pickup_weekday.Friday', 'pickup_weekday.Monday',
                                    'pickup_weekday.Saturday', 'pickup_weekday.Sunday',
                                    'pickup_weekday.Thursday', 'pickup_weekday.Tuesday',
                                    'pickup_weekday.Wednesday')]


# Build training and test data

train_test_split <- function(data, percentage) {
  
  data_with_row_id <- data %>% 
    mutate(id = row_number())
  
  set.seed(1234)
  training_data <- data_with_row_id %>%
    sample_frac(percentage)
  test_data <- anti_join(
    data_with_row_id,
    training_data,
    by='id'
  )
  
  training_data$id <- NULL
  test_data$id <- NULL
  
  return (list(training_data, test_data))
}

training_data <- train_test_split(taxi_data_modelling, 0.05)[[1]]
test_data <- train_test_split(taxi_data_modelling, 0.05)[[2]]

# keep 10% of the dataset for testing purposes
# Start by fitting a linear model!

taxi_linear_model <- lm(
  trip_duration ~ .,
  data=training_data
)

summary(taxi_linear_model)

# Check the R-Squared on the test set and the Root Mean Squared Error

predictions <- predict(taxi_linear_model, test_data)

calc_rsquare = function(y,y_pred){
  return (cor(y, y_pred)^2)
}

calc_rsquare(test_data$trip_duration, predictions)

# Use metrics from mltools
library(mltools)

# Check the Root Mean Squared Error
rmse(predictions, test_data$trip_duration)


# Not bad!
# RMSE: 365.1405
# R Squared: 0.5278993

# Remove some objects that are taking space in the environment
rm(dummy_variables)
rm(taxi_data_modelling)
rm(taxi_linear_model)
rm(taxi_data)
gc()

features <- c('vendor_id','passenger_count',
              'pickup_month',
              'pickup_day','pickup_hour',
              'manhattan_distance', 'euclidean_distance',
              'pickup_weekday.Friday', 'pickup_weekday.Monday',
              'pickup_weekday.Saturday', 'pickup_weekday.Sunday',
              'pickup_weekday.Thursday', 'pickup_weekday.Tuesday',
              'pickup_weekday.Wednesday')

# ranger for Random Forest
library(ranger)

rf_model <- ranger(
  trip_duration ~ .,
  data=training_data,
  num.trees = 4,
  mtry = 4
  )

predictions_rf <- predict(rf_model, test_data)$predictions

# Calculate the R-Square for Random Forest
calc_rsquare(test_data$trip_duration, predictions_rf)

# Much better! 0.5758208

# Check the Root Mean Squared Error
rmse(predictions_rf, test_data$trip_duration)

#use caret to train the model using the ranger library

caret_model <- train(
  trip_duration ~ .,
  data = training_data, 
  method = "ranger",
  num.trees = 4
)

predictions_caret <- predict(caret_model, test_data)

calc_rsquare(test_data$trip_duration, predictions_caret)
rmse(predictions_caret, test_data$trip_duration)


# Defining a tuning grid, trying different mtry and num.trees

tuneGrid <- data.frame(
  .mtry = c(2, 3, 4, 5, 6),
  .splitrule = "variance",
  .min.node.size = c(10,50,1000,1000,1000)
)

# Cross validation
model <- train(
  trip_duration ~ .,
  tuneGrid = tuneGrid,
  data = training_data, 
  method = "ranger",
  trControl = trainControl(method = "cv", 
                           number = 2, 
                           verboseIter = TRUE)
)

model

predictions_final_model <- predict(model, test_data)

calc_rsquare(test_data$trip_duration, predictions_final_model)
rmse(predictions_caret, test_data$trip_duration)

# Saving model in a file
saveRDS(model, "./models/random_forest_model.rds")

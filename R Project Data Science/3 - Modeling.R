# Let's start by loading the data with the features

path <- './data/taxi_data_features.csv'
taxi_data <- read.csv(path)

# Load the libraries
library(dplyr)

# Build a variable with the week day reflected with 1's or 0's 
library(caret)

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

# Keep 10% of the dataset for testing purposes
# Start by fitting a linear model!

taxi_linear_model <- lm(
  trip_duration ~ .,
  data=training_data
)

summary(taxi_linear_model)

# What is the mean average squared error
# Check the R-Squared on the test set and also the Root Mean Squared Error

predictions <- predict(taxi_linear_model, test_data)

calc_rsquare = function(y,y_pred){
  return (cor(y, y_pred)^2)
}

calc_rsquare(test_data$trip_duration, predictions)

# Using metrics from mltools
library(mltools)

# Check the Root Mean Squared Error
rmse(predictions, test_data$trip_duration)


# Not bad 
# RMSE: 365.1405
# R Squared: 0.5278993


# Remove some objects that are taking space in the environment
rm(dummy_variables)
rm(taxi_data_modelling)
rm(taxi_linear_model)
rm(taxi_data)
gc()

# ranger is much faster in terms of implementation for Random Forest
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

# Use caret to train the model using the ranger library
caret_model <- train(
  trip_duration ~ .,
  data = training_data, 
  method = "ranger",
  num.trees = 4
)

predictions_caret <- predict(caret_model, test_data)

calc_rsquare(test_data$trip_duration, predictions_caret)
rmse(predictions_caret, test_data$trip_duration)

# With Caret we can perform Hyperparameter tuning to understand which arguments may be better in random forest
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

# Saving model
saveRDS(model, "./models/random_forest_model.rds")

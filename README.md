# Building a Model that predicts the duration of a Taxi trip 
In this project i build a model that predicts how long a taxi trip will take from the pickup point to the drop of point in seconds. The dataset is obtained from a kaggle competition New York City Taxi Trip Duration.           
The workflow is in four phases:             
1-Processing the data         
2-Features and analysing trends         
3-Modeling phase - Linear Regression and random forest          
4-Scoring the test data based  on the root mean sqaure algorithm


# Project skills: Data analysis, Modeling and Predicitve analytics-Linear regression, Random forest approach
Packages used:dplyr,ggplot2, caret, mltools, range

# Results 
After building the model our root mean square is 0.527

# Dataset information
Link to dataset: https://www.kaggle.com/c/nyc-taxi-trip-duration/overview/description     
File descriptions
train.csv - the training set (contains 1458644 trip records)
test.csv - the testing set (contains 625134 trip records)

Data fields 
id - a unique identifier for each trip  
vendor_id - a code indicating the provider associated with the trip record  
pickup_datetime - date and time when the meter was engaged  
dropoff_datetime - date and time when the meter was disengaged  
passenger_count - the number of passengers in the vehicle (driver entered value)  
pickup_longitude - the longitude where the meter was engaged  
pickup_latitude - the latitude where the meter was engaged  
dropoff_longitude - the longitude where the meter was disengaged  
dropoff_latitude - the latitude where the meter was disengaged  
store_and_fwd_flag - This flag indicates whether the trip record was held in vehicle memory before sending to the vendor because the vehicle did not have a connection to the server - Y=store and forward; N=not a store and forward trip  
trip_duration - duration of the trip in seconds


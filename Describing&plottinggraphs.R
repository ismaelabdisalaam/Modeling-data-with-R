#Describing and Plotting basic histograms and graphs for the famous dataset iris

library(datasets)  
if (!require("pacman")) install.packages 
pacman::p_load(pacman, psych) 

# Load dataset

head(iris)

# describing variables

describe(iris$Sepal.Length)  
describe(iris)               

# Plot basic histograms

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

# Put graphs in 3 rows and 1 column
par(mfrow = c(3, 1))

# Histograms for each species
hist(iris$Petal.Width [iris$Species == "setosa"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Setosa",
     xlab = "",
     col = "red")

hist(iris$Petal.Width [iris$Species == "versicolor"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Versicolor",
     xlab = "",
     col = "purple")

hist(iris$Petal.Width [iris$Species == "virginica"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Virginica",
     xlab = "",
     col = "blue")

# Restore parameter
par(mfrow=c(1, 1))

# Plot
plot(iris$Species)  
plot(iris$Petal.Length) 
plot(iris$Species, iris$Petal.Width)  
plot(iris$Petal.Length, iris$Petal.Width)  
plot(iris)  

# Plot with options
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",  
     pch = 19,         
     main = "Iris: Petal Length vs. Petal Width",
     xlab = "Petal Length",
     ylab = "Petal Width")

# Clear packages and plots
detach("package:datasets", unload = TRUE)
dev.off()  
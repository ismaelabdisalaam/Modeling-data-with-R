# Data manipulation using dplyr packages

install.packages("dplyr")
library(dplyr)

install.packages('nycflights13')
library('nycflights13')

View(flights)
head(flights)

# Subset dataset using filter()

f1 <- filter(flights,month==07)
View(f1)

f2 <- filter(flights,month==07,day==3)
f2
View(f2)

View(filter(flights,month==09,day==2,origin=='LGA'))

#slice() allows to select rows 

slice(flights,1:5)
slice(flights,5:10)

# mutate() to add new column

over_delay<-mutate(flights,overall_delay=arr_delay-dep_delay)
View(over_delay)
head(over_delay)

# transmute() to show only new column

over_delay<-transmute(flights,overall_delay=arr_delay-dep_delay)
View(over_delay)

# summarise() to find descriptive statistics

summarise(flights,avg_air_time=mean(air_time,na.rm=T))
summarise(flights,tot_air_time=sum(air_time,na.rm=T))
summarise(flights,stdev_air_time=sd(air_time,na.rm=T))
summarise(flights,avg_air_time=mean(air_time,na.rm=T),tot_air_time=sum(air_time,na.rm=T))

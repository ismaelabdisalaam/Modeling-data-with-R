# Data manipulation using Tidyr

install.packages('tidyr')
library('tidyr')

n=10

wide <- data.frame(
  ID = c(1:n),
  Face.1 = c(411,723,325,456,579,612,709,513,527,379),
  Face.2 = c(123,300,400,500,600,654,789,906,413,567),
  Face.3 = c(1457,1000,569,896,956,2345,780,599,1023,678)
)

View(wide)

# Gather() - Reshaping data from wide format to long format

long <- wide %>% gather(Face, ResponseTime, Face.1:Face.3)
View(long)

# Separate() - splits a single column into multiple columns

long_separate <- long %>% separate(Face, c("Target", "Number"))

View(long_separate)

# unite() - combines multiple columns into a single column

long_unite <- long_separate %>% unite(Face, Target, Number, sep = ".")

View(long_unite)

# spread() - takes two columns (key & value) and spreads in to multiple columns, 
#it makes "long" data wider

back_to_wide <- long_unite %>% spread(Face, ResponseTime)

View(back_to_wide)
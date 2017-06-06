# Load the library
library(dplyr)

# Create the fake data
fakedata <- data.frame(species=rep(c('a','b','c'), each=10), 
                       location=c('forest','field'),
                       leafarea=rnorm(n=30,mean=c(1,2,3),sd=0.1),
                       height=rnorm(n=30,mean=c(30,20,50),sd=1))

# Two methods of organizing the data

# Method 1: 
fakedata %>%
  group_by(location, species) %>%
  summarize_all(.funs = sum)

# Method 2
summarize_all(group_by(fakedata, location, species), .funs=sum)


library(tidyverse)
library(fpp2) 

# create training and testing set
# of the Google stock data
goog.train <- window(goog, 
                     end = 900)
goog.test <- window(goog, 
                    start = 901)

# create training and testing 
# set of the AirPassengers data
qcement.train <- window(qcement, 
                        end = c(2012, 4))
qcement.test <- window(qcement, 
                       start = c(2013, 1))
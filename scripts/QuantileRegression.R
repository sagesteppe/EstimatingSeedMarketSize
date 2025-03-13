setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)
library(quantregGrowth)
source('functions.R')

x_l <- read.csv(
  file.path('..', 'data', 'processed', 'NoFires-TotalArea_byDOIRegion.csv'))  %>% 
  split(., f = .$REG_NAME)

lapply(x_l, quantReg, wts_p = 'log')
lapply(x_l, quantReg, wts_p = 'exp')
lapply(x_l, quantReg, wts_p = 'none')
lapply(x_l, quantReg, wts_p = 'linear')

# if we make one assumption then we can use a 'growth chart' 
# the assumption is one of monotonicity, that is a directiona trend exists in the data
# although this may seen a rough assumption when forecasting year+1 estimates, it 
# allows for a non-linear fit of the conditional responses by using b-splines. 

# Technically we could avoid the monotone term, but our data are too sparse to 
# really pull this off. 

# using the quantreg package not only allows for us to keep out upper quantile 
# estimates with positive coefficients, but also avoids the 'crossing' of estimates
# which occurs when using quantreg, where each model is fit without consideration 
# of the others. 

lapply(x_l, quantReg, gc = TRUE)

# this will get the estimates for each individual quantile to be interpolated between.
lapply(x_l, quantReg2)















#####33 try and use their predict function!s
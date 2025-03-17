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

# sometimes their is an issue with the final part of the function finding this in the
# appropriate environment; set it globally (yuck) to ensure it can be found. 
form.sm <- paste0('TotalArea_Acre', ' ~ ps(', 'FIRE_YEAR', ', monotone=1)')
lapply(x_l, quantReg, gc = TRUE)

# this will get the estimates for each individual quantile to be interpolated between.
lapply(x_l, quantPred, write = TRUE)



# here we can see the results of a the default prediction, the slope is quite modest. 
aky <- x_l$`Mississippi Basin`
aky <- drop_na(aky)
preds <- data.frame(FIRE_YEAR = 2025:2030)


quants <- c(0.05, 0.25, 0.4, 0.5, 0.6, 0.75, 0.95)
mod <- quantregGrowth::gcrq(TotalArea_Acre ~ ps(FIRE_YEAR, monotone=1), tau = quants, data = aky)
preds <- abs(predict(mod, preds))
mod$call$formula <- 'TotalArea_Acre ~ ps(FIRE_YEAR, monotone=1)'

dat <- cbind(preds, Year = 2025:2030) |>
  data.frame() |>
  pivot_longer(starts_with('X')) |>
  mutate(name = gsub('X', '', name))

ggplot(data = dat, aes(x = Year, y = value, color = name)) +
  geom_line()

read.csv('../results/Tabular/QuantileForecasts/Mississippi_Basin-gc.csv')

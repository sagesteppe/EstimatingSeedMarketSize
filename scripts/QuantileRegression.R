setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)

firesYear <- read.csv(file.path('..', 'data', 'processed', 
                                'NoFires-TotalArea_byDOIRegion.csv')) |>
  filter(REG_NAME == 'California-Great Basin') 

# note we can feed wts into this function, the most simple weights that we can try 
# and impose would be a linear decay, although I wonder whether an exponential or
# other type of decay would be admissible
wts = rep(1, length.out = nrow(firesYear))
wts <- seq(0.1, 1, length.out = nrow(firesYear)) # linear decay
wts <- exp(wts)

# plot(seq(0.01, 1, length.out = length(wts)), wts) # what we want, although more decay in the tail wouldn't be bad

smodel.lm <- lm(TotalArea_Acre ~ FIRE_YEAR, data = firesYear)
model.lwr <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.025, weights = wts)
model.25 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.25, weights = wts)
model.4 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.4, weights = wts)
model.5 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.5, weights = wts)
model.6 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.6, weights = wts)
model.75 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.75, weights = wts)
model.upr <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.975, weights = wts)

# I would expect this to force quantiles from 0.01 to 0.99 to be calculated at 
# intervals of 0.01 based on the documentation. I'm not really sure why that 
# isn't what is happening (per the documentation). 
all <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 1.01)

par(mar = c(7, 5, 4, 2))
plot(
  x = firesYear$FIRE_YEAR,
  y = firesYear$TotalArea_Acre,
  main = paste0('Total Area Burned - quantile regression\n', firesYear[['REG_NAME']][1]),
  xlab = 'Fire Year', 
  ylab = 'Total Area (Acre)',
  pch = 20,
  cex = 1.2,
  yaxt = "n"
) 
abline(model.5, col = 'grey0')
abline(model.4, lty = 5, col = 'grey20')
abline(model.6, lty = 5, col = 'grey20')
abline(model.25, lty = 4, col = 'grey40')
abline(model.75, lty = 4, col = 'grey40')
abline(model.lwr, lty = 3, col = 'grey60')
abline(model.upr, lty = 3, col = 'grey60')
axis(2, 
     at = labs <- pretty(par()$usr[3:4]),
     labels = prettyNum(
       labs, big.mark = ",", scientific = FALSE)
)

legend(
  x = "topleft", 
  legend = c("Median (0.5", "0.4 & 0.6", 'quartiles (0.25,0.75)', '0.95 band (0.025,0.975)'),
  lty = c(1, 5:3),  
  col = paste0('grey', seq(0, 60, length.out = 4)),
  lwd = 2, 
  bg = adjustcolor("white", 0.4)
)  

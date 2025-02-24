setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)

firesYear <- read.csv(file.path('..', 'data', 'processed', 
                                'NoFires-TotalArea_byDOIRegion.csv')) |>
  filter(REG_NAME == 'Columbia-Pacific Northwest') #|>
 # filter(FIRE_YEAR > 1994)

model.lm <- lm(TotalArea_Acre ~ FIRE_YEAR, data = firesYear)
model.1 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.1)
model.2 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.2)
model.4 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.4)
model.5 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.5)
model.6 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.6)
model.8 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.8)
model.9 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.9)


par(mar = c(7, 5, 4, 2))
plot(
  x = firesYear$FIRE_YEAR,
  y = firesYear$TotalArea_Acre,
  main = paste0('Total Area Burned - quantile qegression\n', firesYear[['REG_NAME']][1]),
  xlab = 'Fire Year', 
  ylab = 'Total Area (Acre)',
  pch = 20,
  cex = 1.2,
  yaxt = "n"
) 
abline(model.5, col = 'grey0')
abline(model.4, lty = 5, col = 'grey20')
abline(model.6, lty = 5, col = 'grey20')
abline(model.2, lty = 4, col = 'grey40')
abline(model.8, lty = 4, col = 'grey40')
abline(model.1, lty = 3, col = 'grey60')
abline(model.9, lty = 3, col = 'grey60')
axis(2, 
     at = labs <- pretty(par()$usr[3:4]),
     labels = prettyNum(
       labs, big.mark = ",", scientific = FALSE)
)

legend(
  x = "topleft", 
  legend = c("Median", "0.4 & 0.6", '0.2 & 0.8', '0.1 & 0.9'),
  lty = c(1, 5:3),  
  col = paste0('grey', seq(0, 60, length.out = 4)),
  lwd = 2, 
  bg = adjustcolor("white", 0.4)
)  



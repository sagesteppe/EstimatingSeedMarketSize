setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)

firesYear <- read.csv(file.path('..', 'data', 'processed', 
                                'NoFires-TotalArea_byDOIRegion.csv')) |>
  filter(REG_NAME == 'Columbia-Pacific Northwest')


model.lm <- lm(TotalArea_Acre ~ FIRE_YEAR, data = firesYear)
model.4 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.4)
model.5 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.5)
model.6 <- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.6)
model.625<- rq(TotalArea_Acre ~ FIRE_YEAR, data = firesYear, tau = 0.625)

par(mar = c(7, 5, 4, 2))
plot(
  x = firesYear$FIRE_YEAR,
  y = firesYear$TotalArea_Acre,
  main = paste0('Total Area Burned - and possibly treated\n', firesYear[['REG_NAME']][1]),
  xlab = 'Fire Year', 
  ylab = 'Total Area (Acre)',
  pch = 20,
  cex = 1.2,
  yaxt = "n"
) 
lines(firesYear[['FIRE_YEAR']], firesYear[['TotalArea_Acre']], lty = 3, col = 'grey30')
abline(model.lm, col = 'grey60')
abline(model.5)
abline(model.4)
abline(model.6)
abline(model.625)
axis(2, 
     at = labs <- pretty(par()$usr[3:4]),
     labels = prettyNum(
       labs, big.mark = ",", scientific = FALSE)
)

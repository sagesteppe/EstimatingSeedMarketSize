setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)

x <- read.csv(file.path('..', 'data', 'processed', 
                                'NoFires-TotalArea_byDOIRegion.csv')) |>
  filter(REG_NAME == 'California-Great Basin') 

resp <- 'TotalArea_Acre'
pred <- 'FIRE_YEAR'
wts_p <- 'linear'
quants <- c(0.025, 0.4, 0.5, 0.6, 0.75, 0.95)
form <- as.formula(paste0(resp, '~', pred))

# wts can be feed into the function if more recent values are more important than
# historic values. 

if(wts_p=='none'){
  wts <- rep(1, length.out = nrow(x))
} else {
  wts <- seq(0.1, 1, length.out = nrow(x))
}
if(wts_p=='exp'){
  wts <- exp(wts)
}

quantL <- vector(mode = 'list', length = length(quants))
names(quantL) <- paste0('model.', quants)

for (i in seq(quants)){ 
  quantL[[i]] <- rq(form, data = x, tau = wts[i], weights = wts)
}

model.lm <- lm(form, data = x)

# I would expect this to force quantiles from 0.01 to 0.99 to be calculated at 
# intervals of 0.01 based on the documentation. I'm not really sure why that 
# isn't what is happening (per the documentation). 

linetype <- c()
cols <- paste0('grey', seq(0, 100, length.out = 4))

par(mar = c(7, 5, 4, 2))
plot(
  x = x[[pred]],
  y = x[[resp]],
  main = paste0('Total Area Burned - quantile regression\n', x[['REG_NAME']][1]),
  xlab = 'Fire Year', 
  ylab = 'Total Area (Acre)',
  pch = 20,
  cex = 1.2,
  yaxt = "n"
) 
abline(model.lm, col = 'blue')


for (i in seq()){
  abline(quantL[[i]], lty = linetype[i], col = cols[i])
}
abline(model.5, col = 'grey0')
abline(model.4, lty = 5, col = 'grey20')
abline(model.6, lty = 5, col = 'grey20')
abline(model.25, lty = 4, col = 'grey40')
abline(model.75, lty = 4, col = 'grey40')
abline(model.lwr, lty = 3, col = 'grey80')
abline(model.upr, lty = 3, col = 'grey60')
axis(2, 
     at = labs <- pretty(par()$usr[3:4]),
     labels = prettyNum(
       labs, big.mark = ",", scientific = FALSE)
)

legend(
  x = "topleft", 
  legend = c("Median (0.5", "0.4 & 0.6", 'Quartiles (0.25,0.75)', '0.95 band (0.025,0.975)'),
  lty = c(1, 5:3),  
  col = paste0('grey', seq(0, 60, length.out = 4)),
  lwd = 2, 
  bg = adjustcolor("white", 0.4)
)  


#' Fit quantile regression models to a time series using an autoregressive process. 
#' 
#' @description 
#' @param x Data frame. Must contain all data required to perform modelling. 
#' @param quants Numeric vector - odd. The quantiles which you want to model fits for, defaults to c(0.025, 0.25, 0.5, 0.75, 0.95), for the 0.95 band, the quartiles, and the median. 
#' @param wts_p Character. One of 'none' for unweighted regression (the default), 'linear' for a linear decay of weights, or 'exp' for exponential decay of weigts. 
#' @param resp Character. Name of the response field.  
#' @param pred Character. Name of the predictor field.  
#' @param plotLM Boolean. Defaults to TRUE, whether to also plot the fit of a linear model to the plot. 
quartReg <- function(x){
  
  
  
}
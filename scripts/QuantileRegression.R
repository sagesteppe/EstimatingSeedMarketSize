setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)

x <- read.csv(file.path('..', 'data', 'processed', 
                                'NoFires-TotalArea_byDOIRegion.csv')) |>
  filter(REG_NAME == 'California-Great Basin') 

resp <- 'TotalArea_Acre'
pred <- 'FIRE_YEAR'
wts_p <- 'linear'
quants <- c(0.025, 0.25, 0.4, 0.5, 0.6, 0.75, 0.95)
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
  quantL[[i]] <- rq(form, data = x, tau = quants[i], weights = wts)
}

model.lm <- lm(form, data = x)

'grey100'

if(length(quantL)==7){
  linetypes <- c(3:5, 1, 5:3)
  cols <- paste0('grey', round(seq(80, 0, length.out = length(quantL)/2))
                 )
} else if(length(quantL==11)){
  full = c(5, 2, 6, 4, 3) 
  linetypes <- c(full, 1, rev(full))
}

cols <- paste0('grey', round(seq(0, 100, length.out = 4)))

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
for (i in seq(quantL)){
  abline(quantL[[i]], lty = linetypes[i], col = cols[i])
}
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
quantReg <- function(x){
  
  
  
}
setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(tidyverse)
library(quantreg)
library(quantregGrowth)

x_l <- read.csv(
  file.path('..', 'data', 'processed', 'NoFires-TotalArea_byDOIRegion.csv'))  %>% 
  split(., f = .$REG_NAME)

lapply(x_l, quantReg, wts_p = 'log')
lapply(x_l, quantReg, wts_p = 'exp')
lapply(x_l, quantReg, wts_p = 'none')
lapply(x_l, quantReg, wts_p = 'linear')

lapply(x_l, quantReg, gc = TRUE)

# if we make one assumption then we can use a 'growth chart' 
# the assumption is one of monotonicity, that is a directiona trend exists in the data
# although this may seen a rough assumption when forecasting year+1 estimates, it 
# allows for a non-linear fit of the conditional responses by using b-splines. 

# Technically we could avoid the monotone term, but our data are too sparse to 
# really pull this off. 

# using the quantreg package not only allows for us to keep out upper quantile 
# estimates with positive coefficients, but also avoids the 'crossing' of estimates
# which occurrs when using quantreg, where each model is fit without consideration 
# of the others. 


#' Fit quantile regression models to an annual time series using an auto regressive process. 
#' 
#' @description 
#' @param x Data frame. Must contain all data required to perform modelling. 
#' @param quants Numeric vector - odd. The quantiles which you want to model fits for, defaults to c(0.025, 0.25, 0.5, 0.75, 0.95), for the 0.95 band, the quartiles, and the median. 
#' @param wts_p Character. One of 'none' for unweighted regression (the default), 'linear' for a linear decay of weights, or 'exp' for exponential decay, and 'log' for logistic decay (reversed and made absolute) which is a tad extreme for these purposes. 
#' @param resp Character. Name of the response field.  
#' @param pred Character. Name of the predictor field.  
#' @param gc Boolean. Defaults to FALSE, if true fit conditional responses using a growth chart and b-splines with an enforcement of the assumption of positive monotonocity. 
#' @examples
#' 
#' # you the built in weighing fns can be visualized as so. The natural log seems
#' # a bit extreme.  
#' wts <- seq(0.1, 1, length = 35)
#' 
#' plot(NULL, xlim=c(0,35), xaxt="n", ylim = c(0,3),
#'     ylab="Wt of Observation", xlab="Year (current at right)", main = 'Weights for regression')
#' axis(1, at = seq(0, 35, by = 5), labels = seq(-35, 0, by = 5)) 
#' lines(abs(rev(log(wts)))+0.1, col = 'blue') # for the visualization make the weight origin align. 
#' lines(wts, col = 'red')
#' lines(exp(wts)-1, col = 'green')
#' abline(a = 0.1, b = 0) # unweighted regression 
#' rm(wts)
#'
#' 
quantReg <- function(x, quants, wts_p, resp, pred, gc){
  
  if(missing(quants)){
    quants <- c(0.05, 0.25, 0.4, 0.5, 0.6, 0.75, 0.95)}
  if(missing(wts_p)){wts_p <- 'none'}
  if(missing(resp)){resp <- 'TotalArea_Acre'} 
  if(missing(pred)){pred <- 'FIRE_YEAR'}
  if(missing(gc)){gc <- FALSE}
  
  p <- file.path('..', 'results', 'Plots', 'QuantileForecasts', 
                 paste0(gsub(' ', '_', x[['REG_NAME']][1]), '-'))
  
  form <- as.formula(paste0(resp, '~', pred))
  if(gc == FALSE){
    p <- paste0(p, wts_p, '.png')
  } else {
    form.sm <- as.formula(paste0(resp, ' ~ ps(', pred, ', monotone=1)'))
    p <- paste0(p, 'gc', '.png')
  }
  
  # wts can be feed into the function if more recent values are more important than
  # historic values. 
  
  # note that wts are ignored for the gcrq function, but the authors intends to 
  # add the functionality soon. Supplying them does not break the fn, so we will 
  # include them in args. 
  if(wts_p == 'none'){
    wts <- rep(1, length.out = nrow(x))
  } else {
    wts <- seq(0.1, 1, length.out = nrow(x))
  } # these are the linear weights
  
  if(wts_p == 'exp'){wts <- exp(wts)} else 
    if(wts_p == 'log'){wts <- abs(rev(log(wts)))} # else do nothing to the linear wts. 
  
  # now fit the models. 
  if(gc==FALSE){
    quantL <- vector(mode = 'list', length = length(quants))
    names(quantL) <- paste0('model.', quants)
    
    for (i in seq(quants)){ 
      quantL[[i]] <- quantreg::rq(form, data = x, tau = quants[i], weights = wts)
    }
    
  } else {
    mod <- quantregGrowth::gcrq(form.sm, data = x, tau = quants, weights = wts)
  }

  model.lm <- lm(form, data = x)

  L <- length(quants)
  if(L==7){
    linetypes <- c(3:5, 1, 5:3)
    
    cols <- paste0('grey', round(seq(80, 0, length.out =  floor(L/2)+1)))
    cols <- c(cols, rev(cols[1:L/2]))
  } else if(L==11){
    full = c(5, 2, 6, 4, 3) 
    linetypes <- c(full, 1, rev(full))
    
    cols <- paste0('grey', round(seq(80, 0, length.out =  floor(L/2)+1)))
    cols <- c(cols, rev(cols[1:L/2]))
  }
  
  # now plot in two different fashions... if using the default qc use the first
  # option which requires a smidge of specifications. Otherwise use the method
  # in the quantregGrowth package which will dispatch to a generic. 
  png(p)
  par(mar = c(7, 5, 4, 2))
  if(gc==FALSE){
    plot(
      x = x[[pred]],
      y = x[[resp]],
      main = paste0('Total Area Burned\n', x[['REG_NAME']][1]),
      xlab = 'Year', 
      ylab = 'Total Area Burned (Acre)',
      pch = 20,
      cex = 1.2,
      yaxt = "n"
    ) 
    for (i in seq(quantL)){
      abline(quantL[[i]], lty = linetypes[i], col = cols[i])
    }
  } else {
    plot(
      mod, 
      res = TRUE, 
      col = cols,
      lty = linetypes, 
      yaxt = 'n',
      main = paste0('Total Area Burned\n', x[['REG_NAME']][1]), 
      xlab = 'Year', 
      ylab = 'Total Area Burned (Acre)',
      ) 
    points(x[[pred]], x[[resp]], pch = 20, cex = 1.2, col = 'black')
  }

  abline(model.lm, col = 'dodgerblue4')
  axis(2, 
       at = labs <- pretty(par()$usr[3:4]),
       labels = prettyNum(
         labs, big.mark = ",", scientific = FALSE)
  )
  legend(
    x = "topleft", 
    legend = c("Median (0.5)", "0.4 & 0.6", 'Quartiles (0.25, 0.75)', '0.9 band (0.05, 0.95)', 'Mean'),
    lty = c(rev(linetypes[1:ceiling(L/2)]), 1),  
    col = c(rev(cols[1:ceiling(L/2)]), 'dodgerblue4'),
    lwd = 2, 
    bg = adjustcolor("white", 0.4)
  )  
  mtext(side=1, line=4, adj=1, cex=0.8, col = 'grey40', paste(
      if(wts_p=='exp'){'exponential decay'} else 
        if (wts_p=='linear'){'linear decay'} else
          {'logarithmic decay'}, 
      'weighted quantile regression')
      )
  
  dev.off()
  
  
  ## if using growth charts, extract the 'prediction' for the next year, although 
  # this seems to just be the max year in the data set 'drifted' out to then. I.e. 
  # the predict fn refuses to extrapolate. 
  if(gc==TRUE){
    # there is a way with/when/how quantreg grabs the formula from the fn... 
    mod$call$formula <- form.sm # but... we can overwrite this in the fn call to fix it.   
    
    preds <- data.frame(FIRE_YEAR = as.integer(format(Sys.Date(), "%Y")))
    preds <- predict(mod, preds) |> 
      cbind(preds) |> 
      tibble::rownames_to_column() |> 
      setNames(c('Tau', 'Prediction', 'FIRE_YEAR'))
    
    p <- gsub('Plots', 'Tabular', p)
    p <- gsub('.png', '.csv', p)
    write.csv(preds, p, row.names = FALSE)
  }
}



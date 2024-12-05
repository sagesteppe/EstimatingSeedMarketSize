#' Develop estimates for total burned area in each DOI region by last data year
#' 
#' @description This function performs four tasks. First it fits a simple linear model to the log
#' transformed input data where FIRE_YEAR predicts TotalArea_Acre. It then develops 
#' a confidence interval for this model, as well as three sets of prediction intervals
#' (0.95, 0.9, 0.8, or 1 in 20, 1 in 10, and 1 in 5 odds). It then plots all of these data. 
#' Finally it returns estimates of the prediction intervals in both the most recent time period, 
#' and time period + 1, via extrapolation. 
#' 
#' @param x a list of fire summary data by DOI region. 
#' @dir a directory to save the plots, and estimates. 
RegionalEstimates <- function(x, dir){
  
  mod <- lm(log(TotalArea_Acre) ~ FIRE_YEAR, data = x)
  
  ntrvls <- PredInts(x = x, y = mod)
  BurnedAreaPlots(x = x, z = ntrvls, mod = mod)

  # now we transform the prediction intervals back onto their original scale
  
}


#' Create a simple scatter plot showing estimate total burned areas by year 
#' 
#' @description A quick base r plot indicating burned areas. 
#' @param x the initial data input to `RegionalEstimates` 
#' @param z the ouput of `PredInts`  
#' @param mod the fit model object 
BurnedAreaPlots <- function(x, z, mod){
  
  pval <- anova(mod); pval <- pval$`Pr(>F)`[1]
  pval <- round(pval, 3)
  if(pval <= 0.001){pval <- 0.001}
  
  status <- paste(
    'There is',
    SupportWriter(mod),
    'evidence that the total burned area is increasing.\n', 
    
    'Linear model (p =', paste0(pval, ', RMSE = ', round(rmse(mod), 3), ')')  
  )
  
  p <- file.path('..', 'results', 'Plots', 'AnnualSummaries', 
                 paste0(gsub(' ', '_', x$REG_NAME[1]), '.png'))
  png(p)
  par(mar = c(7, 5, 4, 2))
  # Add some color to the scatter points . 
  nColor <- 10
  colors = paletteer::paletteer_c("viridis::inferno", n=nColor, direction = -1)
  rank <- as.factor(as.numeric(cut(log(x$TotalArea_Acre), nColor)))
  
  plot(
    x = x$FIRE_YEAR,
    y = log(x$TotalArea_Acre), 
    xlim = c(min(z$mod_ci$FIRE_YEAR), max(z$mod_ci$FIRE_YEAR)), 
    ylim = c(min(z$mod_pred95$lwr), max(z$mod_pred95$upr)), 
    
    cex = 1.5,
    pch = 21,
    bg = colors[ rank ], # color each point by fire size. 
    
    las = 1, # turn x axis text horizontal 
    xlab = 'Year', 
    ylab = 'log(Area Burned (Acres))',
    main = paste0('Total Annual Area Burned by Wildfires\n', x$REG_NAME[1]),
    xaxs = 'i', 
    yaxs = 'i'
  )
  
  polygon(
    x = c(
      max(x$FIRE_YEAR)+1, max(x$FIRE_YEAR)+1,  max(x$FIRE_YEAR),  max(x$FIRE_YEAR)),
    y = c(
      min(z$mod_pred95$lwr), max(z$mod_pred95$upr), max(z$mod_pred95$upr), min(z$mod_pred95$lwr)), 
    col = adjustcolor("orange", 0.3), border = 'orange'
  )
  
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred95$fit)
  
  ## CI 
  lines(z$mod_ci$FIRE_YEAR, z$mod_ci$lwr, lty = 5)
  lines(z$mod_ci$FIRE_YEAR, z$mod_ci$upr, lty = 5)
  
  ## prediction intervals 
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred95$lwr, lty = 3, col = 'grey60')
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred95$upr, lty = 3, col = 'grey60')
  
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred90$lwr, lty = 3, col = 'grey40')
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred90$upr, lty = 3, col = 'grey40')
  
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred80$lwr, lty = 3, col = 'grey20')
  lines(z$mod_ci$FIRE_YEAR, z$mod_pred80$upr, lty = 3, col = 'grey20')
  
  legend(
    x = "topleft", 
    legend = c("Fit", "95% CI", '80% PI', '90% PI', '95% PI'),
    lty = c(1, 5, 3, 3, 3),  
    col = c('black', 'black', 'grey20',  'grey40', 'grey60'),
    lwd = 2, 
    bg = adjustcolor("white", 0.4)
  )  
  
  mtext(side=1, line=5, adj=1, cex=0.8, status, col = 'grey40')
  
  dev.off()
}

#' Calculate confidence and prediction intervals
#' @param x the original data set
#' @param y the fitted model 
PredInts <- function(x, y){
  
  gr <- data.frame(FIRE_YEAR =  seq(min(x$FIRE_YEAR), max(x$FIRE_YEAR)+1))
  
  pred_help <- function(lvl){
    mod_pred <- data.frame(
      FIRE_YEAR = gr, 
      predict.lm(
        y, gr, SE=TRUE, interval = 'prediction', level = lvl)
    )
    return(mod_pred)
  }
  
  mod_pred95 <- pred_help(0.95)
  mod_pred90 <- pred_help(0.9)
  mod_pred80 <- pred_help(0.8)
  
  mod_ci <-  data.frame(
    FIRE_YEAR = gr, 
    predict.lm(
      y, gr, SE=TRUE, interval = 'confidence', level = 0.95)
  )
  
  return(
    list(
      mod_pred95 = mod_pred95, 
      mod_pred90 = mod_pred90, 
      mod_pred80 = mod_pred80,
      mod_ci = mod_ci
    )
  )
  
}

SupportWriter <- function(x){
  
  am <- anova(x)
  x1 <- am$`Pr(>F)`[1]
  
  if(x1 < -1e-3){y <- 'very strong'} else if(
    x1 < 0.01) {y <- 'strong'} else if(
      x1 < 0.05) {y <- 'moderate'} else if(
        x1 < 0.1) {y <- 'weak'} else 
        {y <- 'little or no'}
  
  return(y)
}

rmse <- function(x){sqrt(mean(x$residuals^2))} # for calculating mse


###############################################################################


#' How much of an area can be seeded each year? 
#' 
#' @description Calculate the amount of seed in warehouses, and how much of an area can be treated
#' based on a fit model and observed differences. 
#' @param x output of `pred_help` 
#' @param rolled
AreaDeficitSummary <- function(x, rolled){
  
  rolled <- dplyr::left_join(dplyr::select(x, FIRE_YEAR, fit), rolled, by = join_by(FIRE_YEAR))
  AreaDeficit <- rolled |> 
    dplyr::select(FIRE_YEAR, fit, TotalArea_Acre)
  
  AreaDeficit <- data.frame( 
    AreaDeficit,  
    Surplus = NA, 
    Warehouse = NA, 
    AnnualDeficit = NA,
    ExcessArea = NA,
    Treatable = NA
  )
  
  for (i in seq_along(1:nrow(AreaDeficit))){
    
    ## This is the amount of area which burned which is above the regression line fit. 
    AreaDeficit$AnnualDeficit[i] <- AreaDeficit$TotalArea_Acre[i] - AreaDeficit$fit[i] 
    if(AreaDeficit$AnnualDeficit[i] < 0){AreaDeficit$AnnualDeficit[i] <- 0}
    
    ## This is the amount of area seed produced in a year with less burn than regression fit. 
    AreaDeficit$Surplus[i] <- AreaDeficit$fit[i] - AreaDeficit$TotalArea_Acre[i] 
    if(AreaDeficit$Surplus[i] < 1){AreaDeficit$Surplus[i] <- 0}
    
    # This is the amount of seed being pulled from warehouse, and how much is left at
    # years end. 
    AreaDeficit$Warehouse[i] <- AreaDeficit$fit[i] - AreaDeficit$TotalArea_Acre[i] 
    if(AreaDeficit$Warehouse[i] < 1){AreaDeficit$Warehouse[i] <- 0}
    
    ##  
    if(i > 1){
      AreaDeficit$Warehouse[i] <-
        (AreaDeficit$Warehouse[i] + AreaDeficit$Warehouse[i-1]) - AreaDeficit$AnnualDeficit[i]
    }
    
    ## combining fresh harvest and warehoused seed is not always enough to treat all areas
    # how much excess area do we have? 
    if(AreaDeficit$Warehouse[i] < 0){AreaDeficit$ExcessArea[i] <- abs(AreaDeficit$Warehouse[i])}
    if(AreaDeficit$Warehouse[i] < 1){AreaDeficit$Warehouse[i] <- 0}
    
    # now how much area can be treated. 
    
    AreaDeficit$Treatable[i] <- AreaDeficit$TotalArea_Acre[i] - AreaDeficit$ExcessArea[i]
  }
  
  return(AreaDeficit)
}


avg <- function(x, y){
  x$roll <- data.table::frollmean(x$TotalArea_Acre, y)
  return(x)
}

pred_help <- function(y, lvl){
  mod_pred <- data.frame(
    FIRE_YEAR = gr, 
    predict.lm(
      y, gr, interval = 'confidence', level = lvl)
  )
  return(mod_pred)
}


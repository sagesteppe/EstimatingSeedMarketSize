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
  
  lTAA <- log(x$TotalArea_Acre)
  if(any(lTAA=='-Inf')){lTAA[which(lTAA=='-Inf')] <- 0.0}
  x$TotalArea_Acre <- as.numeric(lTAA)
  
  mod <- lm(TotalArea_Acre ~ FIRE_YEAR, data = x)
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
  rank <- as.factor(as.numeric(cut(x$TotalArea_Acre, nColor)))
  
  plot(
    x = x$FIRE_YEAR,
    y = x$TotalArea_Acre, 
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
#' @param prediction
#' @param rolled
AreaDeficitSummary <- function(x, rolled, prediction){
  
  prediction <- dplyr::enquo(prediction)
  rolled <- dplyr::left_join(dplyr::select(x, FIRE_YEAR, fit), rolled, by = join_by(FIRE_YEAR))
  AreaDeficit <- rolled |> 
    dplyr::select(FIRE_YEAR, !!prediction, TotalArea_Acre)
  
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


#' Visualize the amount of area which can be treated each year 
#' 
#' @description This function makes plots and saves them to disk. The plots depict
#' the observed amount of area burnt each year, and the amount of area which can 
#' be treated annually based on a regression line. It denotes areas which can 
#' be treated with warehoused seed in green, while areas in red depict areas in 
#' excess of what warehouses would have in storage. 
#' @param x the output of `AreaDeficitSummary`  
#' @param rolled the ouput of `avg`
#' @param mod a model prediction which can be used to depict the target amount of area which seed is grown for. 
#' @param colname the name of the column in the prediction grid to be used for plotting. 
#' @param yr_roll passed on from outer function. 
TreatableAreaPlots <- function(x, rolled, mod, colname, yr_roll){
  
  p <- file.path('..', 'results', 'Plots', 'AnnualSummariesTreatable', 
                 paste0(gsub(' ', '_', rolled[['REG_NAME']][1]), '-', yr_roll, 'yrAVG', '.png'))
  
  png(p)
  par(mar = c(7, 5, 4, 2))
  
  plot(
    x = rolled$FIRE_YEAR,
    y = rolled$TotalArea_Acre,
    main = paste0('Total Area Burned - and possibly treated\n', rolled[['REG_NAME']][1]),
    xlab = 'Fire Year', 
    ylab = 'Total Area (Acre)',
    pch = 20,
    cex = 1.2,
    yaxt = "n"
  ) 
  lines(rolled[['FIRE_YEAR']], rolled[['roll']], lty = 3, col = 'grey30')
  lines(mod[['FIRE_YEAR']], mod[[colname]])
  axis(2, 
       at = labs <- pretty(par()$usr[3:4]),
       labels = prettyNum(
         labs, big.mark = ",", scientific = FALSE)
  )
  
  # If the burned area is in excess of the fit model, then place a green line 
  # indicating the maximum amount of area which can be treated using the 
  # warehoused seed. 
  # thes are 'partial treatments' p_trt
  p_trt <- x[! is.na(x$Treatable), ]
  for (i in seq_along(1:nrow(p_trt))){
    segments(
      x0 = p_trt[i,'FIRE_YEAR'], x1 = p_trt[i,'FIRE_YEAR'],
      y0 =  p_trt[i, 'Treatable'], 
      y1 = p_trt[i, 'fit'],
      col = "darkgreen"
    )
  }
  
  # now we repeat the process for all burns above the regression line that 
  # we should have the material to treat in their entirety. 
  t_trt <- x[x$FIRE_YEAR != min(x$FIRE_YEAR),]
  t_trt <- t_trt[t_trt$Surplus==0 & is.na(t_trt$Treatable),]
  for (i in seq_along(1:nrow(t_trt))){
    segments(
      x0 = t_trt[i,'FIRE_YEAR'], x1 = t_trt[i,'FIRE_YEAR'],
      y0 =  t_trt[i, 'TotalArea_Acre'], 
      y1 = t_trt[i, 'fit'],
      col = "darkgreen"
    )
  }
  
  # we'll also have a red line segment running through the untreatable areas. 
  for (i in seq_along(1:nrow(p_trt))){
    segments(
      x0 = p_trt[i,'FIRE_YEAR'], x1 = p_trt[i,'FIRE_YEAR'],
      y0 =  p_trt[i, 'Treatable'], 
      y1 = p_trt[i, 'TotalArea_Acre'],
      col = "red4"
    )
  }
  
  # we add red 'x' to the area beyond which we have adequate seed to treat the 
  # area with. Helps draw eye to the transition from treatable-untreatable areas. 
  points(x$FIRE_YEAR, x$Treatable, pch = 4, col = 'orangered')
  
  # write a short summary for the plot. 
  status <- paste0(
    'Of the ', nrow(x)-1, ' years in this data set ', nrow(x[x$AnnualDeficit!=0,]),
    ' had fires above the regression line.\n Of these ', nrow(t_trt), 
    ' years would have enough seed material to plant at recommended\nseeding rates, while ', 
    nrow(p_trt), ' would have inadequate amounts of seed.'
  )
  
  # a short description of the results displayed in the plot. 
  mtext(side=1, line=6, adj=1, cex=0.8, status, col = 'grey40') 
  
  # denote what the lines represent. 
  legend(
    x = "topleft", 
    legend = c(paste0(yr_roll, ' year\n rolling avg.'), "Fit", "Treatable", 'Untreatable'),
    lty = c(3, rep(1, 3)),  
    col = c('grey20', 'black', 'darkgreen', 'red4'),
    lwd = 2, 
    bg = adjustcolor("white", 0.4)
  )  
  
  dev.off()
  
}


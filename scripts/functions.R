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


#' calculate a rolling mean for fire sizes
#' 
#' @description This function wraps around two under laying data.table functions 
#' to calculate a rolling mean of a numeric vector. It implements one of two functions, 
#' either data.table::frollmean, if type = 'arithematic', or a geometric mean if 
#' type = 'geometric'. The geometric mean is calculated using spatstat.utils::harmonicmean
#' wrapped within data.table::frollapply. 
#' @param x Data.frame. A data frame of values
#' @param colname. Character. A Column to calculate the values on. No default, will exit function.
#' @param w Numeric. The rolling window to use for the calculation. Defaults to 3. 
#' @param type Character. One of 'a' for arithematic or 'g' for geometric. Defaults to arithmetic. 
#' @param returns Data.frame x, with a column added with a prefix 'roll_' and the type abbreviation and
#' the window size. For example if using the arithematic mean with a window of 3, 'roll_g3'
#' @examples
#' arith <- data.frame(
#'   Value = 1:10, 
#'   Observation = LETTERS[1:10]
#' )
#'
#' geo <- data.frame(
#'   Value = exp(1:10), 
#'   Observation = LETTERS[1:10]
#' )
#'
#' avg(arith, colname = 'Value')
#' avg(geo, colname = 'Value', type = 'g', w = 4)
s#' @export 
avg <- function(x, colname, w, type){
  if(missing(w)){w <- 3}
  if(missing(type)){type <- 'a'}
  outname <- paste0('roll_', type, w)
  
  if(type == 'a'){
    x[[outname]] <- data.table::frollmean(x[[colname]], w)
  } else {
    x[[outname]]  <- frollapply(x[[colname]], w, FUN = spatstat.utils::harmonicmean)
  }
  return(x)
  
}

pred_help <- function(y, lv, gr){
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
  # these are 'partial treatments' p_trt
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

#' @description the goal of this function is to estimate the amount of area  
#' which would be needed to treat after wildfires in 12 doi regions. Area serves
#' as a proxy for the number of seeds. The function assumes three sources for seed:
#' 1) 'old_warehouse' which is the seed greater than the rolling average in years, and which has
#' not been used in restoration already. 
#' 2) 'new_warehouse' the seed within the rolling average age range
#' 3) 'new' fresh seed which will be delivered straight from farm in Fall. 
#' Despite area being the proxy for seed, and seed aging and losing it's ability to perform as well 
#' in restorations, we will maintain 'area' as a constant. 
#' 
#' @param x Dataframe. The data set to be analysed. 
#' @param yr_roll Numeric. the rolling average to apply for the analysis. 
#' Defaults to 1, which is that no rolling average is used. 
#' @param interval Character Vector. The type of interval to be used for 
#' calculating distance between, defaults to 'confidence' the other option is 'prediction'. 
#' @param prediction Vector. the name of the column from the fit model to use for 
#' calculating the distance between. Defaults to 'fit', other options are: 'lwr', and 'upr'. 
#' @param conf_lvl Numeric. The confidence level to calculate the confidence limits at, defaults to 0.95 for a 95% confidence interval. 
#' @export
reportBalance <- function(x, yr_roll, interval, prediction, conf_lvl){
  
  if(missing(interval)){interval <- 'confidence'}
  if(missing(prediction)){prediction <- 'fit'}
  if(missing(conf_lvl)){conf_lvl <- 0.95}
  
  rolled <- avg(x, yr_roll)
  modr2 <- lm(roll ~ FIRE_YEAR, data = rolled)
  
  gr <- data.frame(
    # only operate on years within the rolling average.
    FIRE_YEAR =  seq(min(rolled[!is.na(rolled$roll), 'FIRE_YEAR']),
                     max(rolled$FIRE_YEAR))
  )
  
  p <- pred_help(modr2, conf_lvl, interval) 
  ob <- AreaDeficitSummary(p, rolled, prediction) 
  
  TreatableAreaPlots(x = ob, rolled, mod = p, colname = 'fit', yr_roll = yr_roll)
}



#' calculate fire return intervals for a DOI region
#'
#' @description Calculate fire return intervals for a DOI region, and save a few different results. 
#' @param x a dataframe of observed fire events. 
#' @param returns  a plot of intervals, a data frame of predictions from the fit model, the model, and a sample of positions from the  predictions data frame. 
#' @param export
CalculateReturnIntervals <- function(x){
  
  x <- x[!is.na(x$NoFire),] # this removes recent no-data years - is safe and stable
  
  # this replaces the log of years without any burned area with a 0. Seems stable
  # but may have peculiar side effects? 
  logs <- log(x$TotalArea_Acre)
  pos <- logs=='-Inf'
  logs <- replace(logs, pos, 0)
  
  fevd_ext <- fevd(
    logs, 
    type = 'GEV',
    units ='Acres', 
    span = length(logs), 
    time.units = '1/year'
  )
  
  ret_period <- seq(1.01, 99, 0.01) # 'years' for the return intervals. 
  return_ls <- return.level(fevd_ext, ret_period, do.ci = TRUE, burn.in = 10000) 
  
  # Find the intersection of the desired value and the generated sequence of return levels
  return_ls <- data.frame(
    Period = ret_period,
    Pr = 1 - (ret_period/100), 
    lwrCI = return_ls[,1], 
    estimate = return_ls[,2], 
    uprCI = return_ls[,3] 
  )
  
  cols <- c('lwrCI', 'uprCI', 'estimate') # convert back to a linear scale 
  return_ls[,cols] <- apply(return_ls[,cols], 2, exp)
  
  positions <- sort( # sampling rows from the data frame based on the return intervals
    sample(1:nrow(return_ls), 
           prob = return_ls$Pr, 
           size = 1250, replace = TRUE))
  plot(density(sort(return_ls[positions,'Pr'])))
  
  ReturnIntervalsPlot(x = x, mod = fevd_ext, return_ls = return_ls)
  
  write.csv(
    positions, 
    file.path(
      '..', 'results', 'Tabular', 'FireReturnIntervals', 
      paste0('SampleIndex-',  x$REG_NAME[1], '.txt')
      ), row.names = F)
  
  write.csv(
    return_ls,
    file.path(
      '..', 'results', 'Tabular', 'FireReturnIntervals', 
      paste0('Predictions-', x$REG_NAME[1], '.csv')
    )
  )
  
}


#' @description Create a simple plot for internal use/documentation of results from estimating
#' fire return intervals. 
#' @param x original object input to parent function, essentially fire summary data for a DOI region 
#' @param mod fit model from `fevd` function. 
#' @param return_ls predictions of burned area 
ReturnIntervalsPlot <- function(x, mod, return_ls){
  
  p <- file.path('..', 'results', 'Plots', 'FireReturnIntervals', 
                 paste0(gsub(' ', '_', x$REG_NAME[1]), '.png'))
  
  if(mod[['type']]=='GEV'){
    type <- 'generalized extreme value distribution'} else {
      type <- 'generalized pareto'}
  status <- paste(
    'Estimates generated using', type, 'with method', 
    mod[['method']], '\nand a sample of', mod[['n']],
    'records, using the R package `extRemes`.'
  )
  
  ticks <- c(c(0, 2, 5, 10), seq(20, 100, by = 20))
  
  png(p, width = 8, height = 5.5, units = 'in', res = 300)
  par(mar = c(6, 5, 4, 2))
  plot(
    return_ls$Period, return_ls$estimate, type = 'l', 
    ylim = c(min(return_ls$lwrCI), max(return_ls$uprCI)), 
    main = paste0('Annual Fire Return Intervals\n', sub = x$REG_NAME[1]), 
    xlab = 'Return Interval (Years)', ylab = 'Area (Acres)', yaxt = "n"
  )
  
  legend(
    x = "topleft", 
    legend = c('Prediction', '95% CI', 'Observed\nAnnual Burns'),
    lty = c(1, 2, 3),  
    col = c('black', 'black', 'red4'),
    lwd = 2, cex = 0.7,
    bg = adjustcolor("white", 0.8) 
  )  
  
  for (i in 1:nrow(x)){ # these are the observed fire events from the training data
    segments(
      x0 = 0, 
      x1 = return_ls[which.min(abs(x[i,'TotalArea_Acre'] - return_ls$estimate)), 'Period'], 
      y0 = x$TotalArea_Acre[i], col = 'red', lty = 3, lwd = 0.8)
  }
  lines(return_ls$Period, return_ls$uprCI, lty = 2)
  lines(return_ls$Period, return_ls$lwrCI, lty = 2)
  axis(1, at = ticks, labels = ticks) 
  axis(2, cex = 0.4,
       at = labs <- pretty(par()$usr[3:4]),
       labels = prettyNum(
         labs, big.mark = ",", scientific = FALSE)
  )
  mtext(side=1, line=5, adj=1, cex=0.8, status, col = 'grey40') 
  dev.off()
  
}


#' Calculate rolling averages, wrapper around data.table::frollmean
#' 
#' TODO: can we use other means - especially geometric with this? 
avg <- function(x, y){
  x$roll <- data.table::frollmean(x$TotalArea_Acre, y)
  return(x)
}

#' prediction help wrapper for am ordinary least squares linear model. 
pred_help <- function(y, conf_lvl, interval){
  mod_pred <- data.frame(
    FIRE_YEAR = gr, 
    predict.lm(
      y, gr, interval = interval, SE = TRUE, level = conf_lvl)
  )
  return(mod_pred)
}


#' Obtain regression parameter estimates for classifying fire years for markov chains
#' 
#' @description Wrapper function to fit a linear model and predict it onto a matrix
#' and classify the training data as being in one of three or optionally three classes. 
#' 'below' for all points beneath the lower confidence limit, 'around' for 
#' points within the confidence limit, and 'upper' for points above the upper 95% confidence limit.  
#' @param x Dataframe. The data set to be analysed. 
#' @param yr_roll Numeric. the rolling average to apply for the analysis. 
#' Defaults to 1, which is that no rolling average is used. 
#' @param interval Character Vector. The type of interval to be used for 
#' calculating distance between, defaults to 'confidence' the other option is 'prediction'. 
#' @param conf_lvl Numeric. The confidence level to calculate the confidence limits at, defaults to 0.95 for a 95% confidence interval. 
#' @param save Boolean. Whether to save objects to disk or just return locally. Defaults to TRUE. 
#' @export
classifyPtsMarkov <- function(x, yr_roll, interval, conf_lvl, prediction, save){
  
  if(missing(interval)){interval <- 'confidence'}
  if(missing(prediction)){prediction <- 'fit'}
  if(missing(save)){save <- TRUE}
  if(missing(conf_lvl)){conf_lvl <- 0.95}
  if(missing(yr_roll)){yr_roll <- 1}
  
  
  rolled <- avg(x, yr_roll)
  modr2 <- lm(roll ~ FIRE_YEAR, data = rolled)
  
  gr <- data.frame(
    # only operate on years within the rolling average.
    FIRE_YEAR =  seq(min(rolled[!is.na(rolled$roll), 'FIRE_YEAR']),
                     max(rolled$FIRE_YEAR))
  )
  
  pred_help <- function(y, lvl, gr){
    mod_pred <- data.frame(
      FIRE_YEAR = gr, 
      predict.lm(
        y, gr, interval = 'confidence', level = lvl)
    )
    return(mod_pred)
  }
  
  p <- pred_help(modr2, lvl = conf_lvl, gr) 
  p$observed <- x$TotalArea_Acre
  
  state = vector(mode = 'character', length = nrow(p))
  for (i in 1:nrow(p)){
    if(p$observed[i] <= p$fit[i]){state[i] = 'below'}  else {state[i] = 'above'}
  }
  
  # first we need to calculate the transition probabilities between the various states. 
  
  # Doing this is not super well documented in the `markovchain` package, but the 
  # function `markovchainFit` will develop transition probabilities. 
  # The input object works in pairs, essentially position 1 x[1] is state 1, and
  # position 2 x[2] is state 2, the 3rd and 4th positions refer to an entirely new 
  # set of observations. 
  
  mcFitMLE <- markovchain::markovchainFit(state, method = 'mle')
  
  # we create another s4 object which the package relies on using base R's `new` 
  # function 
  fire_chain <- new(
    "markovchain", 
    states = c('above', 'below'), 
    transitionMatrix = mcFitMLE[["estimate"]]@transitionMatrix, # access the transition matrix
    name = "Fire Chain - Relative to Regression fit")
  
  # save the original data object. 
  
   # many more years are below the fire amount than are
  # above the trend line is largely informed by a few large fires. 
  
  # we can repeat this process n times. 
  set.seed(1428)
  markov_sim <- replicate(100, expr = 
                            markovchain::markovchainSequence(
                              n = 10,  # number of years into future. 
                              markovchain = fire_chain # chain object. 
                            )
  )
  
  markov_sim <- as.data.frame(markov_sim)
  colnames(markov_sim) <- paste0('S', 1:ncol(markov_sim))
  
  return(
    list(
      chain = fire_chain, 
      steadyStates = markovchain::steadyStates(fire_chain),
      markov_simulations = markov_sim
    )
  )
  
  write.csv()
  
}


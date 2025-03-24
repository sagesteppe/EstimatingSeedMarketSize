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
#' @param w Numeric. The rolling window to use for the calculation. Defaults to 3. 
#' Defaults to 1, which is that no rolling average is used. 
#' @param interval Character Vector. The type of interval to be used for 
#' calculating distance between, defaults to 'confidence' the other option is 'prediction'. 
#' @param prediction Vector. the name of the column from the fit model to use for 
#' calculating the distance between. Defaults to 'fit', other options are: 'lwr', and 'upr'. 
#' @param conf_lvl Numeric. The confidence level to calculate the confidence limits at, defaults to 0.95 for a 95% confidence interval. 
#' @export
reportBalance <- function(x, w, interval, prediction, conf_lvl){
  
  if(missing(interval)){interval <- 'confidence'}
  if(missing(prediction)){prediction <- 'fit'}
  if(missing(conf_lvl)){conf_lvl <- 0.95}
  if(missing(w)){w <- 1}
  
  rolled <- avg(x, w = w, ...)
  rl_col <- colnames(rolled)[grep('roll_.*$', colnames(rolled))]
  form <- as.formula(paste0(rl_col, "~", 'FIRE_YEAR'))
  modr2 <- lm(form, data = rolled)
  
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
#' @param pred
#' @param resp
#' @param plot Boolean. Whether to also make and save a plot of the results. Defaults to TRUE. 
#' @param write Boolean. Whether to write results to disk or not. Defaults to TRUE. 
#' @param ret_period Numeric. If 1, the default, the full range of quantiles from 1.01 to 100 are calculated. Else a user specified range and resolution c(95, 100, 0.01), will calculate from a 95 to 100 year conditions at 0.01 resolution. 
#' @param ... Further arguments passed to ??extRemes::return.level, such as `do.ci` and `burn.in`
#' @returns a plot of intervals, a data frame of predictions from the fit model, the model, and a sample of positions from the  predictions data frame. By default these three objects are written to disk. 
#' @param export
CalculateReturnIntervals <- function(x, resp, pred, plot, write, ret_period, ...){
  
  if(missing(resp)){resp <- 'TotalArea_Acre'}; if(missing(pred)){pred <- 'FIRE_YEAR'}
  if(missing(plot)){plot <- TRUE}
  if(missing(write)){write <- TRUE}
  if(missing(ret_period)){ret_period <- 1}
  
  x <- x[!is.na(x[[resp]]),] # this removes recent no-data years - is safe and stable
  
  # this replaces the log of years without any burned area with a 0. Seems stable
  # but may have peculiar side effects? 
  logs <- log(x[[resp]])
  pos <- logs=='-Inf'
  logs <- replace(logs, pos, 0)
  
  fevd_ext <- extRemes::fevd(
    logs, 
    type = 'GEV',
    units = 'Acres', 
    span = length(logs), 
    time.units = '1/year'
  )
  
  if(length(ret_period)==1){ # 'years' for the return intervals. 
    ret_period <- seq(1.01, 100, 0.01)
  } else {
    ret_period <- seq(ret_period[1], ret_period[2], ret_period[3])
  }
  
  # note min must be > 1.000 , such as 1.0x1 etc. 
  return_ls <- extRemes::return.level(x = fevd_ext, return.period = ret_period, ...)

  return_df <- data.frame(
    Period = ret_period,
    Pr = 1/ret_period,
    CDF = ret_period/100
  )

  if(is.null(dim(return_ls))){
    return_ls <- cbind(
      return_df, 
      estimate = exp(as.numeric(return_ls))
    )
    
  } else {
    return_ls <- cbind(
      return_df, 
      lwrCI = return_ls[,1],
      uprCI = return_ls[,3],
      estimate = return_ls[,2]
    )
    cols <- c('lwrCI', 'uprCI', 'estimate')
    return_ls[,cols] <- apply(return_ls[,cols], 2, exp)
  }

  if(plot){
    ReturnIntervalsPlot(x = x, mod = fevd_ext, return_ls = return_ls)
  }

  if(write){
    write.csv(
      return_ls,
      file.path(
        '..', 'results', 'Tabular', 'FireReturnIntervals', 
        paste0('Predictions-', x$REG_NAME[1], '.csv')
      )
    )
  } else {return(return_ls)}
  
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


#' prediction help wrapper for am ordinary least squares linear model. 
pred_help <- function(y, conf_lvl, interval){
  mod_pred <- data.frame(
    FIRE_YEAR = gr, 
    predict.lm(
      y, gr, interval = interval, SE = TRUE, level = conf_lvl)
  )
  return(mod_pred)
}

#' calculate a rolling mean for fire sizes
#' 
#' @description This function wraps around two under laying data.table functions 
#' to calculate a rolling mean of a numeric vector. It implements one of two functions, 
#' either data.table::frollmean, if type = 'arithematic', or a geometric mean if 
#' type = 'geometric'. The geometric mean is calculated using spatstat.utils::harmonicmean
#' wrapped within data.table::frollapply. 
#' @param x Data.frame. A data frame of values
#' @param colname Character. A Column to calculate the values on. No default, will exit function.
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
#' @export 
avg <- function(x, colname, w, type){
  if(missing(w)){w <- 3}
  if(missing(type)){type <- 'a'}
  outname <- paste0('roll_', type, w)
  
  if(type == 'a'){
    x[[outname]] <- data.table::frollmean(x[[colname]], w)
  } else {
    x[[outname]]  <- data.table::frollapply(
      x[[colname]], w, FUN = spatstat.utils::harmonicmean)
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

#' Obtain regression parameter estimates for classifying fire years for markov chains
#' 
#' @description Wrapper function to fit a linear model and predict it onto a matrix
#' and classify the training data as being in one of three or optionally three classes. 
#' 'below' for all points beneath the lower confidence limit, 'around' for 
#' points within the confidence limit, and 'upper' for points above the upper 95% confidence limit.  
#' @param x Dataframe. The data set to be analysed. 
#' @param interval Character Vector. The type of interval to be used for 
#' calculating distance between, defaults to 'confidence' the other option is 'prediction'. 
#' @param conf_lvl Numeric. The confidence level to calculate the confidence limits at, defaults to 0.95 for a 95% confidence interval. 
#' @param save Boolean. Whether to save objects to disk or just return locally. Defaults to TRUE. 
#' @param ... Further arguments passed on to `rseedneed::avg`, colname is required. 
#' @export 
classifyPtsMarkov <- function(x, w, interval, conf_lvl, type, save, ...){
  
  if(missing(interval)){interval <- 'confidence'}
  if(missing(save)){save <- TRUE}
  if(missing(conf_lvl)){conf_lvl <- 0.95}
  
  rolled <- avg(x, ...)
  rl_col <- colnames(rolled)[grep('roll_.*$', colnames(rolled))]
  form <- as.formula(paste0(rl_col, "~", 'FIRE_YEAR'))
  modr2 <- lm(form, data = rolled)
  
  gr <- data.frame(
    # only operate on years within the rolling average.
    FIRE_YEAR =  seq(min(rolled[!is.na(rolled[rl_col]), 'FIRE_YEAR']),
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
  p$observed <- x[!is.na(rolled[rl_col]), 'TotalArea_Acre']
  
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
      REG_NAME = x$REG_NAME[1],
      chain = fire_chain, 
      steadyStates = markovchain::steadyStates(fire_chain),
      markov_simulations = markov_sim
    )
  )
  
}


#' plot a markov model 
#' 
#' @description
#' This will plot a markov model with two states, and transition and emission probabilities 
#' between the two states. The plots are not as customizable as desirable, but will give
#' OK results for sharing insights from these with colleagues. 
#' @param x the output from `classifyPtsMarkov`
#' @param node_clrs A character vector of length 2. diagrammeR supports only X11 or html colors.
#'  On occasion an X11 colors matches a base R color, but not all base R colors are X11. The first color should be for the first column of the `classifyPtsMarkov` [['steadyStates']] output, the second color for the second column. 
#' @param edge_clrs A character vector of length 4. The first element is a color from the transmission probabilities from node 1 to 2, the second from 2 to 1, the third element from 1 to 1, and the fourth from 4 to 4.
#' @param ... further arguments passed to `?DiagrammeR::export_graph` to save the plot on disk. 
#' 
#' @examples
#' node_clrs <- c('#4A001F', '#62BEC1')
#' edge_clrs <- c('#F5CDA7', '#2E0219', '#F50066', '#C5E7E8')
#' 
dia_wrapper <- function(x, node_clrs, edge_clrs, ...){
  
  if(missing(node_clrs)){node_clrs <- c()}
  if(missing(edge_clrs)){edge_clrs <- c()}
  
  ss <- round(x[['steadyStates']], 3)
  tm <- round(x[['chain']]@transitionMatrix, 3)
  
  a_graph <- DiagrammeR::create_graph() |> 
    DiagrammeR::add_node( # this is node 1 # above to above
      label = 'above',
      node_aes = DiagrammeR::node_aes(
        color = node_clrs[1],
        fillcolor = node_clrs[1],
        fontcolor = "gray50", 
        xlabel = c(ss[,1]), 
        height = ss[,1]
      )
    ) |>  
    DiagrammeR::add_node(
      label = 'below',
      node_aes = DiagrammeR::node_aes(
        color = node_clrs[2],
        fillcolor = node_clrs[2],
        fontcolor = "gray50", 
        xlabel = c(ss[,2]), 
        height = ss[,2]
      )) |> 
    DiagrammeR::add_edge(
      from = 1, to = 2, 
      edge_aes = DiagrammeR::edge_aes(
        color = edge_clrs[1],
        fontcolor =  edge_clrs[1],
        label = c(tm[1,2]),
        penwidth = tm[1,2]
      )) |>
    DiagrammeR::add_edge(
      from = 2, to = 1, 
      edge_aes = DiagrammeR::edge_aes(
        color = edge_clrs[2],
        fontcolor = edge_clrs[2],
        label = c(tm[2,1]),
        penwidth = tm[2,1]
      )) |> 
    DiagrammeR::add_edge(
      from = 1, to = 1,
      edge_aes = DiagrammeR::edge_aes(
        color = edge_clrs[3],
        fontcolor =  edge_clrs[3],
        label = c(tm[1,1]),
        penwidth = tm[1,1]
      )) |> 
    DiagrammeR::add_edge(
      from = 2, to = 2,
      edge_aes = DiagrammeR::edge_aes(
        label = c(tm[2,2]),
        penwidth = tm[2,2], 
        color = edge_clrs[4],
        fontcolor =  edge_clrs[4]
      )) 
  
  DiagrammeR::export_graph(
    a_graph, 
    ...,
    title = x[['REG_NAME']][1], 
    file_name = file.path(
      '..', 'results', 'Plots', 'MarkovChain',
      paste0(gsub(' ', '_', x[['REG_NAME']][1]), '.png'))
  )
}

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
  form.sm <- as.formula(paste0(resp, ' ~ ps(', pred, ', monotone=1)'))
  
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
    if(wts_p == 'log'){wts <- abs(rev(log(wts)))} # else do nothing to linear or none weights 
  
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
    legend = c("Median (0.5)", "0.4 & 0.6", 'Quartiles (0.25, 0.75)', '0.9 band (0.05, 0.95)', 'Conditional Mean'),
    lty = c(rev(linetypes[1:ceiling(L/2)]), 1),  
    col = c(rev(cols[1:ceiling(L/2)]), 'dodgerblue4'),
    lwd = 2, 
    bg = adjustcolor("white", 0.4)
  )  
  mtext(side=1, line=4, adj=1, cex=0.8, col = 'grey40', paste(
    if(wts_p=='exp'){'exponential decay weighted'} else 
      if (wts_p=='linear'){'linear decay weighted'} else 
        if (wts_p=='log'){'logarithmic decay weighted'} else 
          if (wts_p=='none'){'unweighted'}, 
    'quantile regression')
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




#' Fit growth chart quantile regression models to an annual time series using an auto regressive process. 
#' 
#' @description 
#' @param x Data frame. Must contain all data required to perform modelling. 
#' @param quants Numeric vector - odd. The quantiles which you want to model fits for, defaults to seq(0.05, 0.95, by = 0.025). Use to default to 0.01, but results were too 'jumpy', now we consider it best to use fewer predictions points and then using monotonic interpolation via splines ('hyman' method) to create estimates which do not have 'jumps' between predictions. 
#' @param resp Character. Name of the response field.  
#' @param pred Character. Name of the predictor field.  
#' @param write Boolean Whether to write results to disk or return them. 
#' Defaults to FALSE, if you want them returned provide filename and path to it. 
#' @examples
quantPred <- function(x, quants, resp, pred, write){
  
  if(missing(quants)){quants <- seq(0.05, 0.95, by = 0.025)}
  if(missing(resp)){resp <- 'TotalArea_Acre'}
  if(missing(pred)){pred <- 'FIRE_YEAR'}
  if(missing(write)){write <- FALSE}
  
  p <- file.path('..', 'results', 'Plots', 'QuantileForecasts',
                 paste0(gsub(' ', '_', x[['REG_NAME']][1]), '-'))
  
  form <- as.formula(paste0(resp, '~', pred))
  form.sm <- as.formula(paste0(resp, ' ~ ps(', pred, ', monotone=1)'))

  mod <- tryCatch(
    {quantregGrowth::gcrq(form.sm, data = x, tau = quants)},
    error = function(e) {return(NA)}
  )

  # we can try and jitter the values but... this hasn't worked so far... 
  if(length(mod)==1){
    mod <- tryCatch(
      {x[[resp]] <- abs(jitter(x[[resp]], amount = 0.025))
        quantregGrowth::gcrq(form.sm, data = x, tau = quants)},
      error = function(e) {return(NA)}
      )
  }
  
  # so let's simply interpolate less values of tau, and then apply linear interpolation
  # between them to get our resolution. 
  if(length(mod)==1){
    quant_sub <- seq(min(quants), max(quants), by = 0.05)
    
    mod <- tryCatch(
      {quantregGrowth::gcrq(form.sm, data = x, tau = quant_sub)},
      error = function(e) {return(NA)}
    )
  }
  
  if(length(mod)==1){
    quant_sub <- seq(min(quants), max(quants), by = 0.1)
    
    mod <- tryCatch(
      {quantregGrowth::gcrq(form.sm, data = x, tau = quant_sub)},
      error = function(e) {return(NA)}
    )
  }
  
  # soft exit for mods when nothing happens. 
  if(length(mod)==1){return(NA)}
  
  ## if using growth charts, extract the 'prediction' for the next year, although 
  # this seems to just be the max year in the data set 'drifted' out to then. I.e. 
  # the predict fn refuses to extrapolate. 
  
  # there is a way with/when/how quantreg grabs the formula from the fn... 

    mod$call$formula <- form.sm # but... we can overwrite this in the fn call to fix it.   

    preds <- data.frame(pred = max(x[[pred]])+1 )
    names(preds) <- pred
    
    preds <- predict(mod, preds) |>
      cbind(preds, 'Predicted') |>
      tibble::rownames_to_column() |>
      setNames(c('Tau', 'Prediction', pred, 'Method')) |>
      dplyr::mutate(Tau = as.numeric(Tau)) |>
      data.frame()
    
    # now we interpolate the missing values for areas where we could not
    # fit directly
    
    if(nrow(preds)!=length(quants)) {
      
      quant_need <- setdiff(quants, quant_sub)
      int_preds <- data.frame(
        approx(preds$Tau, preds$Prediction, xout = quant_need), 
        as.integer(format(Sys.Date(), "%Y")), 
        'Interpolated'
      ) |>
        setNames(c('Tau', 'Prediction', pred, 'Method'))
      
      preds <- rbind(preds, int_preds)
      preds <- preds[order(preds$Tau), ]
      
      # a strange event occurs where some values get duplicated - the project funding
      # was pulled and can't investigate root cause, but the following band-aid
      # will remove the symptom
      preds <- preds |>
        dplyr::group_by(Tau) |>
        dplyr::arrange(Method) |> # alphabetical, 'Interpolated' before 'Predicted' we want to keep predicted. 
        dplyr::slice_tail(n = 1) |>
        dplyr::ungroup() |>
        data.frame()
    }
    
    if(write){
      p <- gsub('Plots', 'Tabular', p)
      p <- paste0(p, 'allTau.csv')
      write.csv(preds, p, row.names = FALSE)
    } else {return(preds)}
    
}

#' Use this to reconcile over predictions of quantiles (or underpreds of EVT) so they can form a continuous distribution. 
#' 
#' @param qr Data frame. Results of quantile regression. 
#' @param ev Data frame. Results of Extreme Value Theory modelling. 
#' @param thresh Numeric. If values are flagged for overwriting, a buffer offset to include 'downstream' (lower Tau values) quantiles to also be overwritten. This is to try and help make predictions 'more smooth' between the GEV/EVT predictions and the upper quantile regression CDF transformed values. Defaults to 0, note 2.5% would be 0.025. 
reconcileEVT_quantiles <- function(qr, ev, thresh=0){
  
  # just for now... 
 # ev <- ev[ev$Region==qr$Region[1],]
  
  # remove any negative predictions, these cannot exist. 
  qr <- qr[qr$Prediction>0,]
  
  # ensure that all taus are equal to the previous value or ascending
  qr <- qr[!diff(qr$Prediction, 1) < 0, ]
  
  # isolate the 'lowest' extreme prediction beyond 0.95
  exts <- ev
  exts_min <- exts[which.min(exts$Tau),]

  # determine if any quantile predictions exceed this value.
  problems <- qr$Prediction > exts_min$Prediction

  # if so apply the interpolation between the two methods. 
  if(any(problems)==TRUE){
    
    # find the lowest Tau value which we will edit the predictions at. 
    toAlter <- qr[problems==TRUE,]
    
    # apply threshold 
    qr$Prediction > exts_min$Prediction
    problems <- qr$Tau > ((min(toAlter$Tau) *  (1 - thresh)))
    toAlter <- qr[problems==TRUE,]
  
    # keep all other records, and add our lowest EVT to them to form the upper bound. 
    toRefer <- dplyr::bind_rows(qr[problems==FALSE,], exts)
    toAlter$Prediction <- spline(
      x = toRefer[['Tau']],  y = toRefer[['Prediction']], xout = toAlter$Tau, method="hyman")$y
    toAlter$ReInterpolated <- TRUE
    
    # now we can add back together the pieces. 
    ret <- dplyr::bind_rows(
      exts,
      qr[problems==FALSE,],
      toAlter
    ) |>
      dplyr::arrange(Tau) 
    ret <- ret [ !diff(ret$Prediction, 1) < 0 , ] 
  
  } else {
    exts$ReInterpolated <- NA
    
    ret <- dplyr::bind_rows(
      exts,
      qr
    ) |>
      dplyr::arrange(Tau)
    ret <- ret [ !diff(ret$Prediction, 1) < 0 , ] 
  }
}

#' Simulate the total amount of burned area in the near future (=< 5 years from now)
#' 
#' @param historic Data frame. Containing observed fire characteristics from a user defined start point (e.g. introduction to a relevant paradigm) to the most recent records. 
#' @param extremes Data frame. A set of extreme value predictions, at the same resolution as `steps` to be sampled from to form the upper bounds (see `quant_bounds`) for the simulations
#' @param resp Character. The name of the response variable, e.g. total area burned. Defaults to 'TotalArea_Acre'
#' @param pred Character. The name of the predictor variable, e.g. 'Year'. Defaults to 'FIRE_YEAR'. 
#' @param years Numeric. The number of years to simulate into the future. Defaults to 3, maxes out at 5.  
#' @param steps Numeric. The resolution to forecast simulations at, Defaults to 0.001 or a thousandth; faster results may occur with 0.01 etc, but your mileage may vary on what people continuous. 
#' @param sims Numeric. The number of simulations to perform. Defaults to 100 for speed, but 1000 are quite reasonable. 
#' @param quant_bounds Numeric, length two. A lower and upper quantile bound to make predictions within. Defaults to c(0.05, 0.95)
#' @param recalc_extremes Character. Whether to recalculate the 'extremes' data set as time progresses. One of either: 'none' (the default), which will use the user supplied values as a ceiling (which may be fine for simulations of only a year or two), 'every' for recalculating the extreme values each year, 'other' for recalculating every other year (which becomes even 'years' as we count from 1), and 'median' which will recalculate the extreme values only once at the midpoint of observations (uses `round(median(1:years))`, so even years will round down.). 
BurnedAreasSimulator <- function(historic, extremes, resp, pred, years, Syear,
                                 steps, sims, quant_bounds, recalc_extremes){
  
  if(missing(resp)){resp <- 'TotalArea_Acre'}; if(missing(pred)){pred <- 'FIRE_YEAR'}
  if(missing(years)){years <- 5}; if(missing(steps)){steps <- 0.001}
  if(missing(sims)){sims <- 100}

  # identify the first year for the simulation 
  if(missing(Syear)){Syear <- max(historic[[pred]])+1}
  
  # identify any years to trigger recalculation of extreme variables. 
  if(missing(recalc_extremes)){recalc_extremes <- 'none'}
  if(recalc_extremes == 'none'){
    re <- years+1} else # just set to > 1 so never encountered. 
      if(recalc_extremes == 'every'){
        re <- seq_along(1:years)} else # equal to i so always happens. 
        if(recalc_extremes == 'other') {
          re <- which(seq_along(1:years) %% 0)} else # even years 
            if(recalc_extremes == 'median'){
              re <- round(median(seq_along(1:years))) # median year, and round to integer. 
            }
  # ADD A CONDITION TO CALCULATE XTREMES IF IT IS MISSING !!! 
  
  # subset extremes to the particular region we are focusing on . 
  extremes <- extremes[extremes$Region == historic$REG_NAME[1], ]
  
  pb <- txtProgressBar(max = sims, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  on.exit(parallel::stopCluster(cluster))
  on.exit(close(pb))
  
  cluster <- parallel::makeCluster(parallel::detectCores()/4)
  doSNOW::registerDoSNOW(cluster)
  parallel::clusterExport(
   cluster, c(
     'quantPred', 'years', 'CalculateReturnIntervals', 'reconcileEVT_quantiles', 'all_quants'))

   iters <- foreach::foreach(
     j = seq_along(1:sims), .packages = 'quantregGrowth', .options.snow = opts) %dopar% {
  
     # we will loop through the predictions and estimates...
     # from this process we will save a csv where each row is the year of the simulation
     # and each column is a different run of the simulation.
     predictions <- vector(mode = 'numeric', length = years)
     taus <- vector(mode = 'numeric', length = years)
  
      x <- historic # overwrite the last set of simulations results
      
      for (i in seq_along(1:years)){ # run X simulations for each year.
      
      # recalculate the extreme values as desired. If you don't do this then 
      # eventually they can become a 'ceiling' restricting the true growth of the
      # process if it is monotonic 
      if(any(re)==i){
        extremes <- CalculateReturnIntervals(
          x = x, resp = resp, pred = pred, ret_period = c(95, 100, 0.1),
          plot = FALSE, write = FALSE, burn.in = 1000)  |>
          
          dplyr::mutate(Tau = CDF, Method = 'Extreme') |>
          dplyr::rename(Prediction = estimate)
      }
      # model the growth chart from the beginning of the sample period, to the year-1
      # for this next prediction. 
      preds <- quantPred(x, quants = seq(0.05, 0.95, by = 0.025))
      
      #### need to kill the simulation is preds is NA ####
      if(length(preds)==1){
          predictions = rep(-999, length(predictions))
          tau = rep(-999, length(taus))
          break
      }
 
      # some of the tau estimates are more extreme than what we would expect from 
      # extreme value theory, we will bound them with the extreme value predictions. 
      # to do this we simply run linear interpolation between the highest qr pred
      # and the lowest extreme value pred. 
      reconciled <- reconcileEVT_quantiles(qr = preds, ev = extremes, thresh = 0.025)

      # note that the quantile regression methods can only deal with so many values of tau
      # otherwise errors start to occur. So we try to get a prediction at each 
      # whole quantile (e.g. integer if you will)
      # and then we have to perform linear interpolations afterwards. 
      all_q <- all_quants(reconciled)
      
      # we can now sample from this object, and fit a model to the simulated data. 
      all_q <- dplyr::mutate(all_q, LIKELIHOOD = dplyr::if_else(Tau > 0.5, 1 - Tau, Tau))
      samples <- all_q[
        sample(1:nrow(all_q), size = 1, prob = all_q$LIKELIHOOD), 
        c('Tau', 'Prediction')
        ] |>
        setNames(c('Tau', resp)) # this is our sample for year+1
      samples[pred] <- max(x[pred] + 1)
  
      # now simply bind the new prediction onto the observed data, plus the samples
      # from the previous iterations of the simulations (if current year > final year + 1)
      x <- dplyr::bind_rows(
          dplyr::select(x, dplyr::all_of(c(resp, pred))),
          samples
      )

      # finally save the output here. could be done at end, but we go as it goes. 
      predictions[i] <- x[[resp]][nrow(x)]
      taus[i] <- x[['Tau']][nrow(x)]

    }
    return(
      list(
        Predictions = predictions, 
        Tau = taus)
    )
  }
   
  close(pb)
  parallel::stopCluster(cluster)

  # every set of simulations is in two sublists, we will combine them into 
  # matrices for easy saving. An array could be a better option for certain users. 
  predictions <- matrix(unlist(lapply(iters, '[', 'Predictions')), nrow = years)
  taus <- matrix(unlist(lapply(iters, '[', 'Tau')), nrow = years)
  
  # and names these so we can keep the matrix numeric - pretty obvious but... 
  rownames(predictions) <- max(historic[[pred]]) +  seq_along(1:years)
  colnames(predictions) <- paste0('sim', seq_along(1:sims))
  rownames(taus) <- max(historic[[pred]]) + seq_along(1:years)
  colnames(taus) <-  paste0('sim', seq_along(1:sims))
  
  keeps <- apply(predictions, MARGIN = 2, sum) > 0
  predictions <- predictions[,keeps]
  taus <- taus[,keeps]
  
  # finally repack this pupper as a list of two matrices. 
  return(
    list(
      Predictions = predictions, 
      Tau = taus)
  )
}

#' Backfill the predictions at a series of fine resolutions Taus to create a continuous distribution to sample from 
#' @param x The data from `reconcileEVT_quantiles` as a list of data frames. 
all_quants <- function(x){
  
  des_res <- seq(0.0, 1.0, by = 0.0001)
  
  # ensure that all taus are equal to the previous value or ascending
  
  x <- x[!diff(x$Prediction, 1) < 0 ,]
  #return(x$Prediction)
  
  spli <- spline(
    x = c(0, x[['Tau']]),
    y = c(0, x[['Prediction']]),
    xout = des_res,
    method = "hyman"
  ) |>
    setNames(c('Tau', 'Prediction'))
  
  pos <- which(!spli$Tau %in% x$Tau)
  spli <- lapply(spli, '[', pos)
  
  dt <- data.frame(
    'Tau' = spli$Tau,
    'Prediction' = spli$Prediction,
    'Method' = 'Quantile',
    'Approach' = 'Splines'
  ) |>
    dplyr::bind_rows(x) |>
    dplyr::arrange(Tau)
  
  return(dt)
}

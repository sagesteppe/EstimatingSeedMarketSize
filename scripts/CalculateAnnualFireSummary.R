setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
source('functions.R')
library(terra)
library(tidyverse)
library(sf)

## Import all years fire perimeters ##

p2dat <- file.path('..', 'data', 'geospatial', 'FirePerimeters')
fire_perim <- st_read(
  file.path(p2dat, list.files(p2dat, pattern = 'shp$')), quiet = TRUE) |>
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) |>
  filter(FIRE_YEAR >= 1990) |> # we are mostly interested in the trend since 95, but some few year
  # rolling averages may be worth having. 
  filter(
    FEATURE_CA == 'Wildfire Final Fire Perimeter', FIRE_YEAR != 9999) |>
  select(OBJECTID, FIRE_YEAR, INCIDENT) |>
  st_transform(4326) |> # was in pseudo-mercator !!! no no 
  st_make_valid()

## Join the relevant DOI region to the fire 
p2dat <- file.path('..', 'data', 'geospatial', 'DOIRegions')
regions <- st_read(
  file.path(p2dat, list.files(p2dat, pattern = 'shp$')), quiet = TRUE) |>
  select(REG_NAME) |>
  sf::st_transform(st_crs(fire_perim))

# this join will split the fires so that they snap to region, this means that
# summary statistic, such as burned area will be based on the region rather than 
# the fire. 
fire_perim <- st_join(fire_perim, regions, join = st_intersects) |>
  drop_na()

# now we can calculate the areas burned by each fire. 
areas <- units::set_units(st_area(fire_perim), value = 'acre')
fire_perim <- mutate(fire_perim, Area = areas, .before = geometry)

rm(areas, regions)

# we want to save our cleaned up fire_perimeters, the original had those issues which 
# made it unsatisfactory for analysis. 
p2dat <- file.path('..', 'data', 'geospatial', 'FirePerimeters', 'Cleaned',
                   'InterAgencyFirePerimeterHistory_All_Years_View.shp')
st_write(fire_perim, p2dat, append = FALSE)

## we can calculate a variety of summaries as follows:
fire_perim <- group_by(fire_perim, REG_NAME, FIRE_YEAR) |>
  sf::st_drop_geometry()

firesYear <- fire_perim %>% 
  summarise(
    NoFire = n(), 
    TotalArea_Acre = sum(Area)
    ) |>
  drop_na() # three fires in tiny small wonky areas - fractions of an acre. 

# note that if a place is missing fire in a year, we need to impute that as an explicit '0'. 
# in the firesYear data set. 
fire_abs <- expand.grid(
  REG_NAME = unique(firesYear$REG_NAME), 
  FIRE_YEAR = 1990:2023, NoFire = 0, TotalArea_Acre = 0) 

# anti join will identify rows in X which are not matched in Y, if all rows in X have matches in Y
# then the anti join will produce an empty tibble as seen here. 
anti_join(firesYear, fire_abs, by  = c('REG_NAME' = 'REG_NAME', 'FIRE_YEAR' = 'FIRE_YEAR'))

# write this out as a CSV
# 'NoFires-TotalArea_byDOIRegion.csv'
firesYear <- arrange(firesYear, REG_NAME, FIRE_YEAR)
write.csv(firesYear, file.path('..', 'data', 'processed', 'NoFires-TotalArea_byDOIRegion.csv'), row.names = F)

# we will also write out the raw Fire areas
# 'FireSizes.csv'
fire_perim <- arrange(fire_perim, REG_NAME, FIRE_YEAR)
write.csv(fire_perim, file.path('..', 'data', 'processed', 'FireSizes.csv'), row.names = F)

rm(p2dat, fire_perim, firesYear)

firesYear <- read.csv(file.path('..', 'data', 'processed', 'NoFires-TotalArea_byDOIRegion.csv'))




# note that if a place is missing fire in a year, we need to impute that as an explicit '0'. 
ggplot(data = firesYear, aes(x = NoFire)) + 
  geom_density() + 
  facet_wrap(~REG_NAME, scales = 'free') 

ggplot(data = firesYear, aes(x = TotalArea_Acre)) + 
  geom_histogram() + 
  facet_wrap(~REG_NAME, scales = 'free')


fire_perim <- read.csv(file.path('..', 'data', 'processed', 'FireSizes.csv'))
# vastly underestimated the skew of the fire sizes, There are MANY very small fires
# there are relatively few large fires. 
ggplot(data = fire_perim, aes(x = Area)) + 
  geom_density() + 
  facet_wrap(~REG_NAME, scales = 'free')

ggplot(data = firesYear, aes(x = log(NoFire), y = log(TotalArea_Acre))) +
  geom_point()  + 
 # geom_smooth() + 
  geom_smooth(method = 'lm') + 
  facet_wrap(~REG_NAME, scales = 'free')

ggplot(data = firesYear, aes(x = FIRE_YEAR, y = log(TotalArea_Acre))) +
  geom_point()  + 
  geom_smooth(method = 'lm') + 
  facet_wrap(~REG_NAME, scales = 'free') 


listicle <- split(firesYear, f = firesYear$REG_NAME)
lapply(listicle, RegionalEstimates)

"A prediction interval indicates the probability of the total burned area laying between the upper and lower bounds in each year. A 95% PI indicates that only 1 in 20 years will observe a total burned area more, or less, extreme than indicated by the lines. A 90% PI indicates 1 in 10, and a 80% 1 in 5, each year the fire has an equal probabily of being smaller or larger than the model fit."




lcb <- filter(firesYear, REG_NAME == 'South Atlantic Gulf')
yr_roll <- 5

avg <- function(x, y){
  x$roll <- data.table::frollmean(x$TotalArea_Acre, y)
  return(x)
}


rolled <- avg(lcb, yr_roll)
modr2 <- lm(roll ~ FIRE_YEAR, data = rolled)

gr <- data.frame(
  # only operate on years within the rolling average.
  FIRE_YEAR =  seq(min(rolled[!is.na(rolled$roll), 'FIRE_YEAR']),
  max(rolled$FIRE_YEAR))
  )

p <- pred_help(modr2, 0.95) 
ob <- AreaDeficitSummary(p, rolled) 

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
    x = x$FIRE_YEAR,
    y = x$TotalArea_Acre,
    main = paste0('Total Area Burned - and possibly treated\n', rolled[['REG_NAME']][1]),
    xlab = 'Fire Year', 
    ylab = 'Total Area (Acre)',
    pch = 20,
    cex = 1.2,
    yaxt = "n"
  ) 
  lines(rolled[['FIRE_YEAR']], rolled[['roll']], lty = 2, col = 'grey20')
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
  
  
  mtext(side=1, line=6, adj=1, cex=0.8, status, col = 'grey40') 
  
  # denote what the lines represent. 
  legend(
    x = "topleft", 
    legend = c(paste0(yr_roll, ' year\n rolling avg.'), "Fit", "Treatable", 'Untreatable'),
    lty = c(2, rep(1, 3)),  
    col = c('grey20', 'black', 'darkgreen', 'red4'),
    lwd = 2, 
    bg = adjustcolor("white", 0.4)
  )  
  
  dev.off()
  
}



TreatableAreaPlots(x = ob, rolled, mod = p, colname = 'fit', yr_roll = yr_roll)


#' 
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
#' @param x The data set to be analysed. 
#' @param roll the rolling average to apply for the analysis. Rolling averages are used because
#' the estimates of the amount of seed required annually through them has a regression with a higher slope
#' that is they better reflect recent fire severity. 
#' @param export
reportBalance <- function(x, roll){
  
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
  
  rolled <- avg(x, roll)
  # fit the linear model to the averaged data set, the averages replacing the raw
  # fire amounts 
  mod_roll <- lm(roll ~ FIRE_YEAR, data = rolled)
  
  # create a grid which we can predict the fit model onto. 
  gr <- data.frame(
    # only operate on years within the rolling average.
    FIRE_YEAR =  seq(min(test[!is.na(test$roll3), 'FIRE_YEAR']),
                     max(test$FIRE_YEAR))
  )
  
  # we'll use the fitted values for each annual summary 
  preds_rolled <- pred_help(mod_roll, 0.95)
  
  
}

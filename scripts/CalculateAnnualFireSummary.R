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







avg <- function(x, y){
  x$roll <- data.table::frollmean(x$TotalArea_Acre, y)
  return(x)
}


rolled <- avg(lcb, 3)
modr2 <- lm(roll ~ FIRE_YEAR, data = rolled)

plot(rolled$FIRE_YEAR, rolled$TotalArea_Acre) 
lines(rolled$FIRE_YEAR, rolled$roll)

gr <- data.frame(
  # only operate on years within the rolling average.
  FIRE_YEAR =  seq(min(rolled[!is.na(rolled$roll), 'FIRE_YEAR']),
  max(test$FIRE_YEAR))
  )

pred_help <- function(y, lvl){
  mod_pred <- data.frame(
    FIRE_YEAR = gr, 
    predict.lm(
      y, gr, interval = 'confidence', level = lvl)
  )
  return(mod_pred)
}


p <- pred_help(modr2, 0.95)

#' 
#' 
#' @description Calculate the amount of seed in warehouses, and how much of an area can be treated
#' based on a fit model and observed differences. 
#' @param x output of `pred_help` 
#' @param rolled
AreaDeficitSummary <- function(x, rolled){
  
  rolled <- dplyr::left_join(dplyr::select(x, FIRE_YEAR, fit), rolled)
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



ob <- AreaDeficitSummary(p, rolled)


plot(
  x = ob$FIRE_YEAR,
  y = ob$TotalArea_Acre,
  main = 'Total Burned Area - and how much could be Treated', 
  xlab = 'Fire Year', 
  ylab = 'Total Area (Acre)'
  ) 
abline(modr2)


# If the burned area is in excess of the fit model, then place a green line 
# indicating the maximum amount of area which can be treated using the 
# warehoused seed. 

segments(
  x0 = 2023, x1 = 2023,
  y0 =  ob[which(ob$FIRE_YEAR==2023), 'fit'], 
  y1 = ob[which(ob$FIRE_YEAR==2023), 'Treatable'],
  col = "darkgreen"
  ) 

points(ob$FIRE_YEAR, ob$Treatable, pch = 4, col = 'red')







#' Calculate the amount of seed in warehouse at t0
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
  
  yname <- paste0('roll', y) # the value used for rolling the average.
  
  avg <- function(x, roll, yname){
    x[[yname]] <- data.table::frollmean(x$TotalArea_Acre, roll)
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
  
  rolled <- avg(x, roll, yname)
  # fit the linear model to the averaged data set, the averages replacing the raw
  # fire amounts 
  mod_roll <- lm(yname ~ FIRE_YEAR, data = rolled)
  
  # create a grid which we can predict the fit model onto. 
  gr <- data.frame(
    # only operate on years within the rolling average.
    FIRE_YEAR =  seq(min(test[!is.na(test$roll3), 'FIRE_YEAR']),
                     max(test$FIRE_YEAR))
  )
  
  # we'll use the fitted values for each annual summary 
  preds_rolled <- pred_help(mod_roll, 0.95)
  
  
}

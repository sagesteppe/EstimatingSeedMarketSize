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
  FIRE_YEAR = 1990:2023, NoFire = 0, 
  TotalArea_Acre = units::set_units(0, 'acre')) 

# anti join will identify rows in X which are not matched in Y, if all rows in X have matches in Y
# then the anti join will produce an empty tibble 
anti_join(fire_abs, firesYear, by  = c('REG_NAME' = 'REG_NAME', 'FIRE_YEAR' = 'FIRE_YEAR'))

firesYear <- rbind(
  firesYear, 
  anti_join(fire_abs, firesYear, by  = c('REG_NAME' = 'REG_NAME', 'FIRE_YEAR' = 'FIRE_YEAR'))
  )

# recent years likely just do not have data yes... 
firesYear <- mutate(firesYear, NoFire = if_else(FIRE_YEAR >= 2022 & NoFire==0, NA, NoFire))

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
fire_perim <- read.csv(file.path('..', 'data', 'processed', 'FireSizes.csv'))


ggplot(data = firesYear, aes(x = NoFire)) + 
  geom_density() + 
  facet_wrap(~REG_NAME, scales = 'free') 

ggplot(data = firesYear, aes(x = TotalArea_Acre)) + 
  geom_histogram() + 
  facet_wrap(~REG_NAME, scales = 'free')

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


# to run the analyses we have to remove the more recent years without mapped fire data. 

firesYear <- drop_na(firesYear)

listicle <- split(firesYear, f = firesYear$REG_NAME)
lapply(listicle, RegionalEstimates)

"A prediction interval indicates the probability of the total burned area laying between the upper and lower bounds in each year. A 95% PI indicates that only 1 in 20 years will observe a total burned area more, or less, extreme than indicated by the lines. A 90% PI indicates 1 in 10, and a 80% 1 in 5, each year the fire has an equal probabily of being smaller or larger than the model fit."



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
#' @param yr_roll Numeric. the rolling average to apply for the analysis. Defaults to 1, which is that no rolling average is used. 
#' @param interval Character Vector. The type of interval to be used for calculating distance between, defaults to 'confidence' the other option is 'prediction'. 
#' @param prediction Vector. the name of the column from the fit model to use for calculating the distance between. Defaults to 'fit', other options are: 'lwr', and 'upr'. 
#' @param conf_lvl Numeric. The confidence level to calculate the confidence limits at, defaults to 0.95 for a 95% confidence interval. 
#' @export
reportBalance <- function(x, yr_roll, interval, prediction, conf_lvl){
  
  if(missing(interval)){interval <- 'confidence'}
  if(missing(prediction)){prediction <- 'fit'}
  if(missing(conf_lvl)){conf_lvl <- 0.95}
  
  avg <- function(x, y){
    x$roll <- data.table::frollmean(x$TotalArea_Acre, y)
    return(x)
  }
  pred_help <- function(y, conf_lvl, interval){
    mod_pred <- data.frame(
      FIRE_YEAR = gr, 
      predict.lm(
        y, gr, interval = interval, SE = TRUE, level = conf_lvl)
    )
    return(mod_pred)
  }
  
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


rolled <- lapply(
  listicle['Great Lakes'], 
  reportBalance, 
  yr_roll = 2, 
  interval = 'prediction', 
  prediction = 'fit',
  conf_lvl = 0.95)

lapply(listicle, reportBalance, yr_roll = 1, conf_lvl = 0.95)
lapply(listicle, reportBalance, yr_roll = 2)
lapply(listicle, reportBalance, yr_roll = 3)
lapply(listicle, reportBalance, yr_roll = 4)
lapply(listicle, reportBalance, yr_roll = 5)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

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


fire_perim <- read.csv(file.path('..', 'data', 'processed', 'FireSizes.csv'))
# vastly underestimated the skew of the fire sizes, There are MANY very small fires
# there are relatively few large fires. 
ggplot(data = fire_perim, aes(x = Area)) + 
  geom_density() + 
  facet_wrap(~REG_NAME, scales = 'free')

################ the other fire stuff #############

p2dat <- '../data/geospatial/AnnualDisturbance/'
f <- 'LF2015_Dist_200_CONUS_20220811/LF2015_Dist_200_CONUS/Tif/LC15_Dist_200.tif'
# r <- rast(file.path(p2dat, f))

# activeCat(r) <- 2
# plot(r)

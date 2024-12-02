setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(terra)
library(tidyverse)
library(sf)

p2dat <- '../data/geospatial/AnnualDisturbance/'
f <- 'LF2015_Dist_200_CONUS_20220811/LF2015_Dist_200_CONUS/Tif/LC15_Dist_200.tif'
r <- rast(file.path(p2dat, f))

activeCat(r) <- 2
plot(r)


## Import all years fire perimeters ##

p2dat <- file.path('..', 'data', 'geospatial', 'FirePerimeters')
fire_perim <- st_read(
  file.path(p2dat, list.files(p2dat, pattern = 'shp$')), quiet = TRUE) |>
  mutate(FIRE_YEAR = as.numeric(FIRE_YEAR)) |>
  filter(FIRE_YEAR >= 1990) |> # we are mostly interested in the trend since 95, but some few year
  # rolling averages may be worth having. 
  filter(FEATURE_CA == 'Wildfire Final Fire Perimeter') |>
  select(OBJECTID, FIRE_YEAR, INCIDENT, AGENCY) |>
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
fire_perim <- st_join(fire_perim, regions, join = st_intersects)

# now we can calculate the areas burned by each fire. 
areas <- units::set_units(st_area(fire_perim), value = 'acre')
fire_perim <- mutate(fire_perim, Area = areas, .before = geometry)

rm(areas, regions)

# we want to save our cleaned up fire_perimeters, the original had those issues which 
# made it unsatisfactory for analysis. 

## we can calculate a variety of summaries as follows:
fire_perim <- group_by(fire_perim, REG_NAME, FIRE_YEAR) |>
  sf::st_drop_geometry()

firesYear <- fire_perim %>% 
  summarise(
    NoFire = n(), 
    TotalArea = sum(Area)
    )

# write this out as a CSV
# 'NoFires-TotalArea_byDOIRegion.csv'


# we will also write out the raw Fire areas
# 'FireSizes.csv'
firesYear |>
  mutate()

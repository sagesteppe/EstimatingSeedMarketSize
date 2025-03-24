# For the purposes of the survey and simulations we are interested in tabulating 
# the following values. 
# 1) The total amount of terrestrial area administered by the DOI per each DOI region 
# 2) The total amount of terrestrial area per DOI agency by each DOI region
# 3) The total amount of 

library(tidyverse)
library(terra)
library(sf)
setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
sp <- file.path('..', 'data', 'geospatial', 'NLCD')

hi <- rast(
  file.path('HI_landcover_wimperv_9-30-08_se5', 'hi_landcover_wimperv_9-30-08_se5.img'))
ak <- rast(
  file.path(p, 'NLCD_2001_Land_Cover_AK_20200724', 'NLCD_2001_Land_Cover_AK_20200724.img'))
pr <- rast(
  file.path(p, 'PR_landcover_wimperv_10-28-08_se5', 'pr_landcover_wimperv_10-28-08_se5.img'))

conus <- rast(file.path(p, 'Annual_NLCD_LndCov_2023_CU_C1V0.tif'))
plot(pr)

# get rid of the 100+ unused levels, because the data are classified as im
# each step along the 255 band components are classed. 
values <- c(31, 41:43, 52, 71, 81, 90, 95)

pr <- terra::droplevels(pr)
z <- as.numeric(pr)

rast(z) <- cover(z)

z1 <- raster::getValues(pr)

msk <- ifel(z, z %in% values, z, 0)
m <- mask(r, msk)
z1 <- raster::getValues(z)

pr1 <- ifel(pr$`Land Cover Class`, ! pr$`Land Cover Class` %in% 
       c('Barren Land', 'Deciduous Forest', 'Evergreen Forest', 
         'Mixed Forest', 'Shrub/Scrub', 'Herbaceuous', 'Hay/Pasture', 'Woody Wetlands', 
         'Emergent Herbaceuous Wetlands'), 0, pr$`Land Cover Class`, 
       filename = 'Pr.tif')

# For terrestrial surface we use the GADM, which has excellent resolution for the CONUS coastlines, islands off Alaska, and the Pacific Islands. 
# However it also draws land ownership over the Great Lakes. 
# We will use Natural Earth data to remove this coverage. 

# mask each NLCD data set to DOI land 

p2pad <- '/media/steppe/hdd/SeedMarketSizeTemplates/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb'
padus <- sf::st_read(
  dsn = p2pad, quiet = TRUE,
  layer = 'PADUS4_0Fee')  |>
  dplyr::select(Mang_Name, Own_Name, GIS_Acres, Unit_Nm)  |>
  dplyr::filter(Mang_Name %in% c('BIA', 'BLM', 'NPS', 'USFWS', 'USBR')) |>
  sf::st_cast('MULTIPOLYGON') |>
  sf::st_make_valid()

# We will convert padus to a raster data set 

p2nlcd <- '/media/steppe/hdd/SeedMarketSizeTemplates/tiles/nlcd'

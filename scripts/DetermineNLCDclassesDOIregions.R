# For the purposes of the survey and simulations we are interested in tabulating 
# the following values. 
# 1) 


library(terra)
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



# mask each NLCD data set to DOI land 


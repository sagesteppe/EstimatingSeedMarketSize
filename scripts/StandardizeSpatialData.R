setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

# These data come in a variety of formats which are not optimized for spatial analysis, but which serve
# as decent means of transmitting the data. We will convert them into analysis ready file formats here
# so that data transformations do not obfusciate the analytical operations in the reporting markdown file. 

library(terra)

### Probabilistic Wildfire Information ###

p2prob <- file.path('..', 'data', 'geospatial', 'WildfireProbability')
f <- list.files(p2prob, pattern = 'png$')

r <- terra::rast( file.path(p2prob, f[1]))
  
# terra will emit a warning that the raster has an unknown extent. 
# fortunately their are metadata for the file available at
# https://apps.fs.usda.gov/fsgisx01/rest/services/RDW_Wildfire/RMRS_ProbabilisticWildfireRisk_FLP2_HI/ImageServer
# which describe the extent, which should allow us to match it up perfectly to the domain! 

#
# Extent:
#   XMin: -375405
#   YMin: 650085
#   XMax: 236415
#   YMax: 1028355
#   Spatial Reference: 102007  (102007) 

# note that their spatial reference is an EPSG code which points to 
# https://epsg.io/102007 which is made for Hawaii... 
# However, 10200*8* is a CONUS albers projection... Much more likely this is what they 
# meant 


r1 <- terra::set.ext(
  x = r, 
  value = c(-375405,
            236415,
            650085,
            1028355)
)

ext(r)
crs(r) <- "EPSG:102008"


library(httr)
library(jsonlite)

# but we absolutely need to supply a bbox!

# 65.872086, -167.724033 # NW
# 65.647121, -167.675968 # SW
# 65.658444, -166.926151 # SE 
# 65.973492, -166.989322 # NE 

bbox="bbox=-167.724033,65.647121,65.872086,-166.926151"
bboxSR="bboxSR=4326"
size="size=800,800"
imageSR='imageSR=102006'

url <- 'https://apps.fs.usda.gov/fsgisx01/rest/services/RDW_Wildfire/RMRS_ProbabilisticWildfireRisk_FLP1_AK/ImageServer/exportImage?format=tiff'

res <- GET(paste0(url, '&', paste(bbox, bboxSR, imageSR, size, sep = '&')))
res$url
res <- content(res)


terra::rast(res)

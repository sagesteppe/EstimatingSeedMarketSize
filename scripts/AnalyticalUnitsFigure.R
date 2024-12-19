setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(sf)
library(tidyverse)
library(rmapshaper)

regions <- st_read('../data/geospatial/AdministrativeUnits/DOIRegions/DOI_12_Unified_Regions_20180801.shp', quiet = T)
l3 <- st_read('../data/geospatial/AdministrativeUnits/Ecoregions/us_eco_l3.shp', quiet = T)

###############################################################################
### Simplify the Protected areas database for cartography purposes ##
pad <- st_read(
  '../data/geospatial/AdministrativeUnits/SurfaceManagement/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb', 
  quiet = TRUE, layer = 'PADUS4_0Fee') |> 
  select(Mang_Name, Loc_Ds, Unit_Nm) |> 
  filter(Mang_Name %in% c('BLM', 'NPS', 'FWS')) |> 
  st_cast('MULTIPOLYGON')

  head(pad)
# if on parkland, or with less than 32 (gib) if that ram config exists, run piecewise 
# FWS causes crashes 

padFWS <- filter(pad, Mang_Name == 'FWS') 

pad_simp <- bind_rows(
  filter(pad, Mang_Name == 'NPS')|>
    rmapshaper::ms_simplify(keep = 0.05), 
  filter(pad, Mang_Name == 'BLM')|>
    rmapshaper::ms_simplify(keep = 0.05), 
  rmapshaper::ms_simplify(padFWS[1:650,], keep = 0.05),
  rmapshaper::ms_simplify(padFWS[651:1300,], keep = 0.05), 
  rmapshaper::ms_simplify(padFWS[1301:1955,], keep = 0.05)
) |>
  sf::st_make_valid() 

pad_simp <- pad_simp[which(st_geometry_type(pad_simp) %in% c('POLYGON', 'MULTIPOLYGON') == TRUE), ]

st_write(pad_simp, append = FALSE, 
         '../data/geospatial/AdministrativeUnits/SurfaceManagement/PAD_simp.shp')
rm(pad, padFWS)
#############################################################################

# in the pacific islands regions we can see that much of the protected areas are  
# marine sanctuaries, and other non-terrestrial surfaces. 
# We will remove those from our mapping exercises

pad_simp <- st_read(
  '../data/geospatial/AdministrativeUnits/SurfaceManagement/PAD_simp.shp')

land <- st_read(
  '../data/geospatial/TerrestrialAreas/ne_10m_land/ne_10m_land.shp')
minor_islands <- st_read(
  '../data/geospatial/TerrestrialAreas/ne_10m_minor_islands/ne_10m_minor_islands.shp') 

terrestrial <- bind_rows(minor_islands, land)  |>
  sf::st_transform(st_crs(pad_simp)) |>
  sf::st_crop(st_bbox(pad_simp)) |>
  select(featurecla) |>
  sf::st_make_valid()

pad_simp <- sf::st_intersection(terrestrial, pad_simp)

ggplot(data = pad_simp) + 
  geom_sf()

st_write(pad_simp, append = FALSE, 
         '../data/geospatial/AdministrativeUnits/SurfaceManagement/PAD_simp.shp')

rm(land, terrestrial, minor_islands)

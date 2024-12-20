setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

library(sf)
library(tidyverse)
library(rmapshaper)

public_lands_pal <- setNames(
  
  # these manually transcribed from the H-1553-Publications Standards Manual
  # Handbook - hopefully no errors.
  # [H-1553](https://www.ntc.blm.gov/krc/uploads/223/Ownership_Map_Color_Reference_Sheet.pdf)
  
  c( # colours
    rgb(254, 230, 121, max = 255), # BLM
    rgb(204, 235, 197, max = 255), # USFS
    rgb(202, 189, 220, max = 255), # NPS
    rgb(127, 204, 167, max = 255), # FWS
    rgb(255, 255, 179, max = 255), # USBR
    rgb(253, 180, 108, max = 255), # TRIB
    rgb(251, 180, 206, max = 255), # DOD
    rgb(228, 196, 159, max = 255), # OTHF
    rgb(179, 227, 238, max = 255), # SLB
    rgb(255, 255, 255, max = 255), # PVT
    rgb(143, 181, 190, max = 255) # CITY CNTY
  ), 
  
  c( # names
    'BLM', 'USFS', 'NPS', 'FWS', 'USBR', 'TRIB', 'DOD', 'OTHF', 'SLB', 'PVT', 'CITY_CNTY_SDC_SDNR_SPR')
)

regions <- st_read(
  '../data/geospatial/AdministrativeUnits/DOIRegions/DOI_12_Unified_Regions_20180801.shp', 
  quiet = T)
l3 <- st_read(
  '../data/geospatial/AdministrativeUnits/Ecoregions/us_eco_l3.shp', 
  quiet = T)

###############################################################################
### Simplify the Protected areas database for cartography purposes ##
pad <- st_read(
  '../data/geospatial/AdministrativeUnits/SurfaceManagement/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb', 
  quiet = TRUE, layer = 'PADUS4_0Fee') |> 
  select(Mang_Name, Loc_Ds, Unit_Nm) |> 
  filter(Mang_Name %in% c('BLM', 'NPS', 'FWS')) |> 
  st_cast('MULTIPOLYGON')

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
pad_simp <- sf::st_intersection(pad_simp, regions)

ggplot(data = pad_simp) + 
  geom_sf() + 
  geom_sf(data = regions, fill = NA, color = 'red') + 
  geom_sf(data = terrestrial)

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
extra_terrestrial <- st_erase(terrestrial, regions)

terrestrial <- st_intersection(terrestrial, regions)

st_write(terrestrial, append = F, 
         '../data/geospatial/TerrestrialAreas/focal_terrestrial/focal_terrestrial.shp')

st_write(extra_terrestrial, append = F,
         '../data/geospatial/TerrestrialAreas/non-us_terrestrial/non-us_terrestrial.shp')

st_write(pad_simp, append = FALSE, 
         '../data/geospatial/AdministrativeUnits/SurfaceManagement/PAD_simp.shp')

rm(land, terrestrial, minor_islands, extra_terrestrial, st_erase)

####################################
## make a simplified copy of the provisional seed transfer zones real quick too. 
pSTZ <- st_read('../data/geospatial/BowerProvisional/original/2017_New_PSZ_Labels.shp')
pSTZ <- rmapshaper::ms_simplify(pSTZ)
pSTZ <- pSTZ[which(st_geometry_type(pSTZ) %in% c('POLYGON', 'MULTIPOLYGON') == TRUE), ]

st_write(
  pSTZ, append = FALSE, 
  '../data/geospatial/BowerProvisional/simplified/BowerProvisionalPSTZ.shp'
  )

pSTZ <- st_read('../data/geospatial/BowerProvisional/simplified/BowerProvisionalPSTZ.shp')
################################################################################
### Now create figures ###
################################################################################


pad_simp <- st_read(
  '../data/geospatial/AdministrativeUnits/SurfaceManagement/PAD_simp.shp')

pad_simp <- st_intersection(regions, pad_simp)
pad_simp <- pad_simp[which(st_geometry_type(pad_simp) %in% c('POLYGON', 'MULTIPOLYGON') == TRUE), ]

terrest_nt <- st_read(
  '../data/geospatial/TerrestrialAreas/non-us_terrestrial/non-us_terrestrial.shp')
terrest <- st_read(
  '../data/geospatial/TerrestrialAreas/focal_terrestrial/focal_terrestrial.shp')

ak <- filter(regions, REG_NAME == 'Alaska') |> 
  sf::st_bbox()

ak_plot <- ggplot() + 
  geom_sf(data = terrest, fill = '#C4D4C8') + 
  geom_sf(data = pad_simp, aes(fill = Mang_Name), color = NA) + 
  scale_fill_manual(values = public_lands_pal) + 
  coord_sf(xlim = c(ak[1], ak[3]), ylim = c(ak[2], ak[4])) + 
  theme_void() + 
  theme(
    legend.position = 'none'
  )

rm(ak)

pi <- filter(regions, REG_NAME == 'Pacific Islands') |> 
  sf::st_bbox()

pi_plot <- ggplot() + 
  geom_sf(data = terrest, fill = '#C4D4C8') + 
  geom_sf(data = pad_simp, aes(fill = Mang_Name), color = NA) + 
  scale_fill_manual(values = public_lands_pal) + 
  coord_sf(xlim = c(pi[1], pi[3]), ylim = c(pi[2], pi[4])) + 
  theme_void() + 
  theme(
    legend.position = 'none'
  )

lks <- rnaturalearth::ne_download( 
  type = "lakes",  
  category = "physical", 
  scale = "small") |> 
  select(name) |>
  sf::st_make_valid() |>
  sf::st_transform(st_crs(regions))

lks <- lks [ lengths(st_intersects(lks, regions)) > 0, ]

conus_reg <- filter(regions, ! REG_NAME %in% c('Alaska', 'Pacific Islands')) |> 
  sf::st_bbox()

conus_plot <- ggplot() + 
  geom_sf(data = terrest, fill = '#C4D4C8') + 
#  geom_sf(data = terrest_nt, fill = '#837569') + 
  geom_sf(data = lks, fill = '#2D728F') + 
  
  geom_sf(data = pad_simp, aes(fill = Mang_Name), color = NA) + 
  geom_sf(data = regions, color = '#ED254E', fill = NA) + 
  scale_fill_manual(values = public_lands_pal) + 
  coord_sf(
    xlim = c(conus_reg[1]-10000, conus_reg[3]+10000), 
    ylim = c(conus_reg[2], conus_reg[4])
    ) + 
  theme_void() + 
  theme(
    legend.position = 'none'
  ) 


cowplot::ggdraw() + 
  cowplot::draw_plot(conus_plot) + 
  cowplot::draw_plot(pi_plot, x = 0.8, y = -0.2, width = 0.25, height = 0.25) + 
  cowplot::draw_plot(ak_plot, x = 0.2, y = -0.175, width = 0.5, height = 0.5) 

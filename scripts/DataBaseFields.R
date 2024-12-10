species_longevity_tab <- data.frame(
  'PLANTS_CODE' = as.character(), # USDA Plants code at time of DB establishment- can be updated in overhauls. 

  'long_days' = as.integer(), # User converts input reported data to number of days
  'prop_viable' = as.double(), # the proportion viable reported as: 0.77
  'long_resolution' = as.integer(), # the resolution of measurements in day, if measured every 6 months, ~182
  'temperature' = as.integer(),  # the temperature in Celsius to the closest whole number. 
  'RH' = as.double(), # the relative humidity as a proportion: 0.2
  
  'data_source' = as.character(), # the source for data, if recent publication DOI preferred, else Lead author year (minimum)
  'method' = as.character(), # method of measuring viability 
  'comment' = as.character() # any analyst comments on the data set. 
) 

species_PLS_tab <- data.frame(
  'PLANTS_CODE' = as.character(), # the current USDA Plants code - can be updated in overhauls. 
  'PLS-LB' = as.character(), # the number of seeds per lb
  'PLS-LB_source' =  as.character(), # the source for data, if recent publication DOI preferred, else Lead author year (minimum)
  'PLS-LB_source-method' =  as.character(), # method, assumed 'weighed'
  'PLS-LB_source-comment' =  as.character() # any comments on method  
) 

species_taxonomy_tab <- data.frame(
  'PLANTS_CODE' = as.character(), # the current USDA Plants code - can be updated in overhauls.
  'PLANTS_NAME' = as.character(), # currently accepted USDA plants name  - can be updated in overhauls.
  'POW_NAME' = as.character(), # currently accepted Plants of the World plants name  - can be updated in overhauls.
  'YEAR_POPULATED' = as.numeric(), # the year these values were acquired
  'MONTH_POPULATED' = as.numeric() # the number month of the year these values were acquired. 
) 

coverclass_tab <- data.frame(
  'DOI_REG' = as.integer(), # the DOI region of analysis, as it's number
  'NLCD_No' = as.integer(), # the NLCD number for the cover type 
  'agency' = as.character(), # the particular agencies surface administrative area 
  'year' = as.integer(), # the year of the measurements
  'area' = as.numeric(), # the total area of the cover class on agencies land, in acres
  'slope_gr_10' = as.numeric(), # total area of CC on agencies land, where slope >= 10* in acres, 
  'slope_le_10' = as.numeric(),  # total area of CC on agencies land, where slope < 10* in acres, 
  'area_burned' = as.numeric(),  # the total area of the cover class on agencies land, in acres which burned. 
  'slope_gr_10_burned' = as.numeric(), # total area of CC on agencies land, where slope >= 10* in acres which burned. 
  'slope_le_10_burned' = as.numeric()  # total area of CC on agencies land, where slope < 10* in acres which burned. 
) 

# this table showcases the estimated ACTUAL treatment rates applied by a practitioner over a time range 
restoration_realized_tab <- data.frame(
  'DOI_REG' = as.numeric(), # the doi region treatments occurred in 
  'agency' = as.character(), # the agency administering the treatments 
  'NLCD_No' = as.integer(), # the NLCD the treatment occurs in 
  'year_end' = as.integer(), # when the practitioner started doing treatments 
  'year_start' = as.integer(), # when the practitioner stopped doing treatments / survey year
  'method' = as.character(), # method one of "Drill", "Aerial", "Broadcast", "Hydro"
  'PLS_acre' = as.numeric(), # PLS per acre applied 
  'PLS_m2'= as.numeric() # PLS per m2 applied 
) 

# this table showcases what a practitioner would require to meet land health standards or management objectives. 
restoration_needed_tab <- data.frame(
  'DOI_REG' = as.numeric(), # the doi region treatments occurred in 
  'agency' = as.character(), # the agency administering the treatments 
  'NLCD_No' = as.integer(), # the NLCD the treatment occurs in 
  'method' = as.character(), # method one of "Drill", "Aerial", "Broadcast", "Hydro"
  'PLS_acre' = as.numeric(), # PLS per acre which would need to be applied to achieve goals 
  'PLS_m2'= as.numeric() # PLS per m2 which would need to be applied to achieve goals 
)


seedmix_species_tab <- data.frame(
  
)







lifeform_realized_tab <- data.frame(
  'pctGraminoid' = as.numeric(), 
  'pctForb' = as.numeric(), 
  'pctShrub' = as.numeric(), 
  'pctTree' = as.numeric()
)

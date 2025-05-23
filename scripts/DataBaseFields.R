setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')

species_longevity <- data.frame(
  'PLANTS_code' = as.character(), # USDA Plants code at time of DB establishment- can be updated in overhauls. 

  'longevity_days' = as.integer(), # User converts input reported data to number of days
  'prop_viable' = as.double(), # the proportion viable reported as: 0.77
  'longevity_resolution' = as.integer(), # the resolution of measurements in day, if measured every 6 months, ~182
  'temperature' = as.integer(),  # the temperature in Celsius to the closest whole number. 
  'relative_humidity' = as.double(), # the relative humidity as a proportion: 0.2
  
  'data_source' = as.character(), # the source for data, if recent publication DOI preferred, else Lead author year (minimum)
  'method' = as.character(), # method of measuring viability 
  'comment' = as.character() # any analyst comments on the data set. 
) 

species_PLS <- data.frame(
  'PLANTS_code' = as.character(), # the current USDA Plants code - can be updated in overhauls. 
  'PLS_per_lb' = as.character(), # the number of seeds per lb
  'PLS_per_lb_source' =  as.character(), # the source for data, if recent publication DOI preferred, else Lead author year (minimum)
  'PLS_per_lb_source_method' =  as.character(), # method, assumed 'weighed'
  'PLS_per_lb_source_comment' =  as.character() # any comments on method  
) 

species_taxonomy <- data.frame(
  'PLANTS_code' = as.character(), # the current USDA Plants code - can be updated in overhauls.
  'PLANTS_name' = as.character(), # currently accepted USDA plants name  - can be updated in overhauls.
  # note genus and family not provided for PLANTS, because of reluctance to fully embrace a phylogenetic perspective. 
  'POW_name' = as.character(), # currently accepted Plants of the World plants name  - can be updated in overhauls.
  'POW_genus' = as.character(), # currently accepted Plants of the World plants name  - can be updated in overhauls.
  'POW_family' = as.character(), # currently accepted Plants of the World plants name  - can be updated in overhauls.
  'year_populated' = as.numeric(), # the year these values were acquired
  'month_populated' = as.numeric() # the number month of the year these values were acquired. 
) 

administration <- data.frame(
  'region_agency' = as.integer(), 
  'agency' = as.character(), 
  'DOI_region' = as.integer(), # the DOI region of analysis, as it's number
  'NLCD_no' = as.integer(), # the NLCD number for the cover type 
  'agency_name' = as.character(), # the land management agency. 
  'agency_area' = as.character() # the particular agencies surface administrative area 
)

cover_class <- data.frame(
  'NLCD_no' = as.integer(),  # JOIN KEY
  'year' = as.integer(), # the year of the measurements
  'area' = as.numeric(), # the total area of the cover class on agencies land, in acres
  'slope_greater_10' = as.numeric(), # total area of CC on agencies land, where slope >= 10* in acres, 
  'slope_less_10' = as.numeric(),  # total area of CC on agencies land, where slope < 10* in acres, 
  'area_burned' = as.numeric(),  # the total area of the cover class on agencies land, in acres which burned. 
  'slope_greater_10_burned' = as.numeric(), # total area of CC on agencies land, where slope >= 10* in acres which burned. 
  'slope_less_10_burned' = as.numeric()  # total area of CC on agencies land, where slope < 10* in acres which burned. 
) 

# this table showcases the estimated ACTUAL treatment rates applied by a practitioner over a time range 
restoration_realized <- data.frame(
  'NLCD_no' = as.integer(),  # JOIN KEY
  'year_end' = as.integer(), # when the practitioner started doing treatments 
  'year_start' = as.integer(), # when the practitioner stopped doing treatments / survey year
  'method' = as.character(), # method one of "Drill", "Aerial", "Broadcast", "Hydro"
  'PLS_per_acre' = as.numeric(), # PLS per acre applied 
  'PLS_per_m2'= as.numeric() # PLS per m2 applied 
) 

# this table showcases what a practitioner would require to meet land health standards or management objectives. 
restoration_needed <- data.frame(
  'NLCD_no' = as.integer(),  # JOIN KEY
  'method' = as.character(), # method one of "Drill", "Aerial", "Broadcast", "Hydro"
  'PLS_per_acre' = as.numeric(), # PLS per acre which would need to be applied to achieve goals 
  'PLS_per_m2'= as.numeric() # PLS per m2 which would need to be applied to achieve goals 
)

species_seedmix_realized <- data.frame(
  'NLCD_no' = as.integer(), # JOIN KEY 
  'PLANTS_code' = as.character(), # JOIN KEY 
  'pct_in_mix_avg' = as.integer(), # When used, on average, how much of a seed mix contains this species? 
  'pct_in_mix_min' = as.integer(), # When used, what's the smallest amount, of this a seed mix contains? 
  'pct_in_mix_max' = as.integer(), # When used, what's the largest amount, of this a seed mix contains? 
  'pct_area_treated' = as.integer() # what proportion of area get's treated by this species? 
)

species_seedmix_needed <- data.frame(
  'NLCD_no' = as.integer(), # JOIN KEY 
  'PLANTS_code' = as.character(), # JOIN KEY 
  'pct_in_mix_avg' = as.integer(), # Ideally, on average, how much of a seed mix contains this species? 
  'pct_in_mix_min' = as.integer(), # Ideally, what's the smallest amount, of this a seed mix contains? 
  'pct_in_mix_max' = as.integer(), # Ideally, what's the largest amount, of this a seed mix contains? 
  'pct_area_treated' = as.integer() # Ideally what proportion of area get's treated by this species? 
)

lifeform_realized <- data.frame( 
  'NLCD_no' = as.integer(), # JOIN KEY 
  'pct_graminoid_avg' = as.integer(), # When used, on average, how much of a seed mix contains this lifeform? 
  'pct_forb_avg' = as.integer(), 
  'pct_shrub_avg' = as.integer(), 
  'pct_tree_avg' = as.integer(),
  
  'pct_graminoid_min' = as.integer(), # When used, on average, what's the minimum amount of a seed mix contains this lifeform? 
  'pct_forb_min' = as.integer(), 
  'pct_shrub_min' = as.integer(), 
  'pct_tree_min' = as.integer(),
  
  'pct_graminoid_max' = as.integer(),  # When used, on average, what's the maximum amount of a seed mix contains this lifeform? 
  'pct_forb_max' = as.integer(), 
  'pct_shrub_max' = as.integer(), 
  'pct_tree_max' = as.integer()
  
)

lifeform_needed <- data.frame(
  'NLCD_no' = as.integer(), 
  
  'pct_graminoid_avg' = as.integer(), 
  'pct_forb_avg' = as.integer(), 
  'pct_shrub_avg' = as.integer(), 
  'pct_tree_avg' = as.integer(),
  
  'pct_graminoid_min' = as.integer(), 
  'pct_forb_min' = as.integer(), 
  'pct_shrub_min' = as.integer(), 
  'pct_tree_min' = as.integer(),
  
  'pct_graminoid_max' = as.integer(), 
  'pct_forb_max' = as.integer(), 
  'pct_shrub_max' = as.integer(), 
  'pct_tree_max' = as.integer() 
  
)

respondent <- data.frame(
  'RespondentID' = as.numeric(), # unique identifier for each respondent - job combination. 
  'region_agency' = as.numeric(), # combined internally
  'agency' = as.character(),  # agency abbreviation 
  'DOI_region' = as.integer(), # DOI region
  'name_first' = as.character(), # respondents first name
  'name_last' = as.character(), # last name
  'office' = as.character(), # office name
  'email'= as.character(),  # email for contact/clarification 
  'position_start' = as.integer(),  # when they started the position they will be reporting for
  'position_end' = as.integer() # optionally the year they left, especially if filling out info based on a previous job. 
)

library(dm)

all_tables <- Filter(function(x) is(x, "data.frame"), mget(ls()))
short_items <- c(
  'respondent', 'administration', 'cover_class', 'restoration_needed', 'restoration_realized',
  'lifeform_needed', 'lifeform_realized')
short_term <- all_tables[short_items]

med_items <- c(
  'respondent', 'administration', 'cover_class', 'restoration_needed', 'restoration_realized',
  'species_seedmix_needed', 'species_seedmix_realized', 'species_PLS')
medium_term <- all_tables[med_items]

rm(list=setdiff(ls(), c("short_term", 'medium_term')))

short_term_dm <- as_dm(short_term)
short_term_dm <- short_term_dm %>% 
  dm_add_pk(respondent, RespondentID) %>% 
  dm_add_pk(administration, region_agency) %>% 
  dm_add_pk(cover_class, NLCD_no) %>% 
  dm_add_pk(restoration_realized, NLCD_no) %>% 
  dm_add_pk(restoration_needed, NLCD_no) %>% 
  
  dm_add_fk(
    table = respondent, columns = RespondentID, ref_table = administration) %>% 
  dm_add_fk(
    table = administration, columns = region_agency, ref_table = cover_class) %>% 
  dm_add_fk(
    table = cover_class, columns = NLCD_no, ref_table = restoration_realized) %>% 
  dm_add_fk(
    table = cover_class, columns = NLCD_no, ref_table = restoration_needed) %>% 
  
  # just short term stuff below 
  dm_add_pk(lifeform_realized, NLCD_no) %>% 
  dm_add_pk(lifeform_needed, NLCD_no) %>% 

  dm_add_fk(
    table = restoration_realized, columns = NLCD_no, ref_table = lifeform_realized) %>% 
  dm_add_fk(
    table = restoration_needed, columns = NLCD_no, ref_table = lifeform_needed) 
  
cols <- setNames(
  names(short_term), c('#755C1B', '#83BCA9', '#F76F8E', rep('#5E5C6C',2), rep('#0B5563', 2))
) 

par(bg = 'red')

sh_dm_plot <- short_term_dm %>%
  dm_set_colors(!!!cols) %>%
  dm_draw(rankdir = "LR", view_type = "all", columnArrows = FALSE)

svg <- DiagrammeRsvg::export_svg(sh_dm_plot)
htmltools::html_print(htmltools::HTML(svg))

htmltools::html_print(
  html = sh_dm_plot,
  background = "transparent", 
  viewer = '../PitchTalk/images/ShortTermERD.html')

### plot two distinct graphs - one for short term - one for medium term . 

medium_term_dm <- as_dm(medium_term)
medium_term_dm <- medium_term_dm %>% 
  dm_add_pk(respondent, RespondentID) %>% 
  dm_add_pk(administration, region_agency) %>% 
  dm_add_pk(cover_class, NLCD_no) %>% 
  dm_add_pk(restoration_realized, NLCD_no) %>% 
  dm_add_pk(restoration_needed, NLCD_no) %>% 
  dm_add_pk(species_PLS, PLANTS_code) %>% 
  
  dm_add_fk(
    table = respondent, columns = RespondentID, ref_table = administration) %>% 
  dm_add_fk(
    table = administration, columns = region_agency, ref_table = cover_class) %>% 
  dm_add_fk(
    table = cover_class, columns = NLCD_no, ref_table = restoration_realized) %>% 
  dm_add_fk(
    table = cover_class, columns = NLCD_no, ref_table = restoration_needed) %>% 

  # just medium term stuff below
  dm_add_pk(species_seedmix_needed, NLCD_no) %>% 
  dm_add_pk(species_seedmix_realized, NLCD_no) %>% 
  
  dm_add_fk(
    table = restoration_realized, columns = NLCD_no, ref_table = species_seedmix_realized) %>% 
  dm_add_fk(
      table = restoration_needed, columns = NLCD_no, ref_table = species_seedmix_needed) %>% 
  
  dm_add_fk(
    table = species_seedmix_needed, columns = PLANTS_code, ref_table = species_PLS
  ) %>% 
  dm_add_fk(
    table = species_seedmix_realized, columns = PLANTS_code, ref_table = species_PLS
  ) 

cols <- setNames(
  names(medium_term), c('#755C1B', '#83BCA9', '#F76F8E', rep('#5E5C6C', 2), rep('#0B5563', 2), '#89023E')
)  

mt_dm_plot <- medium_term_dm %>%
  dm_set_colors(!!!cols) %>%
  dm_draw( rankdir = "LR", view_type = "all", columnArrows = FALSE)

htmltools::html_print(background = "transparent")
saveRDS(mt_dm_plot, '../PitchTalk/images/MediumTermERD.rda')










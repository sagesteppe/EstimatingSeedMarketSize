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

ggplot(data = firesYear, aes(x = TotalArea_Acre)) + 
  geom_histogram() + 
  facet_wrap(~REG_NAME, scales = 'free')


fire_perim <- read.csv(file.path('..', 'data', 'processed', 'FireSizes.csv'))
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





mse <- function(x){mean(x$residuals^2)}

mb <- filter(firesYear, REG_NAME == 'Columbia-Pacific Northwest') 


mod <- lm(log(TotalArea_Acre) ~ FIRE_YEAR, data = mb)
summary(mod)
mse(mod)

gr <- data.frame(FIRE_YEAR =  seq(min(mb$FIRE_YEAR), max(mb$FIRE_YEAR)+2))
mod_pred95 <- data.frame(
  FIRE_YEAR = gr, 
    predict.lm(
  mod, gr, SE=TRUE, interval = 'prediction', level = 0.95)
)

mod_pred90 <- data.frame(
  FIRE_YEAR = gr, 
  predict.lm(
    mod, gr, SE=TRUE, interval = 'prediction', level = 0.90)
)

mod_pred80 <- data.frame(
  FIRE_YEAR = gr, 
  predict.lm(
    mod, gr, SE=TRUE, interval = 'prediction', level = 0.80)
)

mod_ci <-  data.frame(
  FIRE_YEAR = gr, 
  predict.lm(
    mod, gr, SE=TRUE, interval = 'confidence', level = 0.95)
)

plot(
  mb$FIRE_YEAR,
  log(mb$TotalArea_Acre), 
  xlab = 'Year', 
  ylab = 'Area Burned (log)',
  xlim = c(min(gr$FIRE_YEAR), max(gr$FIRE_YEAR)), 
  ylim = c(min(mod_pred95$lwr), max(mod_pred95$upr)), 
  main = 'Observed and Predicted', 
  xaxs = 'i', 
  yaxs = 'i'
)


polygon(
  x = c(
    max(mb$FIRE_YEAR)+1, max(mb$FIRE_YEAR)+1,  max(gr$FIRE_YEAR),  max(gr$FIRE_YEAR)),
  y = c(
    min(mod_pred95$lwr), max(mod_pred95$upr), max(mod_pred95$upr), min(mod_pred95$lwr)), 
  col = adjustcolor("orange", 0.3), border = 'orange'
)

lines(gr$FIRE_YEAR, mod_pred95$fit)

## CI 
lines(gr$FIRE_YEAR, mod_ci$lwr, lty = 5)
lines(gr$FIRE_YEAR, mod_ci$upr, lty = 5)

## prediction intervals 
lines(gr$FIRE_YEAR, mod_pred95$lwr, lty = 3, col = 'grey60')
lines(gr$FIRE_YEAR, mod_pred95$upr, lty = 3, col = 'grey60')

lines(gr$FIRE_YEAR, mod_pred90$lwr, lty = 3, col = 'grey40')
lines(gr$FIRE_YEAR, mod_pred90$upr, lty = 3, col = 'grey40')

lines(gr$FIRE_YEAR, mod_pred80$lwr, lty = 3, col = 'grey20')
lines(gr$FIRE_YEAR, mod_pred80$upr, lty = 3, col = 'grey20')

legend(
  x = "topleft", 
  legend = c("Fit", "95% CI", '80% PI', '90% PI', '95% PI'),
  lty = c(1, 5, 3, 3, 3),  
  col = c('black', 'black', 'grey20',  'grey40', 'grey60'),
  lwd = 2, 
  bg = adjustcolor("white", 0.4)
  )      

mod$coefficients[2] # if positive we have an increase 
sm <- summary(mod)

SupportWriter <- function(x){
  
  am <- anova(x)
  x1 <- am$`Pr(>F)`[1]
  
  if(x1 < -1e-3){y <- 'very strong'} else if(
    x1 < 0.01) {y <- 'strong'} else if(
      x1 < 0.05) {y <- 'moderate'} else if(
        x1 < 0.1) {y <- 'weak'} else 
        {y <- 'little or no'}
  
  return(y)
}



## Alaska 'staying about the same'
## Great Lakes - 'slightly increasing'
## Pacific Islands ??? one of the two above...
## North Atlantic-Appalachian 
## all others "Increasing" 

paste(
  'There is', SupportWriter(mod), 'evidence that the total area burned in this area is', ,  'each year.'
)


"A prediction interval indicates the probability of the total burned area laying between the upper and lower bounds in each year. A 95% PI indicates that only 1 in 20 years will observe a total burned area more, or less, extreme than indicated by the lines. A 90% PI indicates 1 in 10, and a 80% 1 in 5, each year the fire has an equal probabily of being smaller or larger than the model fit."














boot(
  fy$TotalArea_Acre, statistic = mean,
  stype = 'i',
  R = 1000, 
  parallel = "multicore",
)

data <- data.frame(xs = rnorm(15, 2))

library(boot)
meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}

bo <- boot(fy[, "TotalArea_Acre", drop = FALSE], statistic=meanfun, R=5000)

ci80 <- boot.ci(bo, conf=0.8, type="bca")
ci90 <- boot.ci(bo, conf=0.9, type="bca")
ci95 <- boot.ci(bo, conf=0.95, type="bca")



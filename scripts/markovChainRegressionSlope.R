library(markovchain)
library(tidyverse)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
source('functions.R')

annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')
annual <- filter(annual, FIRE_YEAR  > 1998)
ann_cgb <- filter(annual, REG_NAME == 'Great Lakes')

x <- classifyPtsMarkov(ann_cgb)

library(data.table)
d = as.data.table(list(1:6/2, 3:8/4))
# rollmean of single vector and single window


frollapply(d[, V1], 3, FUN = spatstat.utils::harmonicmean)
frollmean(d[, V1], 3)

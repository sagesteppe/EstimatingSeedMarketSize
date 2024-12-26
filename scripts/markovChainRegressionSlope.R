library(markovchain)
library(tidyverse)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
source('functions.R')

annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')
annual <- filter(annual, FIRE_YEAR  > 1998)
ann_cgb <- filter(annual, REG_NAME == 'Great Lakes')
annual <- split(annual, f = annual$REG_NAME)

x <- classifyPtsMarkov(ann_cgb, colname = 'TotalArea_Acre', w = 1, type = 'g')
output <- lapply(annual, classifyPtsMarkov, w = 1, colname = 'TotalArea_Acre' )
 
dia_wrapper(
  output$Alaska, node_clrs = node_clrs, edge_clrs = edge_clrs,
  title = 'Alaska',
  file_name = 'test.svg', file_type = 'svg', width = 400, height = 140)

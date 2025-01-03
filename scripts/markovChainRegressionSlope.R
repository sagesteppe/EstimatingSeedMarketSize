library(markovchain)
library(tidyverse)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
source('functions.R')

annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')
annual <- filter(annual, FIRE_YEAR  > 1998)
annual <- split(annual, f = annual$REG_NAME)

output <- lapply(annual, classifyPtsMarkov, w = 1, colname = 'TotalArea_Acre' )

node_clrs <- c('#4A001F', '#62BEC1')
edge_clrs <- c('#F5CDA7', '#2E0219', '#F50066', '#C5E7E8')

lapply(output, 
       dia_wrapper, node_clrs = node_clrs, edge_clrs = edge_clrs,
         file_type = 'PNG', width = 400, height = 140)

png_files <- list.files(
  '../results/Plots/MarkovChain', 
  pattern = "png$", full.names = TRUE
)

gifski::gifski(
  png_files,
  gif_file = '~/Documents/assoRted/SeedMarketSizingTalk/images/MarkovChains.gif', 
  delay = 2)

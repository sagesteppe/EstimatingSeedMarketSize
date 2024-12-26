library(markovchain)
library(tidyverse)
library(DiagrammeR)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
source('functions.R')

annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')
annual <- filter(annual, FIRE_YEAR  > 1998)
ann_cgb <- filter(annual, REG_NAME == 'Great Lakes')
annual <- split(annual, f = annual$REG_NAME)

x <- classifyPtsMarkov(ann_cgb, colname = 'TotalArea_Acre', w = 1, type = 'g')
output <- lapply(annual, classifyPtsMarkov, w = 1, colname = 'TotalArea_Acre' )

# in each object, 2 will be below, 1 will be above. 

ss <- round(output$Alaska[['steadyStates']], 3)
tm <- round(output$Alaska[['chain']]@transitionMatrix, 3)


a_graph <- create_graph() |> 
  add_node( # this is node 1 # above to above
    node_aes = node_aes(
      color = "#4A001F",
      fillcolor = "#4A001F",
      fontcolor = "gray85", 
      xlabel = 'above', 
      height = ss[,1]
    )
  ) |>  
  add_node(
    node_aes = node_aes(
      color = "#62BEC1",
      fillcolor = "#62BEC1",
      fontcolor = "gray85", 
      xlabel = 'below', 
      label = ss[,2], 
      height = ss[,2]
    )) |> 
  add_edge(
    from = 1, to = 2, 
    edge_aes = node_aes(
      color = "#F5CDA7",
      penwidth = tm[1,2]
    )) |>
  add_edge(
    from = 2, to = 1, 
    edge_aes = edge_aes(
      color = "#2E0219",
      penwidth = tm[2,1]
    )) |> 
  add_edge(
    from = 1, to = 1,
    edge_aes = edge_aes(
      color = '#F50066',
      penwidth = tm[1,1]
    )) |>
  add_edge(
    from = 2, to = 2,
    edge_aes = edge_aes(
      penwidth = tm[2,2], 
      color = '#C5E7E8'
    )) 

render_graph(a_graph)


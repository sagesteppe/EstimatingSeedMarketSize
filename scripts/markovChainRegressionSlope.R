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

# in each object, 2 will be below, 1 will be above. 


#' plot a markov model 
#' 
#' @description
#' This will plot a markov model with two states, and transition and emission probabilities 
#' between the two states. The plots are not as customizable as desirable, but will give
#' OK results for sharing insights from these with colleagues. 
#' @param x the output from `classifyPtsMarkov`
#' @param node_clrs A character vector of length 2. diagrammeR supports only X11 or html colors.
#'  On occasion an X11 colors matches a base R color, but not all base R colors are X11. The first color should be for the first column of the `classifyPtsMarkov` [['steadyStates']] output, the second color for the second column. 
#' @param edge_clrs A character vector of length 4. The first element is a color from the transmission probabilities from node 1 to 2, the second from 2 to 1, the third element from 1 to 1, and the fourth from 4 to 4.
#' @param path a location to save the plot to. 
#' @examples
#' node_clrs <- c('#4A001F', '#62BEC1')
#' edge_clrs <- c('#F5CDA7', '#2E0219', '#F50066', '#C5E7E8')
#' 
dia_wrapper <- function(x, node_clrs, edge_clrs){
  
  if(missing(node_clrs)){node_clrs <- c()}
  if(missing(edge_clrs)){edge_clrs <- c()}
  
  ss <- round(x[['steadyStates']], 3)
  tm <- round(x[['chain']]@transitionMatrix, 3)
  
  a_graph <- DiagrammeR::create_graph() |> 
    DiagrammeR::add_node( # this is node 1 # above to above
      label = 'above',
      node_aes = node_aes(
        color = node_clrs[1],
        fillcolor = node_clrs[1],
        fontcolor = "gray50", 
        xlabel = c(ss[,1]), 
        height = ss[,1]
      )
    ) |>  
    DiagrammeR::add_node(
      label = 'below',
      node_aes = node_aes(
        color = node_clrs[2],
        fillcolor = node_clrs[2],
        fontcolor = "gray50", 
        xlabel = c(ss[,2]), 
        height = ss[,2]
      )) |> 
    DiagrammeR::add_edge(
      from = 1, to = 2, 
      edge_aes = edge_aes(
        color = edge_clrs[1],
        fontcolor =  edge_clrs[1],
        label = c(tm[1,2]),
        penwidth = tm[1,2]
      )) |>
    DiagrammeR::add_edge(
      from = 2, to = 1, 
      edge_aes = edge_aes(
        color = edge_clrs[2],
        fontcolor = edge_clrs[2],
        label = c(tm[2,1]),
        penwidth = tm[2,1]
      )) |> 
    DiagrammeR::add_edge(
      from = 1, to = 1,
      edge_aes = edge_aes(
        color = edge_clrs[3],
        fontcolor =  edge_clrs[3],
        label = c(tm[1,1]),
        penwidth = tm[1,1]
      )) |> 
    DiagrammeR::add_edge(
      from = 2, to = 2,
      edge_aes = edge_aes(
        label = c(tm[2,2]),
        penwidth = tm[2,2], 
        color = edge_clrs[4],
        fontcolor =  edge_clrs[4]
      )) 
  
  return(a_graph)
 # DiagrammeR::render_graph(a_graph, title = 'place')
  
}

ob <- dia_wrapper(output$Alaska, node_clrs = node_clrs, edge_clrs = edge_clrs)



mylist <- list(foo1=1:10,foo2=11:20)
imap(mylist, function(x, y) mean(x))

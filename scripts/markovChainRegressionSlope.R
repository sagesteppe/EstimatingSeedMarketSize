library(markovchain)
library(tidyverse)

setwd('~/Documents/assoRted/EstimatingSeedMarketSize/scripts')
source('functions.R')

annual <- read.csv('../data/processed/NoFires-TotalArea_byDOIRegion.csv')
annual <- filter(annual, FIRE_YEAR  > 1998)
ann_cgb <- filter(annual, REG_NAME == 'Columbia-Pacific Northwest')

x <- classifyPtsMarkov(ann_cgb)
state = vector(mode = 'character', length = nrow(x))
for (i in 1:nrow(x)){
  
  if(x$observed[i] < x$fit[i]){state[i] = 'below'}  else {state[i] = 'above'}
  
}

plot(x$FIRE_YEAR, x$observed) 
lines(x$FIRE_YEAR, x$fit)
lines(x$FIRE_YEAR, x$upr, lty = 2)
lines(x$FIRE_YEAR, x$lwr, lty = 2)

# first we need to calculate the transition probabilities between the various states. 

# Doing this is not super well documented in the `markovchain` package, but the 
# function `markovchainFit` will develop transition probabilities. 
# The input object works in pairs, essentially position 1 x[1] is state 1, and
# position 2 x[2] is state 2, the 3rd and 4th positions refer to an entirely new 
# set of observations. 

mcFitMLE <- markovchainFit(state, method = 'mle')

# we create another s4 object which the package relies on using base R's `new` 
# function 
weather_chain <- new(
  "markovchain", 
  states = c('above', 'below'), 
  transitionMatrix = mcFitMLE[["estimate"]]@transitionMatrix, # access the transition matrix
  name = "Regression Estimate Position")

steadyStates(weather_chain) # many more years are below the fire amount than are
# above the trend line is largely informed by a few large fires. 

# we can repeat this process n times. 
set.seed(1428)
markov_sim <- replicate(100, expr = 
            markovchainSequence(
              n=10,  # number of years into future. 
              markovchain=weather_chain, # chain object. 
              
              # TRY AND PREDICT T0? OR Just sample naively..... likely worth
              # sampling naively. 
              
          #    t0 = 'below', # state at t0
          #    include.t0 = TRUE
            )
)




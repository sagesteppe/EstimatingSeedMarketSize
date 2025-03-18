bas <- function(sims, years){
  
  pb <- txtProgressBar(
    min = 0,
    max = sims, 
    style = 3,  
    char = "=") 
  
  cluster <- parallel::makeCluster(parallel::detectCores()/4 ) 
  doParallel::registerDoParallel(cluster)
  parallel::clusterExport(
    cluster, c(
      'quantPred', 'years', 'reconcileEVT_quantiles', 'all_quants'))
  
  values <- foreach::foreach(j = seq_along(1:sims), .combine = c, .packages = 'quantregGrowth') %dopar% {
    
    for (i in seq_along(1:years)){ 
      values2 <- paste(sims[j], years[i])
    }
 #   setTxtProgressBar(pb, j)
  }
  parallel::stopCluster(cluster)
  
  return(values)
}

bas(sims = 20, years = 5)


csum <- function(x){sum(sample(1:10, size = x))} # make up a custom function 

#notice it doesn't need to be exported in this case. 
cluster <- parallel::makeCluster(parallel::detectCores()/4 ) 
doParallel::registerDoParallel(cluster)
foreach(n = 1:20, .combine = c) %dopar% csum(3)
parallel::stopCluster(cluster)

funny <- function(x){
  
  cluster <- parallel::makeCluster(parallel::detectCores()/4 ) 
  doParallel::registerDoParallel(cluster)
  parallel::clusterExport(cluster, c('csum')) # in this environment we need to export the FN but not 'x'
  
  resin <- foreach(n = 1:20, .combine = c) %dopar% csum(3)
  parallel::stopCluster(cluster)
  
  return(resin) # we need an explicit return call , and to save the results... 
}

funny(x = 3)


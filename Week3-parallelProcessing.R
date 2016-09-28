library(parallel)
library(foreach)
library(doParallel)

startParallelProcessing <- function() {
  noOfCores <- detectCores() - 1
  cluster <- makeCluster(noOfCores)
  registerDoParallel(cluster)
  cluster
}

stopParallelProcessing <- function(cluster) {
  stopCluster(cluster)
  stopImplicitCluster()
}
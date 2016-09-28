source("./Week3-parallelProcessing.R")

# Scoping issue
makeNGrams <- function(ovid) {
  setJvmOption <- function() {
    options(java.parameters = "-Xmx8192m" )
  }
  
  gramTokenizer <- function(ovid, n) {
    setJvmOption()
    RWeka::NGramTokenizer(ovid, RWeka::Weka_control(min = n, max = n))
  }
  
  cluster <- startParallelProcessing()
  clusterEvalQ(cluster, function() { 
    setJvmOption()
    library(RWeka)
  })
  
  print(paste("START TIME:", Sys.time()))
  
  ngrams <- foreach(x = c(1:1),
                    .combine = list,
                    .multicombine = TRUE,
                    .export = "ovid") %dopar% 
    gramTokenizer(ovid, x)
  
  stopParallelProcessing(cluster)
  
  print(paste("END TIME:", Sys.time()))
  
  gc()
  
  return(ngrams)
}
# Difference between delta = 0 vs. detla > 0 vs. delta < 0

library(BayesFactor)
library(parallel)
library(assortedRFunctions)

# Setting seed
set.seed(39257)


# Sample Size of simulation
n     <- 40
iter  <- 100 
delta <- seq(-2, 2, 0.1)

calculateBFs <- function(params){
  n     <- params[1]
  delta <- params[2]
  dat   <- rnorm(n, delta, 1)
  test1  <- ttestBF(dat)
  test2  <- ttestBF(dat, nullInterval = c(-Inf, 0))
  bfs   <- as.numeric(as.vector(c(test1, test2)))
  return(bfs)
}


params <- data.frame(n = rep(n, length(delta)*iter),
                     delta = rep(delta, iter))


# Creating cluster
numCores <- detectCores() - 1
print(paste('Cores used:', numCores))
cluster  <- makeCluster(numCores)
clusterExport(cluster, c('ttestBF', 
                         'calculateBFs'))


# Running analysis
startTime <- Sys.time()
print(paste('Start time:', startTime))
bfs      <- parRapply(cluster, params, calculateBFs)
time1    <- Sys.time()
print(paste('Finished 1:', time1))

# Stopping analysis
stopCluster(cluster)

# Saving results
save.image(file = datedFileNam('choiceOfNullHypothesis', '.RData'))

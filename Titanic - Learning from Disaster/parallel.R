# This script initialized the parallel processing depending of OS and system resources

usable.cores <- detectCores(all.tests = FALSE, logical = FALSE)-1

if(.Platform$OS.type == "unix") {
  # Linux
  require("doMC")
  registerDoMC(usable.cores)
  
} else {
  # Windows
  require("doParallel")
  registerDoParallel(makeCluster(usable.cores), cores=usable.cores)
  
 
}
# This script initialized the parallel processing depending of OS and system resources

require("parallel")

usable.cores <- detectCores(all.tests = TRUE, logical = TRUE)-1

writeLines(paste("Available cores: ", usable.cores))

if(.Platform$OS.type == "unix") {
  # Linux
  require("doMC")
  registerDoMC(usable.cores)
  
} else {
  # Windows
  require("doParallel")
  registerDoParallel(makeCluster(usable.cores), cores=usable.cores)
}

writeLines(paste("Running on",.Platform$OS.type))

rm(usable.cores)
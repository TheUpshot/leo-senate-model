workingDir  <-  getwd()
dataDir     <-  paste(workingDir, "data-publisher/", sep = "/")
modelDir    <-  paste(workingDir, "model", sep = "/")
fundyDir    <-  paste(workingDir, "fundamentals", sep = "/")

### run the model
setwd(modelDir)
n.days <- 30      # number of days to sim. set to "all" to run all days. 
just.today <- T   # if T, overrides n.days
n.sims <- 50000
source("senate-model-2014.R")

if (just.today) source("combine-data.R")

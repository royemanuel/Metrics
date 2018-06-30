source("fleetRes.R")

setwd("")

timetoGradFiles <- list.files(path = ".", pattern = "aircrew")

skedFiles <- list.files(path = ".", pattern = "sked")

######################################################################
## Define different Chi for each requirement

chiSatPre <- c(0)
chiSatPost <- c(0)

chiAoPre <- c(0)
chiAoPost <- c(0)

chiGradPre <- c(1, .5, .25)
chiGradPost <- c(1, 1, .5)

######################################################################
## Lists where the final resilience values are stored
satList <- list()
gradList <- list()
AoList <- list()

######################################################################
## Big for loop to go through the directory where I put the data


for (ttg in 1:length(timetoGradFiles)){
    DFrun <- read_csv(timetoGradFiles[ttg])
    DFrun$Need <- .85
    satList[[ttg]] <- satRes(DFrun)
}

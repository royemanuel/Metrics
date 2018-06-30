#source("fleetRes.R")

#setwd("fleetData/20180630-090621")

starttime <- Sys.time()
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

rseed <-
    list.files(path = ".", pattern = "ParametersRdm") %>%
    str_remove("ParametersRdmSd") %>%
    str_remove(".xlsx")
rseed <- as.integer(rseed)

######################################################################
## Lists where the final resilience values are stored
satList <- c()
gradList <- c()
AoList <- c()
xpList <- c()
rList <- c()


######################################################################
## Big for loop to go through the directory where I put the data


for (ttg in 1:length(timetoGradFiles)){
    DFrun <- read_csv(timetoGradFiles[ttg])
    DFrun$Need <- .85
    satList <- c(satList, satRes(DFrun, .85, 1200))
}

for (skd in 1:length(skedFiles)){
    sked <- read_csv(skedFiles[skd])
    AoList <- c(AoList, AoRes(sked, 0.85))
    gradList <- c(gradList, gradRes(sked, 65, chiGradPre, chiGradPost))
    exprmnt <-
        skedFiles[skd] %>%
        str_remove("skedTrackerExp") %>%
        str_remove("Run(.).csv")
    xpList <- c(xpList, as.integer(exprmnt))
    run <-
        skedFiles[skd] %>%
        str_remove("skedTrackerExp(.)Run") %>%
        str_remove(".csv")
    rList <- c(rList, as.integer(run))
}


resDF <- data.frame(SAT = satList, GRAD = gradList, Ao = AoList,
                    Run = rList, Experiment = xpList)
resDF$Seed <- rseed

endtime <- Sys.time()
print(endtime - starttime)





















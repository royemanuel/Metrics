## Building the first resilience indifference curves.
## First build the resilience matrix.
## Parameters: Failure level and Need level (constant)
## Constants:
## sigma = 0
## time horizon/vector 0 to 100 at increments of 1
## start level = 1
## end level = 1
## failure time = 25
## recovery time = 75
## time_delta (ESDF) = 20
## time decay = 0
library("caTools")
library("tidyverse")
starttime <- Sys.time()               

time <- data.frame(endTime = 100,
                   resolution = .5)

need <- data.frame(func = "constantNeed",
                   cLevel = seq(from = 0.05,
                       to = 1.0,
                       by = .05),
                   startTime = NA,
                   slope = NA)

## p <- data.frame(func = "step",
##                 failTime = 25,
##                 recTime = 75,
##                 preLevel = 1,
##                 failLevel = seq(from = 0.1,
##                     to = 1,
##                     by = 0.1),
##                 recLevel = 1)

## resParams <- data.frame(tDelta = 20,
##                         decay = 0,
##                         sigma = 0)

indiffResilience <- list()
failSteps <- seq(from = 0, to = 100, by = 25)
recoverSteps <- seq(from = 0, to = 100, by = 25)
sigmaSteps <- seq(from = 0, to = 1, by = 0.5)
trk <- 0
for(ftime in 1:(length(failSteps)-1)){
    for(rtime in ftime:(length(recoverSteps)-1)){
        for(stime in 1:(length(sigmaSteps))){
            resParams <- data.frame(tDelta = 20,
                                    decay = 0,
                                    sigma = sigmaSteps[stime])
            trk <- trk + 1
            ftValue = failSteps[ftime]
            print(ftValue)
            rtValue = recoverSteps[rtime + 1]
            print(rtValue)
            if(rtValue == 100){
                rtValue <- 99.9
            }
            p <- data.frame(func = "step",
                    failTime = ftValue,
                    recTime = rtValue,
                    preLevel = 1,
                    failLevel = seq(from = 0.05,
                        to = 1,
                        by = 0.05),
                    recLevel = 1)
            frResilience <- resLoop(time,need, p, resParams) 
            frResilience <- mutate(frResilience,
                   FailTime = ftValue,
                   RecoveryTime = rtValue)
            print(paste("Round", trk, ", fstep,", ftValue, ", rstep,", rtValue,
                        "sigmaStep,", sigmaSteps[stime]))
            indiffResilience[[trk]] <- frResilience
        }
    }
}

LoE <- bind_rows(indiffResilience)

LoE <- as.tibble(LoE)

LoEprepSig1 <-
    LoE %>%
    filter(Sigma == 0) %>%
    mutate(FailTime = as.character(FailTime),
           RecoveryTime = as.character(RecoveryTime),
           extResilience = round(extResilience, digits = 2),
           FailPerformance = pRun / 20)

EIRLoE <-
    LoEprepSig1 %>%
    filter(Time == 100) %>%
    select(Need, extResilience, FailPerformance, FailTime, RecoveryTime)

EIRLoEplotNeedByPerf <-
    ggplot(EIRLoE, aes(Need, FailPerformance, z=extResilience)) +
    geom_contour() +
    facet_grid(FailTime ~ RecoveryTime) +
    theme_bw() 

EIRLoEFailByNeed <- 
    ggplot(EIRLoE, aes(FailTime, Need, z=extResilience)) +
    geom_contour() +
    facet_grid(Need ~ pRun) +
    theme_bw()



## indiffResSmall <- resLoopShrink(time, need, p, resParams)
## indiffRes <- resLoop(time, need, p, resParams)

## Build the plottable dataframe for integral resilience
endtime <- Sys.time()
print(endtime - starttime)




IntResInd <- indiffRes %>%
    filter(Time == 100) %>%
        select(extResilience, Time, Need, pRun, EQR, extRho)

r <- ggplot(indiffResSmall, aes(pRun, Need, z=extResilience)) +
    geom_contour() +
    theme_bw()
q <- ggplot(IntResInd, aes(pRun, Need, z=EQR)) +
    geom_contour()
e <- ggplot(IntResInd, aes(pRun, Need, z=extRho)) +
    geom_contour() +
    theme_bw()

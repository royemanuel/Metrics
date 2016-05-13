## Try to write the metrics in R. It may be easier with
## dplyr and plyr and such available
library('plyr')
library('dplyr')
library('reshape2')
library('ggplot2')

## Instead of doing this step by step, should I build a single function
## that builds everything at once? No. but you should define the
## inputs at the beginning

## Define the time dataframe. From this, we will calculate all the other
## values we need.

## buildAll will get built as I make more and more functions to flesh
## out the whole thing. This way I will keep track of my inputs.
## buildAll <- function(timeHorizon, ## time from zero SH is interested in
##                      resolution, ## ticks between 0 - timeHorizon
##                      failtime, ## time failure is initiated
##                      recoverTime, ## time recovery is initiated
##                      preLevel, ## performance before failTime
##                      failLevel, ## performance after failure
##                      recLevel, ## performance after recovery
##                      needFunction, ## function defining need
##                      perfFunction, ## function defining performance
##                      ){
##     ## Build the initial data.frame timeline
##     tt <- data.frame(Time = seq(from = 0,
##                                to = timeHorizon,
##                                by = resolution))
## }


timeHorizon <- 100
resolution <- 5
timeTick <- data.frame(Time = seq(from = 0, to = timeHorizon, by = resolution))

performance <- function(tt, FUN = 1, ...){
    pt <- mutate(tt, Performance = FUN(tt$Time, ...))
}

## This is an example of a step failure with a recovery
stepFailRecover <- function(tt, failTime, recoverTime, preLevel,
                            failLevel, recLevel){
    preFail <- tt %>%
        filter(Time < failTime) %>%
            mutate(Performance = preLevel)
    postFail <- tt %>%
        filter(Time >= failTime & Time < recoverTime) %>%
            mutate(Performance = failLevel)
    postRec <- tt %>%
        filter(Time >= recoverTime) %>%
            mutate(Performance = recLevel)
    timeAndPerf <- rbind(preFail, postFail, postRec)
    return(timeAndPerf)
}

## a quick build for a need column that has a constant value
constantNeed <- function(tt, Need){
    tt <- cbind(tt, Need)
}
## A function to pull the performance at t0, the performance at td,
## and td
paramPull <- function(tt){
    pd <- min(tt$Performance)
    p0 <- tt$Performance[1]
    td <- filter(tt, Performance == pd)$Time[1]
    nd <- filter(tt, Performance == pd)$Need[1]
    rat <- tt %>% filter(npRatio == min(npRatio)) %>%
        filter(Time == min(Time))
    print(rat)
    ratD <- rat$npRatio[1]
    timeRatD <- rat$Time[1]
    ttPull <- data.frame(Perf0 = p0, PerfD = pd,
                         NeedD = nd, TimeD = td,
                         RatD = ratD, timeRatD = timeRatD)
}

## Calculate quotient resilience as the comparison
## quotient resilience is performance value (phi(t|e)) minus the
## performance after failure (phi(pd|e)) divided by the performance
## value prior to failure (phi(p0)) minus the performance after
## failure (phi(pd|e))
## Requires a data frame with a performance function
quotRes <- function(tt){
    pd <- min(tt$Performance)
    p0 <- tt$Performance[1]
    qr <- tt %>%
        ## Will want to change all to transmute so we aren't carrying
        ## around everything I think?
        ## transmute(QR = (Performance - pd)/(p0 - pd))
        mutate(QR = (Performance - pd)/(p0 - pd))
    return(qr)
}

## sigmaApply can be used as a function to apply the time
## substitutability
sigmaApply <- function(tt, sigma, cn){
    df <- tt %>%
        mutate(ratio = ifelse(Performance > Need,
                   1 + sigma *(Performance - Need)/Need,
                   Performance / Need))
    colnames(df)[dim(df)[2]] <- cn
    df
}
## Find the failed states (performance is at the minimum)


## Requires a data frame with a performance function and a need function
## We will use the minimum ratio value between phi(td) and phiN(td)
extQuotRes <- function(tt, sigma){
    pd <- min(tt$Performance)
    p0 <- tt$Performance[1]
    n0 <- tt$Need[1]
    rat0 <- ifelse(p0 > n0, 1 + sigma * (p0 - n0)/n0, p0/n0)
    ## firstFailedState is the row of the dataframe that has the lowest
    ## performance
    ## to need ratio at the minimum value. Not sure if this is 
    ##
    ## What should I do if need
    ## increases faster than the performance recovers (ratio is worse)
    ## than at the moment of worst performance? I dunno right now.
    failedStates <- tt %>% ## NOT MINIMUM PERFORMANCE!!! MIN RATIO!!!
        filter(Performance == pd) %>%
            mutate(failRatio = Performance / Need) %>%
                filter(failRatio == min(failRatio))
    firstFailedState <- failedStates %>% filter(Time == min(Time))
    ffsPerformance <- firstFailedState$Performance
    ffsNeed <- filter(tt, Time == firstFailedState$Time)
    ffsNeed <- ffsNeed$Need
    tt <- sigmaApply(tt, sigma, "npRatio")
    tt <- mutate(tt, EQR = (npRatio - ffsPerformance / ffsNeed) /
                     (rat0 - ffsPerformance / ffsNeed))
    vars <- c(pd, p0, n0, rat0, ffsPerformance,ffsNeed)
    names(vars) <- c("pd", "p0", "n0", "rat0", "ffsPerformance", "ffsNeed")
    print(vars)
    return(tt)
}

## Building the ESDF model
## Speedfactor is a function that needs the time of the disturbance
## (disturbTime), the time that initial recovery activities are complete,
## (initRecTime), the time final recovery activities are complete
## (finRecTime), and a decay factor (decay). The full ESDF requires a
## disturbance probability at a certain time and the probability of failure
## given a disturbance, but that is modeled in the performance profile
## tDelta is defined by the stakeholder for an adequate allowable amount
## of time to get to recovery
speedFactor <- function(disturbTime, initRecTime, finRecTime, tDelta, decay){
    timeToInitRec <- initRecTime - disturbTime
    if(finRecTime >= initRecTime){
        sf <- (tDelta/timeToInitRec)*exp(-decay*(finRecTime - initRecTime))
    } else {
        sf <- tDelta/timeToInitRec
    }
    return(sf)
}
## Resilience Factor rho(s_p, phi_r, phi_d,phi_0)
## Do I take times as the input? I am defining these, correct?
## Takes as its inputs the initial recovery time, the
## Due to failure profiles, the disturbance time and the fail time
## are not the same. ESDF calculates from the disturbance to the initial
## recovery
## resFac <- function(tt, disturbTime, initRecTime, finRecTime, tDelta, decay){
##     phiD <- filter(tt, Time == disturbTime)$Performance
##     sf <- speedFactor(disturbTime, initRecTime, finRecTime, tDelta, decay)
##     phiR <- filter(tt, Time == finRecTime)$Performance
##     phi0 <- tt$Performance[1]
##     vars <- c(sf, phiD, phi0)
##     names(vars) <- c("SpeedFactor", "Phi_D", "Phi_0")
##     
##     tt <- mutate(tt, Rho = ifelse(Time < disturbTime, 1,
##                          sf * phiD * tt$Performance /
##                              (phi0 ^ 2)))
## }
## resFac using only tt and tDelta
resFac <- function(tt,
                   tDelta,
                   initRecTime,
                   finRecTime,
                   decay){
    disturbRow <- tt %>% filter(Performance == min(Performance)) %>%
        filter(Time == min(Time))
    phiD <- disturbRow$Performance
    timeD <- disturbRow$Time
    sf <- speedFactor(timeD, initRecTime, finRecTime, tDelta, decay)
    phi0 <- tt$Performance[1]
    vars <- c(sf, phiD, timeD, phi0)
    names(vars) <- c("SpeedFactor", "Phi_D", "timeD", "Phi_0")
    print(vars)
    tt <- mutate(tt, Rho = ifelse(Time < timeD, 1,
                         sf * phiD * tt$Performance /
                             (phi0 ^ 2)))
}


extResFac <- function(tt,
                      tDelta,
                      initRecTime,
                      finRecTime,
                      decay,
                      sigma){
    disturbRow <- tt %>% filter(npRatio == min(npRatio)) %>%
        filter(Time == min(Time))
    print(disturbRow$Time)
    dTime <- disturbRow$Time
    print("This is dTime")
    print(dTime)
    disturbRatio <- disturbRow$npRatio
    sf <- speedFactor(dTime, initRecTime, finRecTime, tDelta, decay)
    recovRatio <- filter(tt, Time == finRecTime)$npRatio
    vars <- c(sf,
              dTime,
              disturbRow$Time,
              initRecTime,
              finRecTime,
              disturbRatio,
              recovRatio)
    names(vars) <- c("SF",
                     "dTime",
                     "disturbRow$Time",
                     "initRecTime",
                     "finRecTime",
                     "disturbRatio",
                     "recovRatio")
    print(vars)
    tt <- mutate(tt, extRho = ifelse(Time < dTime, 1,
                         sf * (disturbRatio * tt$npRatio)))
}

intRes <- function(tt, sigma){
    ## Calculate the area under the performance curve
    tt <- mutate(tt, perfLag = lag(Performance, 1))
    tt$perfLag[1] <- tt$perfLag[2]
    stepSize <- tt$Time[2] - tt$Time[1]
    tt <- mutate(tt, perfHeight = ifelse(Performance > perfLag, perfLag,
                         Performance),
                 perfStepArea = stepSize *
                     (perfHeight + abs(Performance - perfLag)/2))
    tt$perfStepArea[1] <- 0
    tt <- mutate(tt, perfArea = cumsum(perfStepArea))
    ## Calculate the area under the status quo Need curve
    tt <- mutate(tt, statQuoStepArea = stepSize * tt$Performance[1])
    tt$statQuoStepArea[1] <- 0
    ## Calculate the resilience value
    tt <- mutate(tt, statQuoArea = cumsum(statQuoStepArea),
                 statQuoResilience = perfArea / statQuoArea)
    tt$statQuoResilience[1] <- 1
    ## Clean up the tt data.frame
    ## Calculate the resilience with need
    ## Calculate the area under the Need Curve
    tt <- mutate(tt, needLag = lag(Need, 1))
    tt$needLag[1] <- tt$needLag[2]
    stepSize <- tt$Time[2] - tt$Time[1]
    tt <- mutate(tt, needHeight = ifelse(Need > needLag, needLag,
                         Need),
                 needStepArea = stepSize *
                     (needHeight + abs(Need - needLag)/2),
                 needArea = cumsum(needStepArea),
                 extResStep = ifelse(needStepArea > perfStepArea,
                     stepSize * perfStepArea / needStepArea,
                     stepSize *(1 + sigma *
                                    (perfStepArea - needStepArea) /
                                        needStepArea)))
    tt$extResStep[1] <- 0
                 tt <- mutate(tt, extResArea = cumsum(extResStep),
                              extResilience = extResArea / Time)
    tt$extResilience[1] <- tt$extResilience[2]
    return(tt)
}

## Cleanup the data.frame after running all of the above
tidyDF <- function(tt){
    tt <- select(tt, -c(perfLag, perfHeight, perfStepArea,
                        perfArea, statQuoStepArea, statQuoArea,
                        needLag, needHeight, needStepArea, needArea,
                        extResStep, extResArea))
}

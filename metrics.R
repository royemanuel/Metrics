## Try to write the metrics in R. It may be easier with
## dplyr and plyr and such available
library('plyr')
library('dplyr')
library('reshape2')
library('ggplot2')
library('extrafont')

## Function that builds a  time column from 0 to endTime with resolution
## size intervals
timeColumn <- function(endTime, resolution){
    t <- data.frame(Time = seq(0, endTime, resolution))
}

## This is an example of a performance profile with step failure and recovery
## ____     ____
##     |   |
##     |___|
## failTime = time the system degrades
## recoverTime = time the system has recovered
## preLevel = performance level of the system before failure
## failLevel = level of the system after failure and before recovery
## reclevel = level of the system after recovery
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
## This is a performance profile with step failure and linear recovery
## ____    ____
##     |  /
##     | /
##     |/
resTriangle <- function(tt,
                        failTime,
                        recoverTime,
                        preLevel,
                        failLevel,
                        recLevel){
    slope <- (recLevel -failLevel)/(recoverTime - failTime)
    preFail <- tt %>%
        filter(Time < failTime) %>%
            mutate(Performance = preLevel)
    postFail <- tt %>%
        filter(Time >= failTime & Time < recoverTime) %>%
            mutate(Performance = failLevel + slope *(Time - failTime))
    postRec <- tt %>%
        filter(Time >= recoverTime) %>%
            mutate(Performance = recLevel)
    timeAndPerf <- rbind(preFail, postFail, postRec)
    return(timeAndPerf)
}

######################################################################
## I will need to add a function that pulls the time endpoints and
## the interval from the model outputs automatically. This should be
## somewhat simple. 
######################################################################

## a quick build for a need column that has a constant value
constantNeed <- function(tt, Need){
    tt <- cbind(tt, Need)
}
## A need that changes linearly at a defined start time. Inputs are
## initial need level (need0), startTime, and slope of the need.
## For decreasing need put in a negative value
linearNeed <- function(tt, need0, startTime, slope){
    tt <- tt %>%
        mutate(Need = ifelse(Time < startTime,
               need0,
               need0 + (Time - startTime) * slope))
}
## A function to pull the performance at t0, the performance at td,
## and td
######################################################################
## This was commented out because we won't necessarily have the time
## of failure and such defined for us. For a changing need, that may
## be different
## paramPull <- function(tt){
##     pd <- min(tt$Performance)
##     p0 <- tt$Performance[1]
##     td <- filter(tt, Performance == pd)$Time[1]
##     nd <- filter(tt, Performance == pd)$Need[1]
##     rat <- tt %>% filter(npRatio == min(npRatio)) %>%
##         filter(Time == min(Time))
##     ## print(rat)
##     ratD <- rat$npRatio[1]
##     timeRatD <- rat$Time[1]
##     ttPull <- data.frame(Perf0 = p0, PerfD = pd,
##                          NeedD = nd, TimeD = td,
##                          RatD = ratD, timeRatD = timeRatD)
## }

## Calculate quotient resilience as the comparison
## quotient resilience is performance value (phi(t|e)) minus the
## performance after failure (phi(pd|e)) divided by the performance
## value prior to failure (phi(p0)) minus the performance after
## failure (phi(pd|e))
## Requires a data frame with a performance function
quotRes <- function(tt){
    ## Define the value of the minimum performance for the entire profile
    ## Much like the comments in ESDF, need to build a searching method
    ## for accounting for multiple failures. Not built in at this time.
    pd <- min(tt$Performance)
    ## Baseline performance at time 1
    p0 <- tt$Performance[1]
    ## Pull the time of the failure by filtering for minimum performance
    ## then taking the time value for the first row. May be more
    ## elegant to use min, but I don't know if that is worthwhile
    Td <- filter(tt, Performance == min(Performance))$Time[1]
    ## print(Td)
    qr <- tt %>%
        mutate(QR = (Performance - pd)/(p0 - pd),
               QR_Td = Td)
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


## Requires a data frame with a performance function and a need function
## We will use the minimum ratio value between phi(td) and phiN(td)
extQuotRes <- function(tt, sigma){
    ## Identify the lowest level of performance
    pd <- min(tt$Performance)
    ## Set the level of performance at the start of the time series
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
    ## print(firstFailedState)
    ffsPerformance <- firstFailedState$Performance
    ffsTime <- firstFailedState$Time
    ffsNeed <- filter(tt, Time == firstFailedState$Time)
    ffsNeed <- ffsNeed$Need
    tt <- sigmaApply(tt, sigma, "npRatio")
    tt <- mutate(tt,
                 ## The actual value for the EQR
                 EQR = (npRatio - ffsPerformance / ffsNeed) /
                     (rat0 - ffsPerformance / ffsNeed),
                 EQR_FailTime = ffsTime,
                 Rat0 = rat0)
    ## vars <- c(pd, p0, n0, rat0, ffsPerformance,ffsNeed)
    ## names(vars) <- c("pd", "p0", "n0", "rat0", "ffsPerformance", "ffsNeed")
    ## print(vars)
    return(tt)
}
###################################################################### 
## Building the ESDF model                                          ## 
######################################################################

## Speedfactor is a function that needs the time of the disturbance
## (disturbTime), the time that initial recovery activities are complete,
## (initRecTime), the time final recovery activities are complete
## (finRecTime), and a decay factor (decay). The full ESDF requires a
## disturbance probability at a certain time and the probability of failure
## given a disturbance, but that is modeled in the performance profile
## tDelta is defined by the stakeholder for an adequate allowable amount
## of time to get to recovery
speedFactor <- function(disturbTime,
                        initRecTime,
                        finRecTime,
                        tDelta,
                        decay){
    ## This is adequate for a strictly increasing recovery.
    ## Must include method for detecting pauses and dips in recovery.
    timeToInitRec <- initRecTime - disturbTime
    ## print("FinRec")
    ## print(finRecTime)
    ## print("initRec")
    ## print(initRecTime)
    if(finRecTime >= initRecTime){
        sf <- (tDelta/timeToInitRec)*exp(-decay*(finRecTime - initRecTime))
    } else {
        ## Note that faster recovery than slack time "rewards" the
        ## system with higher resilience.
        sf <- tDelta/timeToInitRec
    }
    return(sf)
}


## The purpose of the recovery time in the metric per the documentation
## is that the time horizon will equal the finRecTime. init Rect time
## will be defined by tDelta. That is the time from failure until an
## initial recovery must be made. This is defined in the literature
## so I think it is appropriate that we take this to be a case where if
## the time horizon... no that doesn't quite work. hmm....
resFac <- function(tt,
                   tDelta,
                   ##initRecTime,
                   ##finRecTime,
                   decay){
    ## Find the minimum performance (phiD) and the first instance of time
    ## that it is at this level. This becomes the disturbance time timeD
    disturbRow <- tt %>% filter(Performance == min(Performance)) %>%
        filter(Time == min(Time))
    phiD <- disturbRow$Performance
    ## print(phiD)
    timeD <- disturbRow$Time
    if(timeD > 0){
        ## print("timeD")
        ## print(timeD)
        ## Build a data frame that includes all of the recovery times
        ## There is potential to detect additional failures by looping this
        ## process over the recoveryID data frame. We would need to change the
        ## disturbRow from looking for a minimum to looking to the lowest
        ## performance that then is arrested for some given time.
        recoveryID <- tt %>%
            filter(Time > timeD) %>%
                filter(Performance > phiD)
        initRecTime <- recoveryID$Time[1]
        ## print(length(recoveryID))
        ## Likewise to the recoveryID, we can look for the first performance
        ## that is repeated. This is good enough right now for a step
        ## failure with a step recovery. When we start to include multiple
        ## failures and a stepped recovery, it will be necessary to auto-
        ## pull interim recovery times for this metric.
        finRecTime <- recoveryID[which.max(recoveryID$Performance), "Time"]
        ## print(list(initRecTime = initRecTime, finRecTime = finRecTime))
        sf <- 1 ## speedFactor(timeD, initRecTime, finRecTime, tDelta, decay)
        phi0 <- tt$Performance[1]
        vars <- c(sf, phiD, timeD, phi0)
        names(vars) <- c("SpeedFactor", "Phi_D", "timeD", "Phi_0")
        ## print(vars)
        tt <- mutate(tt, Rho = ifelse(Time < timeD, 1,
                             sf * phiD * tt$Performance /
                                 (phi0 ^ 2)))
        tt$RF_FailTime <- timeD
        tt$RF_TDelta <- tDelta
        if (length(finRecTime) > 0){
            tt$RF_RecTime <- finRecTime
        } else {
            tt$RF_RecTime <- Inf
        }
        tt$RF_DwellTime <- tt$RF_RecTime - tt$RF_FailTime
    } else {
        tt <- mutate(tt, Rho = 1)
        tt$RF_FailTime <- NA
        tt$RF_TDelta <- tDelta
        tt$RF_RecTime <- NA
        tt$RF_DwellTime <- NA

    }
    return(tt)
}

## The final recovery times are different between extended and regular
## resilience factors  Think about this.
extResFac <- function(tt,
                      tDelta,
                      ## initRecTime,
                      ## finRecTime,
                      decay,
                      sigma){
    ## I need to figure out the best way to prevent infinity from
    ## occurring with very short recovery times and a finite tDelta
    ## print(head(tt))
    ## print(head(tt$npRatio))
    ## Discussion point in the dissertation. For the current purposes
    ## looking at single failure with strictly decreasing failure,
    ## strictly increasing recovery, and constant need, this will
    ## be adequate.
    ## This is pulling the minimum RATIO!!! With a constant need
    ## this is not a problem, but we will want to demonstrate the
    ## difference when we get to more complicated need. Personally,
    ## I like this because it will catch a catastrophic increase in
    ## Need where the traditional metric does not!!!
    disturbRow <- tt %>% filter(npRatio == min(npRatio)) %>%
        filter(Time == min(Time))
    ## print(disturbRow$Time)
    phiD <- disturbRow$Performance
    ## print(phiD)
    timeD <- disturbRow$Time
    ## print("This is timeD")
    ## print(timeD)
    if(timeD>0){
        disturbRatio <- disturbRow$npRatio
        ## Identify when recovery occurs. Simply defined as the first time
        ## increasing performance
        recoveryID <- tt %>%
            filter(Time > timeD) %>%
                filter(Performance > phiD)
        ## print(head(recoveryID))
        initRecTime <- recoveryID$Time[1]
        ## print("initRecTime ERF")
        ## print(initRecTime)
        ## Simplistic recovery defined as the first time step that has
        ## performance greater than need.
        ## print(tail(tt))
        perfDiff <- tt %>%
            mutate(Diff = Performance - Need) %>%
                filter(Time > timeD & Diff >= 0)
        ## print(perfDiff)
        ## print(dim(perfDiff))
        finRecTime <- ifelse(!dim(perfDiff)[1],
                             max(tt$Time),
                             perfDiff$Time[1])
        if (is.na(initRecTime)){
            initRecTime <- timeD
        }
        ## print(max(tt$Time))
        ## print("finRecTime ERF")
        ## print(finRecTime)
        sf <- 1 ## speedFactor(timeD, initRecTime, finRecTime, tDelta, decay)
        recovRatio <- filter(tt, Time == finRecTime)$npRatio
        ## vars <- c(sf,
        ##           timeD,
        ##           disturbRow$Time,
        ##           initRecTime,
        ##           finRecTime,
        ##           disturbRatio,
        ##           recovRatio)
        ## names(vars) <- c("SF",
        ##                  "timeD",
        ##                  "disturbRow$Time",
        ##                  "initRecTime",
        ##                  "finRecTime",
        ##                  "disturbRatio",
        ##                  "recovRatio")
        ## print(vars)
        tt <- mutate(tt, extRho = ifelse(Time < timeD, 1,
                             sf * (disturbRatio * tt$npRatio)),
                     ERF_FailTime = timeD,
                     ERF_TDelta = tDelta,
                     ERF_FinalRecTime = finRecTime,
                     ERF_DwellTime = ERF_FinalRecTime - ERF_FailTime
                     )
    } else {
        tt <- mutate(tt, extRho = 1,
                     ERF_FailTime = NA,
                     ERF_TDelta = tDelta,
                     ERF_FinalRecTime = NA,
                     ERF_DwellTime = NA)
    }
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
    tt <- mutate(tt,
                 needHeight = ifelse(Need > needLag, needLag, Need),
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



## Build the entire resilience matrix. This needs to include all of the
## inputs for every resilience function as well as for the need and
## performance functions. These will be included by making a vector for
## each one of them that is unpacked in the function itself. Then I will
## run through an if-else or a switch type function to make it happen.
buildResMatrix <- function(timeList, needList, perfList, resList){
    ## a time vector uses an endTime and a resolution
    resMat <- timeColumn(timeList$endTime, timeList$resolution)
    ## print("time done")
    # print( head(resMat))
    resMat <- switch(as.character(needList$func),
                     constantNeed = constantNeed(resMat, needList$cLevel),
                     linearNeed = linearNeed(
                         resMat,
                         needList$cLevel,
                         needList$startTime,
                         needList$slope),
                     ## fullDef should bind a fully defined need
                     ## vector
                     fullDef = cbind(fullDef, resMat))
    ## print("need done")
    # print( head(resMat))
    resMat <- switch(as.character(perfList$func),
                     step = stepFailRecover(resMat,
                         perfList$failTime,
                         perfList$recTime,
                         perfList$preLevel,
                         perfList$failLevel,
                         perfList$recLevel),
                     resTriangle = resTriangle(resMat,
                         perfList$failTime,
                         perfList$recTime,
                         perfList$preLevel,
                         perfList$failLevel,
                         perfList$recLevel),
                     custom = cbind(resMat, Performance =
                                        perfList$Performance))
    ## print("performance done")
    ## print(head(resMat))
    resMat <- quotRes(resMat)
    ## print("QR done")
    resMat <- extQuotRes(resMat, resList$sigma)
    ## print("EQR done")
    resMat <- resFac(tt = resMat,
                     tDelta = resList$tDelta,
                     ## initRecTime = resList$initRecTime,
                     ## finRecTime = resList$finRecTime,
                     decay = resList$decay)
    ## print("RF done")
    resMat <- extResFac(tt = resMat,
                        tDelta = resList$tDelta,
                        ## initRecTime = resList$initRecTime,
                        ## finRecTime = resList$finRecTime,
                        decay = resList$decay,
                        sigma = resList$sigma)
    ## print("ERF done")
    resMat <- intRes(resMat,
                     sigma = resList$sigma)
    ## print("IntRes done")
    resMat <- tidyDF(resMat)
    return(resMat)
}

## Call the function multiple time for variable inputs. All inputs
## are data.frame
resLoop <- function(time, need, performance, resFactors){
    resMat <- data.frame()
    needStep <- dim(need)[1]
    perfStep <- dim(performance)[1]
    resStep <- dim(resFactors)[1]
    timeStep <- dim(time)[1]
    for (needRun in 1:needStep){
        for (perfRun in 1:perfStep){
            for (resRun in 1:resStep){
                for (timeRun in 1:timeStep){
                    print(paste0("NR = ", needRun, ", ",
                                 "PR = ", perfRun, ", ",
                                 "RR = ", resRun, ", ",
                                 "TR = ", timeRun))
                    k <- buildResMatrix(time[timeRun,],
                                        need[needRun,],
                                        performance[perfRun,],
                                        resFactors[resRun,]
                                        )
                    k <- cbind(k,
                               tRun = timeRun,
                               nRun = needRun,
                               pRun = perfRun,
                               rRun = resRun,
                               Decay = resFactors$decay[resRun],
                               Sigma = resFactors$sigma[resRun]
                               )
                    resMat <- rbind(resMat, k)
                }
            }
        }
    }
    resMat
}
## Call the function multiple time for variable inputs. All inputs
## are data.frame. This one pulls only the columns you want. Makes
## a smaller dataframe.
resLoopShrink <- function(time, need, performance, resFactors){
    resMat <- data.frame()
    needStep <- dim(need)[1]
    perfStep <- dim(performance)[1]
    resStep <- dim(resFactors)[1]
    timeStep <- dim(time)[1]
    for (needRun in 1:needStep){
        for (perfRun in 1:perfStep){
            for (resRun in 1:resStep){
                for (timeRun in 1:timeStep){
                    print(paste0("NR = ", needRun, ", ",
                                 "PR = ", perfRun, ", ",
                                 "RR = ", resRun, ", ",
                                 "TR = ", timeRun))
                    k <- buildResMatrix(time[timeRun,],
                                        need[needRun,],
                                        performance[perfRun,],
                                        resFactors[resRun,]
                                        )
                    k <- cbind(k,
                               tRun = timeRun,
                               nRun = needRun,
                               pRun = perfRun,
                               rRun = resRun,
                               Decay = resFactors$decay[resRun],
                               Sigma = resFactors$sigma[resRun]
                               )
                    ## This is where you select the columns you want
                    ## in the final matrix
                    k <-  k %>% filter(Time == 100) %>%
                        select(extResilience, Time, Need, pRun, EQR,
                               extRho)
                    resMat <- rbind(resMat, k)
                }
            }
        }
    }
    resMat
}


######################################################################
 ######################################################################
## PLOTTING FUNCTIONS
######################################################################
######################################################################


## Plot Need for each metric
pltMoveNeed <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
            select(Time, Need, QR, EQR, Rho,
                   extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time", "Need"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
    ## print(colnames(workDF))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    plt <- ggplot(workDF, aes(Need, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
        scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 8, base_family = "serif") +
                theme(legend.position = c(.85, .15))
}

## Plot Recovery time changing for each **original** metric
pltMoveRecovery <- function(df, t){
    workDF <- df %>%
        filter(Time == t) %>%
        mutate(TTR = ERF_FinalRecTime - ERF_FailTime) %>%
        select(Time, QR, EQR, Rho, TTR,
               extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time", "TTR"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
    ## print(colnames(workDF))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                                 "Extended",
                                 "Original")) %>%
        filter(variable == "Original")
    workDF <- rename(workDF, Resilience = value)
    plt <- ggplot(workDF, aes(TTR, Resilience)) +
        geom_line() +
        facet_grid(ResType ~ .)
    plt <- plt +
        theme_bw(base_size = 8, base_family = "serif") +
        theme(legend.position = c(.85, .15)) +
        scale_x_continuous("Time to Recover")
}

## Plot Substitution (sigma) for each metric
pltSubNeed <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
        select(Time, QR, EQR, Rho,
                   extRho, statQuoResilience, extResilience, Sigma)
    workDF <- melt(data = workDF, id = c("Time", "Sigma"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
    ## print(colnames(workDF))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    plt <- ggplot(workDF, aes(Sigma, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable))  +
                                      ## May or may not want to facet
                                      ## this one. Looked pretty good
                                      ## on one plot, but for consistency
                                      ## probably fact it.
                                      facet_grid(ResType ~ .)
    plt <- plt +
        scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 8, base_family = "serif") +
                theme(legend.position = c(.85, .15))
}
## Plot resilience as the time horizon changes
pltMoveTimeH <- function(df){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                   extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Time, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
    scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = c(.85, .15))
}

## Plot the need and performance of a resilience matrix when they are
## held constant. This allows you to look at what the performance profile
## looks like when analyzing the results.
pltNeedPerf <- function(df){
    wdf <- df %>%
        filter(tRun == 1, nRun == 1, pRun == 1, rRun == 1) %>%
            select(Need, Time, Performance)
    wdf <- melt(data = wdf, id = c("Time"))
    wdf <- rename(wdf, Performance = value)
    plt <- ggplot(wdf, aes(Time, Performance, group = variable)) +
                               geom_line(aes(linetype = variable)) +
                               theme_bw(base_size = 8, base_family = "serif") +
                               theme(legend.position = c(.85, .15)) +
                               scale_linetype_discrete(name = "")
}

## Plot the effect of tDelta changes on rho and extRho when decay is
## zero. Need to finish this
pltMoveTDelta <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
            select(Rho,
                   extRho,
                   RF_DwellTime,
                   ERF_DwellTime,
                   RF_TDelta,
                   ERF_TDelta) %>%
                       mutate(SF = RF_TDelta / RF_DwellTime) %>%
                           select(-RF_DwellTime,
                                  -RF_TDelta,
                                  -ERF_DwellTime,
                                  -ERF_TDelta)
    ## Everything after this is wrong
    workDF <- melt(data = workDF, id = c("SF"))
    ## print(head(workDF))
    plt <- ggplot(data = workDF, aes(SF, value, group = variable,
                      color = variable)) + geom_line()
}

pltPerf <- function(df){
    df <- df %>%
        select(Time, Performance, Need) %>%
        melt(id = "Time")
    plt <- ggplot(data = df, aes(Time, value,
                                 group = variable,
                                 linetype = variable)) + geom_line() +
        theme_bw(base_size = 8, base_family = "serif") +
            scale_linetype_discrete(name = "") +
                theme(legend.position = c(.85, .25)) +
                    labs(y = "Performance") + ylim(0, 1)
}

myPlotSave <- function(name){
    ggsave(filename = name, plot = last_plot(), width = 3.5, height = 2)
}

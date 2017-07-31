######################################################################
##         The framework for random performance profiles            ##
######################################################################
TH <- 100
failLambda <- 10
meanTTR <- 5
sigmaTTR <- .1

maxFailP <- .5
minFailP <- 0

## Build multiple failures in timehorizon

## make a vector of failures given a time horizon, a parameter (or more
## if we add other distributions). This is for exponential failures 
perfParams <- function(TimeHorizon,
                       param1,
                       meanRecValue,
                       sigRecValue,
                       failShape,
                       failScale,
                       recShape,
                       recScale){
    ftList <- data.frame( FailureTimes = 0,
                         RecoveryTimes = 0,
                         FailLevel = 0,
                         RecoveryLevel = 0)
    logMean <- log((meanRecValue**2)/(sqrt(sigRecValue**2 + meanRecValue**2)))
    logSigmaSquare <- log((sigRecValue**2 + meanRecValue**2)/meanRecValue**2)
    i <- 1
    while(tail(ftList,1)$FailureTimes < TH){
        ## sum last row failure and recovery to get start point for next
        ## failure
        currentTime <- tail(ftList$FailureTimes,1) +
            tail(ftList$RecoveryTimes,1)
        ## Draw for the failure time
        TF <- rexp(1, 1/param1) + currentTime
        if(TF > TH){
            ## if the failure happens after the time horizon... don't care!
            break
        }
        ## draw for failLevel
        FL <- rbeta(1, failShape, failScale)
        ## draw for the recovery time
        TR <- rlnorm(1, logMean, logSigmaSquare) +
            currentTime + TF
        ## draw for the recovery level
        RL <- rbeta(1, recShape, recScale) * 1.2
        tmpDF <- data.frame(FailureTimes = TF,
                            RecoveryTimes = TR,
                            FailLevel = FL,
                            RecoveryLevel = RL)
        ftList[i,] <- tmpDF
        i <- i+1
    }
    ftList[,1:2] <- (floor(ftList[,1:2]))
    return(ftList)
}

buildExperiment <- function(numberRuns,
                            TimeHorizon,
                            param1,
                            meanRecValue,
                            sigRecValue,
                            failShape,
                            failScale,
                            recShape,
                            recScale){
    frExp <- data.frame()
    for(r in 1:numberRuns){
        simRun <- perfParams(TimeHorizon,
                            param1,
                            meanRecValue,
                            sigRecValue,
                            failShape,
                            failScale,
                            recShape,
                             recScale)
        simRun <- mutate(simRun, Run = r)
        frExp <- bind_rows(frExp, simRun)
    }
    return(frExp)
}

## build profiles from the experiments. Start off with just step functions
buildProfiles <- function(expList, startLevel=1){
    for(r in 1:expList$Run){
        filter(expList, Run == r)
        
    }
    
}


TTR <- function(meanValue, sigValue, failures){
    logMean <- log((meanValue**2)/(sqrt(sigValue**2 + meanValue**2)))
    logSigmaSquare <- log((sigValue**2 + meanValue**2)/meanValue**2)
    recTimes <- rlnorm(length(failures), logMean, logSigmaSquare)
    recTimes <- floor(recTimes)
}

failRecDF <- function(failList, recoverList, TimeHorizon){
    DF <- data.frame(FailureTimes = failList,
                     RecoveryTimes = recoverList)
    DF <- DF %>%
        mutate(AbsRecoveryTime = FailureTimes + RecoveryTimes) %>%
            filter(AbsRecoveryTime < TimeHorizon)
}

failLevels <- function()

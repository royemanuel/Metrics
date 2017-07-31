######################################################################
##         The framework for random performance profiles            ##
######################################################################
TH <- 100
failLambda <- 10
meanTTR <- 20
sigmaTTR <- 6

maxFailP <- .5
minFailP <- 0

## Build multiple failures in timehorizon

## make a vector of failures given a time horizon, a parameter (or more
## if we add other distributions). This is for exponential failures 
TBTF <- function(TimeHorizon, param1, param2=NULL, param3=NULL){
    ftList <- c(0)
    i <- 1
    while(tail(ftList,1) < TH){
        TF <- rexp(1, 1/param1) + ftList[i]
        if(TF > TH){
            break
        }
        ftList <- c(ftList, TF)
        i <- i+1
    }
    ftList <- floor(ftList[-1])
    return(ftList)
}

t <- TBTF(TH, failLambda)
t

TTR <- function(meanValue, sigValue, failures){
    logMean <- log((meanValue**2)/(sqrt(sigValue**2 + meanValue**2)))
    logSigmaSquare <- log((sigValue**2 + meanValue**2)/meanValue**2)
    recTimes <- rlnorm(length(failures), logMean, logSigmaSquare)
    recTimes <- floor(recTimes)
}

failRecDF <- function(failList, recoverList, TimeHorizon){
    
}

failLevels <- function()

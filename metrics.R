## Try to write the metrics in R. It may be easier with
## dplyr and plyr and such available
library('plyr')
library('dplyr')
library('reshape2')
library('ggplot2')

## Define the time dataframe. From this, we will calculate all the other
## values we need.
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
constantNeed <- function(tt, need){
    tt <- tt %>%
        mutate(Need = need)
}

## Calculate quotient resilience as the comparison
quotRes <- function(tt, ){
    
}

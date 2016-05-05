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

## Requires a data frame with a performance function and a need function
## We will use the minimum ratio value between phi(td) and phiN(td)
extQuotRes <- function(tt, sigma){
    pd <- min(tt$Performance)
    print(pd)
    p0 <- tt$Performance[1]
    print(p0)
    n0 <- tt$Need[1]
    print(n0)
    rat0 <- ifelse(p0 > n0, 1 + sigma * (p0 - n0)/p0, p0/n0)
    print(rat0)
    ## firstFailedState is the row of the dataframe that has the lowest
    ## performance
    ## to need ratio at the minimum value.
    ##
    ## What should I do if need
    ## increases faster than the performance recovers (ratio is worse)
    ## than at the moment of worst performance? I dunno right now.
    failedStates <- tt %>%
        filter(Performance == pd) %>%
            mutate(failRatio = Performance / Need) %>%
                filter(failRatio == min(failRatio))
    firstFailedState <- failedStates %>% filter(Time == min(Time))
    ffsPerformance <- firstFailedState$Performance
    print(ffsPerformance)
    ffsNeed <- filter(tt, Time == firstFailedState$Time)
    ffsNeed <- ffsNeed$Need
    print(ffsNeed)
    tt <- sigmaApply(tt, sigma, "npRatio")
    tt <- mutate(tt, EQR = (npRatio - ffsPerformance / ffsNeed) /
                     (rat0 - ffsPerformance / ffsNeed))
}

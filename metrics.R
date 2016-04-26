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


stepFailRecover <- function(tt, failTime, recoverTime, preLevel,
                            failLevel, recLevel){
    tt %>%
        mutate(Performance = ifelse(Time < failTime, preLevel,
                   ifelse(Time <= recoverTime, failLevel,
                          ifelse(time > recoverTime, recLevel))))
    tt
}

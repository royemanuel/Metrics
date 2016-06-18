## Building functions that loop the metric through different values
## to make an interesting plot

##changeNeed is a function that takes in a melted "Need" with "Run" to
## build the resilience metrics

## changeNeed <- function()
## Build a data.frame with the need varying from 0 to 1.3 in increments
## of 0.1
source('metrics.R')
## Build the Time column with the appropriate time ticks
timeHorizon <- 100
resolution <- 5
timeTick <- data.frame(Time = seq(from = 0, to = timeHorizon, by = resolution))

## Build a column with a different value of need. Add a run column for
## each 'Run' with a new need value
needStep <- seq(from = 0.1, to = 1.3, by = 0.01)
ntt <- data.frame()
r <- 0
for (n in needStep) {
    r = r+1
    ntt <- rbind(ntt, cbind(timeTick, Need = n, Run = r))
}

## Build the full resilience matrix for each run and pull them together
fullNtt <- data.frame()
for (run in 1:max(ntt$Run)){
    workNtt <- filter(ntt, Run == run)
    workNtt <- stepFailRecover(workNtt, 30, 75, 1, .1, .95)
    workNtt <- quotRes(workNtt)
    workNtt <- extQuotRes(workNtt, 0)
    workNtt <- resFac(tt = workNtt, tDelta = 40, initRecTime = 75,
                  finRecTime = 75, decay = 0)
    workNtt <- extResFac(tt = workNtt, tDelta = 40, initRecTime = 75,
                     finRecTime = 75, decay = 0, sigma = 0)
    workNtt <- intRes(workNtt, 1)
    fullNtt <- rbind(fullNtt, workNtt)
}
## clear out the unnecessary columns
tidyNtt <- tidyDF(fullNtt)

## Plot Need for each metric
pltMoveNeed <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
        select(-Run, -Performance, -npRatio)
    workDF <- melt(data = workDF, id = c("Time", "Need"))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Need, value,
                              group = variable,
                              color = variable)) +
        geom_line()
}
threeDpltMoveNeed <- function(df){
    workDF <- df %>%
        select(-Run, -Performance, -npRatio)
    workDF <- melt(data = workDF, id = c("Time", "Need"))
    workDF <- group_by(workDF, variable)
}

tNT <- pltMoveNeed(tidyNtt, 100)


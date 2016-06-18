## Building functions that loop the metric through different values
## to make an interesting plot

source('metrics.R')
## Build the Time column with the appropriate time ticks
timeHorizon <- 100
resolution <- .1
timeTick <- data.frame(Time = seq(from = 0, to = timeHorizon, by = resolution))


## Build the full resilience matrix. Only one is necessary because
## time horizon is already built into it with time ticks
## Set the Need to 1
timeHorRes <- constantNeed(timeTick, 1)
## Build the resilience
timeHorRes <- stepFailRecover(timeHorRes, 30, 75, 1.1, .1, 1.2)
timeHorRes <- quotRes(timeHorRes)
timeHorRes <- extQuotRes(timeHorRes, .5)
timeHorRes <- resFac(tt = timeHorRes, tDelta = 40, initRecTime = 75,
                  finRecTime = 75, decay = 0)
timeHorRes <- extResFac(tt = timeHorRes, tDelta = 40, initRecTime = 75,
                     finRecTime = 75, decay = 0, sigma = .5)
timeHorRes <- intRes(timeHorRes, 1)
## clear out the unnecessary columns
tidyNhr <- tidyDF(timeHorRes)


## Plot resilience as the time horizon changes
pltMoveTimeH <- function(df){
    workDF <- df %>%
         select(-Performance, -npRatio, -Need)
    workDF <- melt(data = workDF, id = c("Time"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   1,
                   ifelse((variable == "Rho" | variable == "extRho"),
                          2,
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 3, 0))))
    print(head(workDF))
    print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Time, value,
                              group = variable,
                              color = variable)) +
                                  geom_line() +
               facet_grid(. ~ ResType)
}

tTH <- pltMoveTimeH(tidyNhr)


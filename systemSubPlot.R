## Building functions that loop the metric through different values
## to make an interesting plot
## This is changing the value of sigma - the substitutability of 
## additional performance 

source('metrics.R')
## Build the Time column with the appropriate time ticks
timeHorizon <- 100
resolution <- 5
timeTick <- data.frame(Time = seq(from = 0, to = timeHorizon, by = resolution))
## Need set to .8 to demonstrate performance above failure
timeTick <- constantNeed(timeTick, .8)

## Build a column with a different value of need. Add a run column for
## each 'Run' with a new need value
subStep <- seq(from = 0, to = 1.0, by = 0.01)


## Kept the next four lines for reference while building the "for" loop
## for (n in needStep) {
##     r = r+1
##     ntt <- rbind(ntt, cbind(timeTick, Need = n, Run = r))
## }

## Build the full resilience matrix for each run and pull them together
fullSub <- data.frame()
for (sigma in 1:length(subStep)){
    workSub <- data.frame()
    workSub <- stepFailRecover(timeTick, 30, 75, 1, .1, .95)
    workSub$Sigma <- subStep[sigma]
    workSub <- quotRes(workSub)
    workSub <- extQuotRes(workSub, subStep[sigma])
    workSub <- resFac(tt = workSub, tDelta = 40, initRecTime = 75,
                  finRecTime = 75, decay = 0)
    workSub <- extResFac(tt = workSub, tDelta = 40, initRecTime = 75,
                     finRecTime = 75, decay = 0, sigma = subStep[sigma])
    workSub <- intRes(workSub, subStep[sigma])
    fullSub <- rbind(fullSub, workSub)
}
## clear out the unnecessary columns
tidySub <- tidyDF(fullSub)

## Plot Substitution (sigma) for each metric
pltSubNeed <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
        select(-Need, -Performance, -npRatio)
    workDF <- melt(data = workDF, id = c("Time", "Sigma"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   1,
                   ifelse((variable == "Rho" | variable == "extRho"),
                          2,
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 3, 0))))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Sigma, value,
                              group = variable,
                              color = variable)) +
                                  geom_line()  +
                                      ## May or may not want to facet
                                      ## this one. Looked pretty good
                                      ## on one plot, but for consistency
                                      ## probably fact it.
                                      facet_grid(ResType ~ .)
}

tSub <- pltMoveNeed(tidySub, 100)

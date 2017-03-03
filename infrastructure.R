## A script to pull in the data from the AnyLogic Model
mdl <- read.csv("singleRunOutput.csv")
mdl <- mdl %>% select(-Electric.Degrade) %>%
    melt(id.vars = "Time", na.rm = TRUE)

infraPlot <- ggplot(mdl, aes(Time, value)) +
    facet_wrap(~  variable, ncol = 2) +
    geom_line()

## Build the resilience matrix for each of these

mdlGroup <- group_by(mdl, variable)


## Add a need value
## Here will will make a constant need to be bound to the data.frame

El.Av = mdl %>% filter(variable == "Electricity.Availability") %>%
    select(-variable, Performance = value)
El.Av = cbind(El.Av, Need = .8) ## Stakeholder need of 0.8

r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)

simResilience <- function(TPNmatrix, resFactors){
    resMat <- quotRes(TPNmatrix)
    resMat <- extQuotRes(resMat, sigma = resFactors$sigma)
    resMat <- resFac(resMat,
                     tDelta = resFactors$tDelta,
                     decay = resFactors$decay)
    resMat <- extResFac(resMat,
                        tDelta = resFactors$tDelta,
                        decay = resFactors$decay,
                        sigma = resFactors$sigma)
    resMat <- intRes(resMat, sigma = resFactors$sigma)
    resMat <- tidyDF(resMat)
    return(resMat)
}
g <- simResilience(El.Av, r)
h <- pltMoveTimeH(g)
h

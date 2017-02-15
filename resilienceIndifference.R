## Building the first resilience indifference curves.
## First build the resilience matrix.
## Parameters: Failure level and Need level (constant)
## Constants:
## sigma = 0
## time horizon/vector 0 to 100 at increments of 1
## start level = 1
## end level = 1
## failure time = 25
## recovery time = 75
## time_delta (ESDF) = 20
## time decay = 0

time <- data.frame(endTime = 100,
                   resolution = 1)

need <- data.frame(func = "constantNeed",
                   cLevel = seq(from = 0.1,
                       to = 1.0,
                       by = .05),
                   startTime = NA,
                   slope = NA)

p <- data.frame(func = "step",
                failTime = 25,
                recTime = 75,
                preLevel = 1,
                failLevel = seq(from = 0.1,
                    to = 1,
                    by = 0.05),
                recLevel = 1)

resParams <- data.frame(tDelta = 20,
                        decay = 0,
                        sigma = 0)

indiffRes <- resLoop (time, need, p, resParams)

## Build the plottable dataframe for integral resilience

IntResInd <- indiffRes %>%
    filter(Time == 100) %>%
        select(extResilience, Time, Need, pRun, EQR, extRho)

r <- ggplot(IntResInd, aes(pRun, Need, z=extResilience)) +
    geom_raster(aes(fill = extResilience)) + geom_contour(color = "white")
q <- ggplot(IntResInd, aes(pRun, Need, z=EQR)) +
    geom_contour()
e <- ggplot(IntResInd, aes(pRun, Need, z=extRho)) +
    geom_raster(aes(fill = extRho)) + geom_contour(color = "white")

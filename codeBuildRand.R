## # Stochastic Validation
##   The goal of this is to build random profiles for input into the
##   resilience models.
## 
##   The first steps are to set the random seed and to build the time
##   column for the data frame.

source("metrics.R")
set.seed(123)
beginTime <- 0
lastTime <- 100
res <- 1

baseTime <- data.frame(endTime = lastTime, resolution = res)

## The first example will be a randomized time to failure with a constant
##     time to recover and levels of recovery. This needs to be done carefully.
##     If the TTF is greater than the total time, it should be a constant
##     performance.  df

TTF <- ceiling(rexp(n=10, rate = 1/30))

TTFdf <- data.frame(failTime = TTF,
                    recTime = TTF + 40,
                    preLevel = 1,
                    failLevel = 0.1,
                    recLevel = 1.2
                    ) %>%
                        mutate(func = "step")
## 
## ```
## TTFdf is a data.frame that my resLoop function uses to define the performance
##       vector. Now, let's run resLoop with some dummy variables for Need
##       and resilience variables.
## 
## ```{r}
r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)
needConstant <- data.frame(func = "constantNeed",
                cLevel = 0.8,
                startTime = NA,
                 slope = NA)

randomResilience <- resLoop(time = baseTime,
                            need = needConstant,
                            performance = TTFdf,
                            resFactors = r)
dim(randomResilience)

pltData <- randomResilience %>%
    filter(Time == 80) %>%
        select(QR, EQR, TQR, ETQR, Rho, extRho, statQuoResilience,
               extResilience, pRun)
pltData <- melt(pltData, id.vars = "pRun")
tPlot <- ggplot(pltData, aes(variable, value)) + geom_point()


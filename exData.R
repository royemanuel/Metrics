tL <- data.frame(endTime = c(100, 125), resolution = c(1, .5))
nL <- data.frame(func = c("constantNeed","linearNeed"),
                 cLevel = c(.8, 1.0),
                 startTime = c(NA, 55),
                 slope = c(NA, .01))
pL <- data.frame(func = c("step", "resTriangle"),
                 failTime = c(5, 50),
                 recTime = c(90, 110),
                 preLevel = c(1, 1.1),
                 failLevel = c(0, .1),
                 recLevel = c(.95, 1.7))
rL <- data.frame(tDelta = c(20, 30, 20),
                 decay = c(0, .5, 1),
                 sigma = c(0, .5, 1))


mnop <- resLoop(tL, nL, pL, rL)
qwer <- buildResMatrix(tL[2,], nL[2,], pL[2,], rL[2,])

######################################################################
## Standard values for the inputs to building resilience matrix
######################################################################
t <- data.frame(endTime = 100,
                resolution = .1)
t2 <- data.frame(endTime = 100,
                resolution = 5)
n <- data.frame(func = "constantNeed",
                cLevel = 1.0,
                startTime = NA,
                slope = NA)
p <- data.frame(func = "step",
                failTime = 10,
                recTime = 60,
                preLevel = 1,
                failLevel = .1,
                recLevel = 1)
r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)

## Data frame with a variable level of performance during failure
## from zero to one with a step failure
#######

pVaryFailStep <- data.frame(failLevel = seq(from = 0,
                                            to = .99,
                                            by = .01),
                            func = "step",
                            failTime = 10,
                            recTime = 60,
                            preLevel = 1,
                            recLevel = 1)
## Vary the failure level from 0 to .99
pVFS <- resLoop(t, n, pVaryFailStep, r)

######################################################################
## This is the same as the above, but with a larger step, so all
## can be plotted at once over time to demonstrate the insensitivity
## of the quotient resilience metric
pVaryFailStepBig <- data.frame(failLevel = seq(from = 0,
                                            to = .9,
                                            by = .3),
                            func = "step",
                            failTime = 10,
                            recTime = 60,
                            preLevel = 1,
                            recLevel = 1)
## Vary the failure level from 0 to .99
pVFSbigstep <- resLoop(t, n, pVaryFailStepBig, r)

## Plot it so it has all of the profiles on each one
pltMoveFailLevelPerfP <- function(df){
    workDF <- df %>%
         select(Time, QR, EQR, Rho, 
                extRho, statQuoResilience, extResilience, pRun)
    workDF <- melt(data = workDF, id = c("Time",  "pRun"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original")) %>%
        filter(variable == "Original") %>%
        mutate(pRun = 3 * (pRun - 1)/10)
    workDF <- rename(workDF, Resilience = value)
    print(head(workDF))
    workDF$pRun <- as.character(workDF$pRun)
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Time, Resilience,
                              group = pRun)) +
        geom_line(aes(color = pRun,
                      linetype = pRun)) +
        facet_grid(ResType ~ .) + 
        scale_linetype_discrete(name = "Fail Level") +
        scale_color_discrete(name = "Fail Level") +
        theme_bw(base_size = 12, base_family = "serif")
}

pltVFSbigstep <- pltMoveFailLevelPerfP(pVFSbigstep)

## Also plot the performance curves to go with it
perfVFS <- function(df){
    workDF <- df %>%
        select(Time, Performance, pRun)
    workDF <- melt(data = workDF, id = c("Time", "pRun"))
    workDF <- mutate(workDF, pRun = 3 * (pRun - 1)/ 10)
    workDF$pRun <- as.character(workDF$pRun)
    workDF <- rename(workDF, Performance = value)
    print(head(workDF))
    plt <- ggplot(workDF, aes(Time, Performance, group = pRun)) +
        geom_line(aes(color = pRun, linetype = pRun)) + 
        scale_linetype_discrete(name = "Fail Level") +
        scale_color_discrete(name = "Fail Level") +
        theme_bw(base_size = 12, base_family = "serif")
}

pltperfVFS <- perfVFS(pVFSbigstep)

## Data frame with a variable level of performance during failure
## from zero to one with a linear recovery
#######

pVaryFailTri <- data.frame(failLevel = seq(from = 0,
                                            to = .99,
                                            by = .01),
                            func = "resTriangle",
                            failTime = 10,
                            recTime = 60,
                            preLevel = 1,
                           recLevel = 1)

## Data frame with a variable time of recovery
## from 1 tick after the failure to 1 tick before complete
#######

pVaryRecoveryTime <- data.frame(failLevel = 0.1,
                            func = "step",
                            failTime = 10,
                            recTime = seq(from = 13,
                                          to = 99,
                                          by = 1),
                            preLevel = 1,
                            recLevel = 1)

## Vary time to recover
pVR <- resLoop(t, n, pVaryRecoveryTime, r)
## Plot Resilience by Time to recover

## This is a good plot for showing the indifference quotient resilience
## has in regard to many of the key

pltVR <- pltMoveRecovery(pVR, 100)
######################################################################


## Data.frame with variable level of constant need.
nLinearVary <- data.frame(func = "constantNeed",
                          cLevel = seq(from = 0.01,
                                       to = 1.0,
                                       by = .01),
                          cLevel = 1.0,
                          startTime = NA,
                          slope = NA)


## Data frame with variable level of sigma
##########################################
n2 <- data.frame(func = "constantNeed",
                cLevel = 0.9,
                startTime = NA,
                 slope = NA)

p2 <- data.frame(func = "step",
                failTime = 20,
                recTime = 60,
                preLevel = 1.2,
                failLevel = 0.1,
                recLevel = 1.0)

pRAMS <- data.frame(func = "step",
                failTime = 20,
                recTime = 60,
                preLevel = 1.0,
                failLevel = 0.1,
                recLevel = 1.2)

r2 <- data.frame(tDelta = 30,
                decay = 0,
                 sigma = seq(from = 0,
                     to = 1.0,
                     by = .01))

sVFS <- resLoop(t, n2, p2, r2)

sVFSplotRec <- pltSubNeed(sVFS, 80)

sVFSplotFail <- pltSubNeed(sVFS, 30)

## Data frame with variable time horizon
######################################################################
tVFS <- resLoop(t, n2, p2, r)
tVFSplot <- pltMoveTimeH(tVFS)
tVFSplot

## Data frame with variable tdelta to show its effect on Rho
######################################################################
r3 <- data.frame(tDelta = seq(from = 0,
                     to = 200,
                     by = 1),
                 decay = 0,
                 sigma = 0)
tdVFS <- resLoop(t, n2, p2, r3)
######################################################################
##  This is a nice small data set to play around with if necessary
######################################################################
smallSet <- resLoop(t, n2, p2, r)

pNP <- pltNeedPerf(smallSet)
pNP

######################################################################
## Using Linear Recovery
######################################################################
pLin <- data.frame(func = "resTriangle",
                failTime = 20,
                recTime = 60,
                preLevel = 1.2,
                failLevel = 0.1,
                recLevel = 1.0)

## Time Horizon with Linear Recovery
THLR <- resLoop(t, n2, pLin, r)
THLRplot <- pltMoveTimeH(THLR)
THLRplot

## Vary sigma with Linear recovery
SHLR <- resLoop(t, n2, pLin, r2)
SHLRplotRec <- pltSubNeed(SHLR, 80)
SHLRplotFail <- pltSubNeed(SHLR, 30)

## Vary need with linear recovery
NHLR <- resLoop(t2, nLinearVary, pLin, r)
NHLRplotRec <- pltMoveNeed(NHLR, 80)
NHLRplotFail <- pltMoveNeed(NHLR, 30)

## Vary the constant need from 0 to 1.0
nVFS <- resLoop(t, nLinearVary, p2, r)
nVFSplotRec <- pltMoveNeed(nVFS, 80)
nVFSplotFail <- pltMoveNeed(nVFS, 30)
######################################################################
## This is a plot of the linear performace
######################################################################

smallSetLin <- resLoop(t2, n2, pLin, r)

pNPLin <- pltNeedPerf(smallSetLin)

######################################################################
## Building an example plot where values are pulled                 ##
######################################################################

examFail <- list(func = "custom",
                 Performance = c(rep(1, 10),
                     seq(.85, .25, -.15),
                     rep(.25, 5),
                     seq(.25, .4, .015),
                     seq(.42, .6, .005),
                     seq(.61, .75, .02),
                     seq(.76, .85, .01),
                     seq(.855, .9, .005),
                     rep(.9, 5)))
examTime <- list(endTime = 100, resolution = 1)
examNeed <- list(func = "constantNeed",
                 cLevel = .8)
examRL <- list(sigma = .5, tDelta = 20, decay =  .1)
exampData <- buildResMatrix(examTime, examNeed, examFail, examRL)
examPlot <- pltPerf(exampData)
examPlot <- examPlot + ylim(0, 1)

## Checking that the extended metric pulls are equal to the original
## metrics when they should be
checkExtP <- list(func = "custom",
                 Performance = c(rep(1, 10),
                     seq(.85, .25, -.15),
                     rep(.25, 5),
                     seq(.25, .4, .015),
                     seq(.42, .6, .005),
                     seq(.61, .75, .02),
                     seq(.76, .85, .01),
                     seq(.855, .9, .005),
                     rep(1, 5)))

unitaryNeed <- list(func = "constantNeed", cLevel = 1.0)
checkExtMet <- buildResMatrix(examTime, unitaryNeed, checkExtP, examRL)



exPLneed <- pltMoveTimeH(exampData)
## Add the points where the data is pulled.
points <- data.frame(Time = exampData$QR_Td[1],
                 Performance = select(filter(exampData, Time == QR_Td[1]),
                     Performance),
                 name = 'BekTd')

rbind(points, Time = )

examPlot <- examPlot ## +
##     geom_point(data = points,
##                aes(x = Time, y = Performance)) ## +
##                    geom_segment(data = points, aes(x = Time,
##                                   y = Performance,
##                                   xend = Time,
##                                     yend = 0,
##                                                    linetype = name))
## examPlot <- examPlot + geom_segment(data = points, aes(x = Time,
##                                         y = Performance,
##                                         xend = 0,
##                                         yend = Performance,
##                                         linetype = name))
## +
##     geom_text(data = td, aes(x = Time, y = Performance), label = td$name)

examPlot

rbind(buildPoints, )
p <- c(rep(1, 10),
       seq(.85, .25, -.15),
       rep(.25, 5),
       seq(.25, .4, .015),
       seq(.42, .6, .005),
       seq(.61, .75, .02),
       seq(.76, .85, .01),
       seq(.855, .9, .005),
       rep(.9, 5))

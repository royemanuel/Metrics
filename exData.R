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
## Vary the failure level from 0 to .99
pVFS <- resLoop(t, n, pVaryFailStep, r)

## Vary to
nLinearVary <- data.frame(func = "constantNeed",
                          cLevel = seq(from = 0.01,
                                       to = 1.0,
                                       by = .01),
                          cLevel = 1.0,
                          startTime = NA,
                          slope = NA)

## Vary the constant need from 0 to 1.0
nVFS <- resLoop(t, nLinearVary, p, r)
nVFSplotRec <- pltMoveNeed(nVFS, 80)
nVFSplotFail <- pltMoveNeed(nVFS, 30)

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
                failLevel = .1,
                recLevel = 1.0)
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


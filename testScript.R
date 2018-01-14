######################################################################
## Script to check the results from everything.
######################################################################

## Define common inputs that should be easy to calculate by hand
tTest <- data.frame(Time = 0:10)
nTestSingle <- data.frame(func ="constantNeed",
                    cLevel = c(.5),
                    startTime = NA,
                    slope = NA)
sTestSingle <- data.frame(tDelta = 0,
                    decay = 0,
                    sigma = c(1))
srTestSingle <- stepFailRecover(tTest, 2, 7, 1, 0.25, 1)

srTestDF <- data.frame(func = "step",
                       failTime = 2,
                       recTime = 7,
                       preLevel = 1.0,
                       failLevel = 0.25,
                       recLevel = 1.0)
## What should the values be near (they won't be exact because of
## triangles to connect the performance over time)
tDF <- data.frame(endTime = 10,
                  resolution = 1)

resTestRFD <- resilienceFromData(srTestSingle, nTestSingle, sTestSingle, 10)
resTestRL <- resLoop(tDF, nTestSingle, srTestDF, sTestSingle)

## Checking the time issues I am worried about
tTestL <- data.frame(Time = 0:100)
nTestSingleL <- data.frame(func ="constantNeed",
                    cLevel = c(.5),
                    startTime = NA,
                    slope = NA)
sTestSingleL <- data.frame(tDelta = 0,
                    decay = 0,
                    sigma = c(0))
srTestSingleL <- stepFailRecover(tTestL, 20, 70, 1, 0.25, 1)

srTestDFL <- data.frame(func = "step",
                       failTime = 20,
                       recTime = 70,
                       preLevel = 1.0,
                       failLevel = 0.25,
                       recLevel = 1.0)

tDFL <- data.frame(endTime = 100,
                  resolution = 1)

resTestRFDL <- resilienceFromData(srTestSingleL, nTestSingleL, sTestSingleL, 100)
resTestRLL <- resLoop(tDFL, nTestSingleL, srTestDFL, sTestSingleL)

diffDF <- data.frame(Performance = resTestRFD$Performance - resTestRL$Performance,
                     Need = resTestRFD$Need - resTestRL$Need,
                     QR = resTestRFD$QR - resTestRL$QR,
                     EQR = resTestRFD$EQR - resTestRL$EQR,
                     TQR = resTestRFD$TQR - resTestRL$TQR,
                     ETQR = resTestRFD$ETQR - resTestRL$ETQR,
                     statQuoResilience = resTestRFD$statQuoResilience - resTestRL$statQuoResilience,
                     extResilience = resTestRFD$extResilience - resTestRL$extResilience,
                     Rho = resTestRFD$Rho - resTestRL$Rho,
                     extRho = resTestRFD$extRho - resTestRL$extRho)


kMSFthTimeandPerfFixed <- multScenarioFast("RecLevel.csv", need, rf)
k <- read.csv("RecLevel.csv")

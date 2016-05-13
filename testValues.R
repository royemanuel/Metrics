## Building test values in R to use the metrics on

timeHorizon <- 100
resolution <- 5
timeTick <- data.frame(Time = seq(from = 0, to = timeHorizon, by = resolution))

testFrame1 <- stepFailRecover(timeTick, 30, 75, 1.1, .1, .95)

testFrame1 <- constantNeed(testFrame1, 1)

testFrame1 <- quotRes(testFrame1)

testFrame1 <- extQuotRes(testFrame1, 0)

testFrame1 <- resFac(testFrame1, 30, 75, 75, 40, 0)

testFrame1 <- extResFac(testFrame1, 30, 75, 75, 40, 0, 1)

testFrame1 <- resFac(testFrame1, 30, 75, 75, 40, 0, 1)

testFrame0 <- stepFailRecover(timeTick, 30, 75, 1, .1, .95)

testFrame0 <- constantNeed(testFrame0, 1)

testFrame0 <- quotRes(testFrame0)

testFrame0 <- extQuotRes(testFrame0, 0)

testFrame0 <- resFac(tt = testFrame0, tDelta = 40, initRecTime = 75,
                     finRecTime = 75, decay = 0)

testFrame0 <- extResFac(tt = testFrame0, tDelta = 40, initRecTime = 75,
                        finRecTime = 75, decay = 0, sigma = 0)

testFrame0 <- intRes(testFrame0, 1)
tf0a <- tidyDF(testFrame0)
tfA <- stepFailRecover(timeTick, 30, 75, 1, .1, .95)
tfA <- constantNeed(tfA, .8)
tfA <- quotRes(tfA)
tfA <- extQuotRes(tfA, 0)
tfA <- resFac(tfA, 30, 75, 75, 40, 0)
tfA <- extResFac(tfA, 30, 75, 75, 40, 0, 1)
tfA <- intRes(tfA, 1)
tfA <- tidyDF(tfA)

tt1 <- data.frame(Time = seq(from = 0, to = 100, by = .01))
tfB <- stepFailRecover(tt1, 30, 75, 1, .1, .95)
tfB <- constantNeed(tfB, .8)
tfB <- quotRes(tfB)
tfB <- extQuotRes(tfB, 0)
tfB <- resFac(tfB, 30, 75, 75, 40, 0)
tfB <- extResFac(tfB, 30, 75, 75, 40, 0, 1)
tfB <- intRes(tfB, 1)
tfB <- tidyDF(tfB)
trBm <- melt(tfB, id.vars = "Time")

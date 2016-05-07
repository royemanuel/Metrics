## Building test values in R to use the metrics on

timeHorizon <- 100
resolution <- 5
timeTick <- data.frame(Time = seq(from = 0, to = timeHorizon, by = resolution))

testFrame0 <- stepFailRecover(timeTick, 30, 75, 1.1, .1, .95)

testFrame0 <- constantNeed(testFrame0, .98)

testFrame0 <- quotRes(testFrame0)

testFrame0 <- extQuotRes(testFrame0, 0)

testFrame0 <- resFac(testFrame0, 30, 75, 75, 40, 0)

testFrame0 <- extResFac(testFrame0, 30, 75, 75, 40, 0, 0)

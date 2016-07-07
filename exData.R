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

## Build the entire resilience matrix. This needs to include all of the
## inputs for every resilience function as well as for the need and
## performance functions. These will be included by making a vector for
## each one of them that is unpacked in the function itself. Then I will
## run through an if-else or a switch type function to make it happen.
buildResMatrix <- function(timeList, needList, perfList, resList){
    ## a time vector uses an endTime and a resolution
    resMat <- timeColumn(timeList$endTime, timeList$resolution)
    print("time done")
    print( head(resMat))
    resMat <- switch(needList$func,
                     constantNeed = constantNeed(resMat, needList$cLevel))
    print("need done")
    print( head(resMat))
    resMat <- switch(perfList$func,
                     step = stepFailRecover(resMat,
                         perfList$failTime,
                         perfList$recTime,
                         perfList$preLevel,
                         perfList$failLevel,
                         perfList$recLevel),
                     resTriangle = resTriangle(resMat,
                         perfList$failTime,
                         perfList$recTime,
                         perfList$preLevel,
                         perfList$failLevel,
                         perfList$recLevel))
    print("performance done")
    print(head(resMat))
    resMat <- quotRes(resMat)
    print("QR done")    
    resMat <- extQuotRes(resMat, 0)
    print("EQR done")
    resMat <- resFac(tt = resMat,
                     tDelta = resList$tDelta,
                     ## initRecTime = resList$initRecTime,
                     ## finRecTime = resList$finRecTime,
                     decay = resList$decay)
    print("RF done")
    resMat <- extResFac(tt = resMat,
                        tDelta = resList$tDelta,
                        ## initRecTime = resList$initRecTime,
                        ## finRecTime = resList$finRecTime,
                        decay = resList$decay,
                        sigma = resList$sigma)
    print("ERF done")
    resMat <- intRes(resMat,
                     sigma = resList$sigma)
    print("IntRes done")
}

t <- list(endTime = 100,
          resolution = 1)

n <- list(func = "constantNeed",
          cLevel = 0.9)
p <- list(func = "step",
          failTime = 20,
          recTime = 80,
          preLevel = 1.0,
          failLevel = .1,
          recLevel = .95)
r <- list(tDelta = 30,
          decay = 0,
          sigma = 0)
k <- buildResMatrix(t, n, p, r)

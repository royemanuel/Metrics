## Build the entire resilience matrix. This needs to include all of the
## inputs for every resilience function as well as for the need and
## performance functions. These will be included by making a vector for
## each one of them that is unpacked in the function itself. Then I will
## run through an if-else or a switch type function to make it happen.
buildResMatrix <- function(timeList, needList, perfList, resList){
    ## a time vector uses an endTime and a resolution
    resMat <- timeColumn(timeList$endTime, timeList$resolution)
    resMat <- switch(needList$func,
                     constantNeed = constantNeed(resMat, needList$cLevel))
    resMat <- switch(perfList,
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
    resMat <- quotRes(resMat)
    resMat <- extQuotRes(resMat, 0)
    resMat <- resFac(tt = resMat,
                     tDelta = resList$tDelta,
                     initRecTime = resList$initRecTime,
                     finRecTime = resList$finRecTime,
                     decay = resList$decay)
    resMat <- extResFac(tt = resMat,
                        tDelta = resList$tDelta,
                        initRecTime = resList$initRecTime,
                        finRecTime = resList$finRecTime,
                        decay = resList$decay,
                        sigma = resList$sigma)
    resMat <- intRes(resMat,
                     sigma = resList$sigma)
}

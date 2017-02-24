## Script to find the difference between the status quo metric and the
## extended metric
## resilienceMatrix is the input matrix. We will use the column names
## used in the resLoop function. Should be straightforward to make a
## matrix of differences only

diffMatrix <- function(resilienceMatrix){
    rMat <- resilienceMatrix %>%
        select(Time, Need, Performance, pRun, QR, EQR, Rho, extRho, Sigma,
               Decay, statQuoResilience, extResilience) %>%
                   mutate(QRDiff = QR - EQR,
                          RhoDiff = Rho - extRho,
                          IntResDiff = statQuoResilience - extResilience)
}

d <- diffMatrix(indiffRes)
dInt <- filter(d, !is.na(IntResDiff))
qInt <- filter()
## clean up the data set


r <- ggplot(d, aes(pRun, Need, z=IntResDiff)) + 
    geom_raster(aes(fill = IntResDiff)) + geom_contour()
q <- ggplot(d, aes(pRun, Need, z=EQR)) +
    geom_contour()
e <- ggplot(d, aes(pRun, Need, z=extRho)) +
    geom_raster(aes(fill = extRho)) + geom_contour(color = "white")

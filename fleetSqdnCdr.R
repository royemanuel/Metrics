## Calculate the resilience satisfied students on a five year basis
## and the resilience of graduated students on a five year basis


## This takes in one run of aircrew data and calculates the resilience
## for each squadron commander (5 year period)
COsatRes <- function(DF, nd, dtis, orders){
    wDF <-
        DF %>%
        filter(Disp == "G")
    wDF <- qrtrly_sat(DF, dtis)
    stTime <- wDF$qrtr[1]
    wDF <- wDF %>% mutate(Need = nd,
                          SqCO = floor(qrtr - stTime) %/% orders) 
    ## Hard code the chi values because it is zero always for availability
    wDF <- modifyPerformance(wDF, c(0), c(0)) %>%
        mutate(modPerf = ifelse(diff < 0, modPerf, Need))
    wDF <- wDF %>%
        split(.$SqCO) %>%
        map(assignGroupFast) %>%
        map(step_EIR) %>%
        bind_rows(.) %>%
        gather(SqCO, Resilience)
}



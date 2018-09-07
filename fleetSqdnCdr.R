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
    wDF <-
        modifyPerformance(wDF, c(0), c(0)) %>%
        mutate(modPerf = ifelse(diff < 0, modPerf, Need))
    wDF <- wDF %>%
        split(.$SqCO) %>%
        map(assignGroupFast) %>%
        map(step_EIR) %>%
        bind_rows(.) %>%
        gather(SqCO, Resilience)
}

## Takes in one sked file and puts out the resilience for each CO
COgradRes <- function(DF, ND, Xpre, Xpost, SRG, orders){
    wDF <-
        qrtrly_grads(DF) %>%
        mutate(qrtr = quarter(as_datetime(3600 *
                                          Time +
                                          make_datetime(2005, 1, 1, 8)),
                              with_year = TRUE))
    stTime <- wDF$qrtr[1]
    wDF <-
        wDF %>%
        mutate(SqCO = floor(qrtr - stTime) %/% orders)
    ## Anchor to the time the simulation begins to count the COs
    if (SRG){
        wDF <-
            wDF %>%
            mutate(Need = ifelse(Time > 105120, ND + 10, ND),
                   Need = ifelse(Time > 122640, Need - 10, Need))
    } else {
        wDF <-
            wDF %>%
            mutate(Need = ND)
    }
    wDF <- modifyPerformance(wDF, Xpre, Xpost) %>%
        mutate(modPerf = ifelse(diff < 0, modPerf, Need))
    wDF <-
        wDF %>%
        split(.$SqCO) %>%
        map(assignGroupFast) %>%
        map(step_EIR) %>%
        bind_rows(.)  %>%
        gather(SqCO, Resilience)
}



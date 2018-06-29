library("tidyverse")
library("lubridate")
qrtrly_grads <- function(df){
    df <-
        df %>%
        mutate(date = as_datetime(3600 * Time +
                                  make_datetime(2005, 1, 1, 8))) %>%
        mutate(qrtr =quarter(date, with_year = TRUE)) %>%
        group_by(qrtr) %>%
        filter(date == max(date))
    df <-
        df %>%
        ungroup(df) %>%
        mutate(Grads_in_quarter = graduates - replace(lag(graduates,1),
                                                      is.na(lag(graduates,1)),
                                                      0),
               Attrits_in_quarter = attrites - replace(lag(attrites,1),
                                                      is.na(lag(attrites,1)),
                                                      0)) %>%
        select(Time, Grads_in_quarter, Attrits_in_quarter) %>%
        gather(Infrastructure, Performance, -Time)
}

assignGroup <- function(DF){
    DF <- DF %>% mutate(diff = round(Performance - Need, 2))
    DF$Grp <- 1
    DF_output <- tibble()
    DF$Infrastructure <- as.factor(DF$Infrastructure)
    inf_values <- unique(DF$Infrastructure)
    inf_number <- length(inf_values)
    ## pb <- txtProgressBar(min = 0, max = length(unique(DF$Run)), style = 3)
    for (run in 1:length(unique(DF$Run))){
        DF_by_run <-
            DF %>%
            filter(Run == unique(DF$Run)[run])
        ## print(run)
        DF_inf_grp <- tibble()
        for (i in 1:inf_number){
            grp <- 1
            DF_inf <-
                DF_by_run %>%
                filter(Infrastructure == inf_values[i])
            s <- sign(DF_inf$diff[1])
            for (r in 1:nrow(DF_inf)){
                if (sign(s) == sign(DF_inf$diff[r])){
                    DF_inf$Grp[r] <- grp
                } else {
                    grp <- grp + 1
                    s <- sign(DF_inf$diff[r])
                    ##print(grp)
                    DF_inf$Grp[r] <- grp
                }
                DF_inf
            }
            DF_inf_grp <- bind_rows(DF_inf_grp, DF_inf)
            cat("\r", inf_values[i], run)
        }
        ## setTxtProgressBar(pb, run)
        DF_output <- bind_rows(DF_output, DF_inf_grp)
    }
    print("groups assigned")
    DF_output <- endcap_group(DF_output)
    DF_output
}

pullChunk <- function(DF, preIntSub, postIntSub){
    for (r in 1:nrow(DF)){
        while(sign(r) >= 0){            
        }
        startRow <- r - length(preIntSub)
        while(sign(r) <= 0){
        }
        endRow
    }
}

## Building a method to modify the performance value to satisfy the
## need. DF is the data frame with Time, Category, Performance, and Need
## defined. preIntSub is the intertemporal substitutability vector for
## excess performance prior to the failure. postIntSub is the i.s. vector
## for excess performance after the failure

modifyPerformance <- function(DF, preIntSub, postIntSub){
    DF <-
        DF %>%
        mutate(diff = Performance - Need, modPerf = Performance)
    ## print(DF)
    preTime <- length(preIntSub)
    postTime <- length(postIntSub)
    ## load in the surplus before shortfall
    for (r in 1:nrow(DF)){
        ## Find the first row that is less than zero
        if (DF$diff[r] < 0){
            if (r < preTime){
                preSteps <- r
                ##preIntSub <- preIntSub[1:preSteps]
            } else {
                preSteps <- preTime
            }
            ## print(preSteps)
            surplus <-rev(DF$diff[(r-preSteps):(r-1)])
            ## print(surplus)
            for (pre in 1:preSteps){
                if(DF$diff[r] < 0 & sum(surplus) > 0){
                    xferAvail <- surplus[pre] * preIntSub[pre]
                    ## print(paste("xferAvail", xferAvail))
                    ## First case is when you have more surplus than
                    ## shortfall
                    xRow <- r - pre
                    ## print(paste("xRow", xRow))
                    if (xferAvail > abs(DF$diff[r])){
                        DF$diff[xRow] <- (xferAvail + DF$diff[r]) /
                            preIntSub[pre]
                        DF$modPerf[xRow] <- DF$modPerf[xRow] +
                            DF$diff[r] / preIntSub[pre]
                        DF$diff[r] <- 0
                        DF$modPerf[r] <- DF$Need[r]
                        ## print("case 1")
                        ## print(paste("pre", pre))
                        ## print(paste("r", r))
                        ## print(DF[(xRow):r,])
                    } else {
                        ## second case is when you have more shortfall
                        ## than surplus
                        DF$diff[xRow] <- 0
                        DF$modPerf[xRow] <- DF$modPerf[xRow] - surplus[pre] 
                        DF$diff[r] <- xferAvail + DF$diff[r]
                        DF$modPerf[r] <- DF$modPerf[r] + xferAvail
                        ## print("case 2")
                        ## print(paste("pre", pre))
                        ## print(paste("r", r))
                        ## print(DF[(xRow):r,])
                    }
                }
            }
        }
    }
    invDF <-
        DF %>%
        arrange(-row_number())
    ## print(invDF)
    for (r in 1:nrow(invDF)){
        ## Find the first row that is less than zero
        if (invDF$diff[r] < 0){
            if (r < postTime){
                postSteps <- r
                ##postIntSub <- postIntSub[1:postSteps]
            } else {
                postSteps <- postTime
            }
            ## print(postSteps)
            surplus <-rev(invDF$diff[(r-postSteps):(r-1)])
            ## print(surplus)
            for (post in 1:postSteps){
                if(invDF$diff[r] < 0 & sum(surplus) > 0){
                    xferAvail <- surplus[post] * postIntSub[post]
                    ## print(paste("xferAvail", xferAvail))
                    ## First case is when you have more surplus than
                    ## shortfall
                    xRow <- r - post
                    ## print(paste("xRow", xRow))
                    if (xferAvail > abs(invDF$diff[r])){
                        invDF$diff[xRow] <- (xferAvail + invDF$diff[r]) /
                            postIntSub[post]
                        invDF$modPerf[xRow] <- invDF$modPerf[xRow] +
                            invDF$diff[r] / postIntSub[post]
                        invDF$diff[r] <- 0
                        invDF$modPerf[r] <- invDF$Need[r]
                        ## print("case 1")
                        ## print(paste("post", post))
                        ## print(paste("r", r))
                        ## print(invDF[(xRow):r,])
                    } else {
                        ## second case is when you have more shortfall
                        ## than surplus
                        invDF$diff[xRow] <- 0
                        invDF$modPerf[xRow] <- invDF$modPerf[xRow] - surplus[post] 
                        invDF$diff[r] <- xferAvail + invDF$diff[r]
                        invDF$modPerf[r] <- invDF$modPerf[r] + xferAvail
                        ## print("case 2")
                        ## print(paste("post", post))
                        ## print(paste("r", r))
                        ## print(invDF[(xRow):r,])
                    }
                }
            }
        }
    }
    finalDF <-
        invDF %>%
        arrange(-row_number())
    return(finalDF)
}


qrtrly_EIR <- function(df){
    
    for (i in seq_along(df)){
        dfi = i * 5
    }
}





















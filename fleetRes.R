library("tidyverse")
library("lubridate")
######################################################################
## Function to condition the graduation per quarter for resilience
## analysis

qrtrly_grads <- function(DF){
    DF <-
        DF %>%
        mutate(date = as_datetime(3600 * Time +
                                  make_datetime(2005, 1, 1, 8))) %>%
        mutate(qrtr =quarter(date, with_year = TRUE)) %>%
        group_by(qrtr) %>%
        filter(date == max(date))
    DF <-
        DF %>%
        ungroup(DF) %>%
        mutate(Grads_in_quarter = graduates - replace(lag(graduates,1),
                                                      is.na(lag(graduates,1)),
                                                      0),
               Attrits_in_quarter = attrites - replace(lag(attrites,1),
                                                      is.na(lag(attrites,1)),
                                                      0)) %>%
        select(Time, Grads_in_quarter) %>%
        gather(Category, Performance, -Time)
}

######################################################################
## Function to condition the flightline data for resilience
## Metric is the upAircraft at the start of the flight schedule each day\
## Note: when calculating resilience, chi value is zero for availability
## yesterday's availability has no impact on today's
opAvail <- function(DF){
    DF <-
        DF %>%
        group_by(Day) %>%
        filter(Time == min(Time)) %>%
        mutate(Performance = upAircraft /
                   (flightLine + SLEPlist + boneYard)) %>%
        select(Day, Performance)
}

######################################################################
## Functions to condition the aircrew data for resilience analysis
qrtrly_sat <- function(DF, desiredTIS){
    sumDF <-
        DF %>%
        filter(Disp == "G") %>%
        mutate(Time =  as.integer(exitDate),
               TimeInSqdn = as.double(TimeInSqdn),
               ## number of hour
               indRes = desiredTIS / TimeInSqdn,
               date = as_datetime(3600 * Time +
                                  make_datetime(2005, 1, 1, 8)),
               qrtr = quarter(date, with_year = TRUE),
               satisfaction = ifelse(indRes >= 1, 1, 0)) %>%
        group_by(qrtr) %>%
        summarise(numGrad = n(),
                  satisfaction = sum(satisfaction),
                  Performance = satisfaction / numGrad)
    return(sumDF)
}

######################################################################
## Building a method to modify the performance value to satisfy the
## need. DF is the data frame with Time, Category, Performance, and Need
## defined. preIntSub is the intertemporal substitutability vector for
## excess performance prior to the failure. postIntSub is the i.s. vector
## for excess performance after the failure

######################################################################

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
            if (r <= preTime){
                preSteps <- r - 1
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
            if (r <= postTime){
                postSteps <- r - 1
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
                        invDF$modPerf[xRow] <- invDF$modPerf[xRow] -
                            surplus[post]
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

## Think about building the chi and the non-chi version, that is, when
## chi equals one across the board. Then you don't have to use the
## modify performance function. We are just averaging
step_EIR <- function(DF){
    DF <-
        DF %>%
        group_by(grp) %>%
        summarise(num = n(),
                  sumPerf = sum(modPerf),
                  sumNeed = sum(Need)) %>%
        mutate(secRes = sumPerf * num / sumNeed)
    res <- sum(DF$secRes) / sum(DF$num[])
}



assignGroupFast <- function(DF){
    DF <-
        DF %>%
        mutate(grp = ifelse(sign(diff) < 0, -1, 1))
    groupVector <- rle(DF$grp)
    g <- c()
    for(i in 1:length(groupVector$values)){
        g <- c(g, rep(i, groupVector$lengths[i]))
    }
    DF$grp <- g
    return(DF)
}


AoRes <- function(DF, nd){
    wDF <-
        opAvail(DF) %>%
        ungroup(wDF)%>%
        mutate(Time = Day,
               Need = nd)
    ## Hard code the chi values because it is zero always for availability
    wDF <- modifyPerformance(wDF, c(0), c(0))
    wDF <-
        assignGroupFast(wDF) %>%
        ## Another hard code to ensure we have no residual substitutability
        mutate(modPerf = ifelse(diff < 0, modPerf, Need))
    step_EIR(wDF)
}

satRes <- function(DF, nd, dtis){
    wDF <- qrtrly_sat(DF, dtis)
    wDF <- wDF %>% mutate(Need = nd)
    ## Hard code the chi values because it is zero always for availability
    wDF <- modifyPerformance(wDF, c(0), c(0))
    wDF <-
        assignGroupFast(wDF) %>%
        ## Another hard code to ensure we have no residual substitutability
        mutate(modPerf = ifelse(diff < 0, modPerf, Need))
    step_EIR(wDF)
}

gradRes <- function(DF, nd, chiPre, chiPost, surge){
    wDF <- qrtrly_grads(DF)
    lngth <- dim(DF)[1]
    if (surge){
        wDF <-
            wDF %>%
            mutate(Need = ifelse(Time > 105120, nd + 10, nd),
                   Need = ifelse(Time > 122640, Need - 10, Need))
    } else {
        wDF <-
            wDF %>%
            mutate(Need = nd)
    }
    ## wDF <-
    ##     wDF %>%
    ##     mutate(Need = ifelse(surge,
    ##                   ifelse(Time > 105120 & Time < 122640,
    ##                          nd + 10, nd),
    ##                   nd))
    # print(unique(wDF$Need))
    wDF <- modifyPerformance(wDF, chiPre, chiPost)
    wDF <-
        assignGroupFast(wDF) %>%
        mutate(modPerf = ifelse(diff < 0, modPerf, Need))
    step_EIR(wDF)
}
######################################################################
######################################################################
## Chunk attempt. Failed. May revive. not now though. Goal was to
## make the closest failure first filled by pre-surplus then post-surplus
## then go to the next failure.
######################################################################
######################################################################


## pullChunk <- function(DF, preIntSub, postIntSub){
##     DF <-
##         DF %>%
##         mutate(diff = Performance - Need,
##                grp = ifelse(sign(diff) >=0, 1, -1),
##                modPerf = Performance)
##     chnklngth <- rle(DF$grp)$lengths
##     chnkval <- rle(DF$grp)$values
##     DFchunk <- assignGroupFast(DF)
##     dflist <- list()
##     for (g in 1:max(DFchunk$grp)){
##         print(paste("chunk", g))
##         wDF <-
##             DFchunk %>%
##             filter(grp >= g) %>%
##             filter(grp <= g + 2)
##         wDF <- modifyPerformance(wDF, preIntSub, postIntSub)
##         print(paste("wDF",unique(wDF$grp)))
##         topDF <-
##             DFchunk %>%
##             filter(grp < g)
##         botDF <-
##             DFchunk %>%
##             filter(grp > g + 2)
##         print(paste("botDf", unique(botDF$grp)))
##         DFchunk <- bind_rows(topDF, wDF, botDF) %>%
##             arrange(grp)
##         print(DFchunk)
##     }
##     return(DFchunk)
## }

##     j <- 1
##     for (i in 1:length(chnklngth)){
##         if (chnkval[i] > 0){
##             j <- j + chnklngth[i]
##         } else {
##         }
##     }
##     chnk <- 1
##     while(chnk < dim(DF)[1]){
##         while(sign(DF$diff[chnk]) >= 0){
##             chnk <- chnk + 1
##             if(chnk == dim(DF)[1]){
##                 break
##             }
##         }
##         startRow <- chnk - length(preIntSub)
##         while(sign(DF$diff[chnk]) <= 0){
##             chnk <- chnk + 1
##             if (chnk == dim(DF)[1]){
##                 chnk <- chnk - length(postIntSub)
##                 break
##             }
##         }
##         endRow <- chnk + length(postIntSub)
##         DFchnk <- DF[startRow:endRow,]
##         DFchnk <- modifyPerformance(DFchnk, preIntSub, postIntSub)
##         DF[startRow:endRow,] <- DFchnk[startRow:endRow,]
##     }
##     return(DF)
## }

## build the need profiles for the hurricane scenarios

## This will take in the tidy-fied data from the hurricange ingestion
## sequence. That way we can split by runs.

## This is a first order demonstration, so keep this simple and flexible

## Let's say that Emergency services see a rise in demand that decays
## as the storm goes on, and that it is dependent upon the hurricane strike

build_need <- function(DF,
                       stormlist,
                       system,
                       # delay, # days
                       baseline,
                       year2val
                       #perturb_level,
                       ##recovertime
                       ){ # days
    DF <-
        DF %>%
        filter(Infrastructure == system)
    DF_holder <- tibble()
    ## This is temporary until I build a recovery time and perturbation
    ## level that is dependent upon storm strength
    ## recovertime <- recovertime * 1440
    ## delay <- delay * 1440
    pb2 <- txtProgressBar(min = 0, max = length(unique(DF$Run)), style = 3)
    for (run in 1:length(unique(DF$Run))){
        DF_run<-
            DF %>%
            filter(Run == run)
        need_vector <- seq(from = baseline,
                           to = year2val,
                           length.out = 1051200)
        ## print(length(need_vector))
        storms_run <-
            stormlist[run, 2:11] %>%
            gather() %>%
            filter(value < 1051200)
        rem_storms <-  2 * dim(storms_run)[1] - 10
        storms_run <- storms_run[1:rem_storms,]
        storm_strengths <-
            storms_run %>%
            filter(value < 6)
        biggest_storm <- max(storm_strengths$value)
        ## print(paste("Run ", run, ", BS ", biggest_storm))
        ## print(storms_run)
        ## Hardcoding this for simplicity
        num_storm_run <- dim(storms_run)[1] / 2
        ## print(paste("storms in this row", num_storm_run))
        for (sr in 1:num_storm_run){
            time <- storms_run$value[sr]
            strength <- storms_run$value[sr + num_storm_run]
            ## Need to vary the recovery time and the perturbation level by
            ## strength of the storm
            p_vec <- calc_strength_factors(system, strength)
            ## p_vec[1] is the time to recover (days)
            ## p_vec[2] is the perturbation level
            ## p_vec[3] is the delay time
            recovertime <- p_vec[1] * 1440
            perturb_level <- p_vec[2]
            delay <- p_vec[3] * 1440
            # print(sr)
            abs_rec_time <- time + (delay + recovertime) 
            start_replace <- time + delay 
            ## Need to handle the case when demand does not recover
            ## before the two year point
            if (time + delay > 1051200){
                break
            } else if (abs_rec_time > 1051200){
                end_replace <- 1051200
                rec_level_future <- (year2val - baseline) / 1051200 *
                    (time + delay + recovertime)
                demand_vec <- seq(from = perturb_level,
                                  to = rec_level_future,
                                  length.out = recovertime )
                time_to_end <- 1051200 - (time + delay) + 1
                demand_vec <- demand_vec[1:time_to_end]
             } else {
                end_replace <- start_replace + recovertime  - 1
                rec_level <- need_vector[time + (delay + recovertime) ]
                ## print(rec_level)
                demand_vec <- seq(from = perturb_level,
                                  to = rec_level,
                                  length.out = recovertime )
                ## print("yup")
             }
            if(length(demand_vec) > 0){
                need_vector[start_replace:end_replace] <- demand_vec
            }
            t_vec <- seq(from = 1, to = length(need_vector), by = 1)
            need_tbl <- as.tibble(bind_cols(Need = need_vector,
                                            Time = t_vec))
            need_tbl <- filter(need_tbl, Time %% 240 == 0)
            ## print(need_tbl)
        }
        setTxtProgressBar(pb2, run)
        DF_run <- inner_join(DF_run, need_tbl, by = "Time") %>%
            mutate(Biggest_Storm = biggest_storm,
                   Number_Storms = num_storm_run)
        DF_holder <- bind_rows(DF_holder, DF_run)
    }
    DF_holder
}

## calc_strength_factors <- function(strength, system){
##     switch(system,
##            Transportation_Function =
##                if(strength == 1){
##                    ## sf[1] is the time to recover (days)
##                    ## sf[2] is the perturbation level
##                    ## sf[3] is the delay time
##                    ttr <- 0
##                    pl <- 0
##                    dt <- 0
##                    c(ttr, pl, dt)
##                } else if (strength < 4){
##                    ttr <- 3
##                    pl <- 1.2
##                    dt <- -3
##                    c(ttr, pl, dt)
##                } else {
##                    ttr <- 3
##                    pl <- 2.0
##                    dt <- -3.5
##                    c(ttr, pl, dt)
##                },
##            Emergency_Services_Functionality =
##                if(strength == 1){
##                    ttr <- 2
##                    pl <- 1.2
##                    dt <- .5
##                    c(ttr, pl, dt)
##                } else if (strength < 4) {
##                    ttr <- 8
##                    pl <- 1.4
##                    dt <- .5
##                    c(ttr, pl, dt)
##                } else {
##                    ttr <- 15
##                    pl <- 1.5
##                    dt <- .5
##                    c(ttr, pl, dt)
##                },
##            Electricity_Availability =
##                c(0, 0, 0),
##            Critical_Manufacturing_Functionality = 
##                c(0, 0, 0),
##            Water_Functionality =
##                c(0, 0, 0),
##            Healthcare_Function =
##                if(strength == 1){
##                    ttr <- 2
##                    pl <- 1.2
##                    dt <- .5
##                    c(ttr, pl, dt)
##                } else if (strength < 4) {
##                    ttr <- 8
##                    pl <- 1.4
##                    dt <- .5
##                    c(ttr, pl, dt)
##                } else {
##                    ttr <- 15
##                    pl <- 1.5
##                    dt <- .5
##                    c(ttr, pl, dt)
##                },
##            IT_Function =
##                c(0, 0, 0),
##            Communications_Function =
##                c(0, 0, 0)
##            )
## }
calc_strength_factors <- function(system, strength){
    if(system == "Transportation_Function"){
        if(strength == 1){
            ## sf[1] is the time to recover (days)
            ## sf[2] is the perturbation level
            ## sf[3] is the delay time
            ttr <- 4.5
            pl <- 1.2
            dt <- -2
            c(ttr, pl, dt)
        } else if (strength == 2){
            ttr <- 17
            pl <- 1.8
            dt <- -5
            c(ttr, pl, dt)
        } else if (strength == 3){
            ttr <- 28
            pl <- 1.8
            dt <- -5
            c(ttr, pl, dt)
        } else {
            ttr <- 75
            pl <- 2.0
            dt <- -5
            c(ttr, pl, dt)
        }
    } else if (system == "Emergency_Services_Functionality"){
        if(strength == 1){
            ttr <- 5
            pl <- 1.2
            dt <- 0
            c(ttr, pl, dt)
        } else if (strength == 2) {
            ttr <- 17
            pl <- 1.5
            dt <- 0
            c(ttr, pl, dt)
        } else if (strength == 3) {
            ttr <- 28
            pl <- 1.5
            dt <- 0
            c(ttr, pl, dt)
        } else {
            ttr <- 75
            pl <- 1.5
            dt <- 0
            c(ttr, pl, dt)
        }
    } else if (system == "Healthcare_Function"){
        if(strength == 1){
            ttr <- 5
            pl <- 1.2
            dt <- 0
            c(ttr, pl, dt)
        } else if (strength == 2) {
            ttr <- 17
            pl <- 1.5
            dt <- 0
            c(ttr, pl, dt)
        } else if (strength == 3) {
            ttr <- 28
            pl <- 1.5
            dt <- 0
            c(ttr, pl, dt)
        } else {
            ttr <- 75
            pl <- 1.5
            dt <- 0
            c(ttr, pl, dt)
        }
    } else {
        c(0, 0, 0)
    }
}

bld_need_all <- function(DF, stormlist){
    name_inf <- unique(DF$Infrastructure)
    num_inf <- length(name_inf)
    all_DF <- tibble()
    pb <- txtProgressBar(min = 0, max = num_inf, style = 3)
    for(i in 1:num_inf){
        ## Could call a case switch statement for each of the
        ## infrastructures here to calc baseline and year2val if desired
        bl <- 1
        yr2val <- 1
        system_inf<- name_inf[i]
        need_DF <- build_need(DF, stormlist, system_inf, bl, yr2val)
        all_DF <- bind_rows(all_DF, need_DF)
        setTxtProgressBar(pb, i)
    }
    all_DF
}

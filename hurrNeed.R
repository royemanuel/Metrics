## build the need profiles for the hurricane scenarios

## This will take in the tidy-fied data from the hurricange ingestion
## sequence. That way we can split by runs.

## This is a first order demonstration, so keep this simple and flexible

## Let's say that Emergency services see a rise in demand that decays
## as the storm goes on, and that it is dependent upon the hurricane strike

build_need <- function(DF,
                       stormlist = NA,
                       system,
                       # delay, # days
                       baseline,
                       year2val,
                       time_horizon
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
    ## pb2 <- txtProgressBar(min = 0, max = length(unique(DF$Run)), style = 3)
    for (run in 1:length(unique(DF$Run))){
        DF_run <-
            DF %>%
            filter(Run == unique(DF$Run)[run])
        ## print(paste("baseline is", baseline, "in run", run))
        need_vector <- seq(from = baseline,
                           to = year2val,
                           length.out = time_horizon)
        if(dim(stormlist)[1] != 0){
            storm_run <- filter(stormlist, Run == run)
            for (sr in 1:dim(storm_run)[1]){
                time <- storm_run$FailTime[sr]
                strength <- storm_run$HurricaneStrength[sr]
                ## Need to vary the recovery time and the perturbation level by
                ## strength of the storm
                p_vec <- calc_strength_factors(system, strength)
                ## p_vec[1] is the time to recover (days)
                ## p_vec[2] is the perturbation level
                ## p_vec[3] is the delay time
                recovertime <- p_vec[1] * 1440
                perturb_level <- p_vec[2] *
                    filter(DF_run,
                           Time == round(time / 240) * 240)$Performance
                ## cat(perturb_level)
                delay <- p_vec[3] * 1440
                                        # print(sr)
                abs_rec_time <- time + (delay + recovertime)
                start_replace <- time + delay
                ## cat("\r", system, run, sr, start_replace)
                ## Need to handle the case when demand does not recover
                ## before the two year point
                if (start_replace > time_horizon){
                    break
                } else if (abs_rec_time > time_horizon){
                    end_replace <- time_horizon
                    rec_level_future <- (year2val - baseline) / time_horizon *
                        (time + delay + recovertime)
                    demand_vec <- seq(from = perturb_level,
                                      to = rec_level_future,
                                      length.out = recovertime )
                    time_to_end <- time_horizon - (time + delay) + 1
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
                if(length(demand_vec)>0){
                    need_vector[start_replace:end_replace] <- demand_vec
                    }
            }
            need_vector
        }
        t_vec <- seq(from = 1, to = length(need_vector), by = 1)
        need_tbl <- as.tibble(bind_cols(Need = need_vector,
                                        Time = t_vec))
        need_tbl <- filter(need_tbl, Time %% 240 == 0)
        DF_run <- inner_join(DF_run, need_tbl, by = "Time")
        DF_holder <- bind_rows(DF_holder, DF_run)
    }
    DF_holder
}

build_need_q<- function(DF,
                       stormlist,
                       system,
                       # delay, # days
                       baseline,
                       year2val,
                       time_horizon,
                       SQ = NA
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
    DF_run <-
        DF
    need_vector <- seq(from = baseline,
                       to = year2val,
                       length.out = time_horizon)
    if(dim(stormlist)[1] != 0){
        storm_run <- stormlist ##filter(stormlist, Run == run)
        for (sr in 1:dim(storm_run)[1]){
            time <- storm_run$FailTime[sr]
            if(time > time_horizon){
                break
            }
            strength <- storm_run$HurricaneStrength[sr]
            ## Need to vary the recovery time and the perturbation level by
            ## strength of the storm
            if(is.na(SQ)){
                p_vec <- calc_strength_factors(system, strength)
            } else {
                p_vec <- c(0,0,0)
            }
            ## p_vec[1] is the time to recover (days)
            ## p_vec[2] is the perturbation level
            ## p_vec[3] is the delay time
            recovertime <- p_vec[1] * 1440
            ## Old perturb level. Stored for reversion uses performance,
            ## but it should use demand
            ## perturb_level <- p_vec[2] *
            ##     filter(DF_run,
            ##            Time == round(time / 240) * 240)$Performance
            ## Switching in the need_vector value instead of performance
            perturb_level <- p_vec[2] * need_vector[time]
            delay <- p_vec[3] * 1440
                                    # print(sr)
            abs_rec_time <- time + (delay + recovertime)
            print(abs_rec_time)
            start_replace <- time + delay
            ## cat("\r", system, sr, start_replace)
            ## Need to handle the case when demand does not recover
            ## before the two year point
            if (start_replace > time_horizon){
                break
            } else if (abs_rec_time > time_horizon){
                end_replace <- time_horizon
                rec_level_future <- (year2val - baseline) / time_horizon *
                    (time + delay + recovertime) + baseline
                print(rec_level_future)
                demand_vec <- seq(from = perturb_level,
                                  to = rec_level_future,
                                  length.out = recovertime )
                time_to_end <- time_horizon - (time + delay) + 1
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
            if(length(demand_vec)>0){
                need_vector[start_replace:end_replace] <- demand_vec
                }
        }
        need_vector
    }
    t_vec <- seq(from = 1, to = length(need_vector), by = 1)
    need_tbl <- as.tibble(bind_cols(Need = need_vector,
                                    Time = t_vec))
    need_tbl <- filter(need_tbl, Time %% 240 == 0)
    ## print(need_tbl)
    DF_run <- inner_join(DF_run, need_tbl, by = "Time")
    DF_holder <- bind_rows(DF_holder, DF_run)
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

bld_need_all <- function(DF, time_h, stormlist, need_inf){
    if(missing(need_inf)){
        need_inf <- tibble(Infrastructure = unique(DF$Infrastructure)) %>%
            mutate(BL = 1, Y2 = 1)
    }
    ## print(need_inf)
    name_inf <- unique(DF$Infrastructure)
    print(name_inf)
    num_inf <- length(name_inf)
    print(num_inf)
    all_DF <- tibble()
    ## pb <- txtProgressBar(min = 0, max = num_inf, style = 3)
    for(i in 1:num_inf){
        ## Could call a case switch statement for each of the
        ## infrastructures here to calc baseline and year2val if desired
        system_inf <- name_inf[i]
        bl <-  filter(need_inf, Infrastructure == name_inf[i])$BL
        yr2val <- filter(need_inf, Infrastructure == name_inf[i])$Y2
        need_DF <- build_need(DF = DF,
                              stormlist = stormlist,
                              system = system_inf,
                              baseline = bl,
                              year2val = yr2val,
                              time_horizon = time_h)
        all_DF <- bind_rows(all_DF, need_DF)
        ## setTxtProgressBar(pb, i)
    }
    all_DF
}

bld_need_all_q<- function(DF, time_h, stormlist, need_inf, SQ = NA){
    if(missing(need_inf)){
        need_inf <- tibble(Infrastructure = unique(DF$Infrastructure)) %>%
            mutate(BL = 1, Y2 = 1)
    }
    ## print(need_inf)
    name_inf <- unique(DF$Infrastructure)
    num_inf <- length(name_inf)
    all_DF <- tibble()
    ## pb <- txtProgressBar(min = 0, max = num_inf, style = 3)
    for(i in 1:num_inf){
        ## Could call a case switch statement for each of the
        ## infrastructures here to calc baseline and year2val if desired
        system_inf <- name_inf[i]
        bl <-  filter(need_inf, Infrastructure == name_inf[i])$BL
        yr2val <- filter(need_inf, Infrastructure == name_inf[i])$Y2
        need_DF <- build_need_q(DF = DF,
                              stormlist = stormlist,
                              system = system_inf,
                              baseline = bl,
                              year2val = yr2val,
                              time_horizon = time_h,
                              SQ = SQ)
        all_DF <- bind_rows(all_DF, need_DF)
        ## setTxtProgressBar(pb, i)
    }
    all_DF
}

test_need <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.2, .95, 1.2, 1.2, 1.1, 1.3, 1.1))

zero_storm_profile <- function(DF, time_hor, emptystormlist, need_profile){
    usedRun <- min(DF$Run)
    working_DF <-
        DF %>%
        filter(Run == usedRun) %>%
        mutate(Performance = 1)
    if (missing(need_profile)){
        wdf <- bld_need_all(working_DF, time_hor, emptystormlist)
    } else {
        wdf <- bld_need_all(DF = working_DF,
                            time_h = time_hor,
                            stormlist = emptystormlist,
                            need_inf = need_profile)
    }
}









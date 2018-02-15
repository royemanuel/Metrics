## build the need profiles for the hurricane scenarios

## This will take in the tidy-fied data from the hurricange ingestion
## sequence. That way we can split by runs.

## This is a first order demonstration, so keep this simple and flexible

## Let's say that Emergency services see a rise in demand that decays
## as the storm goes on, and that it is dependent upon the hurricane strike

build_need <- function(DF,
                       stormlist,
                       system,
                       delay, # days
                       baseline,
                       year2val,
                       perturb_level,
                       recovertime){ # days
    DF <-
        DF %>%
        filter(Infrastructure == system)
    DF_holder <- tibble()
    recovertime <- recovertime * 1440
    delay <- delay * 1440
    for (run in 1:length(unique(DF$Run))){
        DF_run<-
            DF %>%
            filter(Run == run)
        need_vector <- seq(from = baseline,
                           to = year2val,
                           length.out = 1051200)
        print(length(need_vector))
        storms_run <-
            stormlist[run, 1:10] %>%
            gather() %>%
            filter(value < 1051200)
        print(storms_run)
        ## Hardcoding this for simplicity
        num_storm_run <- dim(storms_run)[1] - 5
        print(paste("storms in this row", num_storm_run))
        for (sr in 1:num_storm_run){
            time <- storms_run$value[sr]
            strength <- storms_run$value[sr+5]
            print(sr)
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
                time_to_end <- 1051200 - (time + delay) 
                demand_vec <- demand_vec[1:time_to_end]
             } else {
                end_replace <- start_replace + recovertime  - 1
                rec_level <- need_vector[time + (delay + recovertime) ]
                print(rec_level)
                demand_vec <- seq(from = perturb_level,
                                  to = rec_level,
                                  length.out = recovertime )
                print("yup")
            }
            need_vector[start_replace:end_replace] <- demand_vec
            t_vec <- seq(from = 1, to = length(need_vector), by = 1)
            need_tbl <- as.tibble(bind_cols(Need = need_vector,
                                            Time = t_vec))
            need_tbl <- filter(need_tbl, Time %% 240 == 0)
            print(need_tbl)
        }
        DF_run <- inner_join(DF_run, need_tbl, by = "Time")
        DF_holder <- bind_rows(DF_holder, DF_run)
    }
    DF_holder
}




















library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
source("stormDef.R")
## Commented out because build it once and be done.
##source("stormDef.R")
## Set the data directory, the results directory, and the appendage
## to differentiate the files so you can open them in Excel if necessary

data_directory <- "singlestorm/"
results_directory <-  "studyData/singlestormResults/"
file_name <- "10yrPR"

    
study_files <- c(paste0(data_directory,"1sMCoutput.xlsx"))#,
                 ## paste0(data_directory,"2sMCoutput.xlsx"),
                 ## paste0(data_directory,"3sMCoutput.xlsx"),
                 ## paste0(data_directory,"4sMCoutput.xlsx"),
                 ## paste0(data_directory,"5sMCoutput.xlsx"))
run_profiles <- c(paste0(data_directory,"1srunProfile.xlsx"))#,
                  ## paste0(data_directory,"2srunProfile.xlsx"),
                  ## paste0(data_directory,"3srunProfile.xlsx"),
                  ## paste0(data_directory,"4srunProfile.xlsx"),
                  ## paste0(data_directory,"5srunProfile.xlsx"))
 



## mystorms <- as.tibble(read.csv("runyrFR_profiles_seed.csv"))

rising_need2yr <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.04, .95, 0.96, 1.09, .94, 1.06, 1.2))

DF_EIR_SS <- tibble()
DF_EIR_SS_SQ <- tibble()
for(d in 1:length(study_files)){
    sf_data <- ingestHurrDataSS(study_files[d])
    mystorms <- ingest_run_profiles(run_profiles[d])
    cat("\r", "Data file ", d, " ingested from Excel")
    R_EIR <- tibble()
    R_EIR_SQ <- tibble()
    storm_summary <-
        mystorms %>%
        group_by(Run) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  RecoveryTime = RecoveryTime)
    sf_data_clean <-
        sf_data %>%
        filter(Time > 0) %>%
        gather(Infrastructure, Performance, -Run, -Time) %>%
        mutate(Performance = round(Performance / 100, 2))  %>%
        group_by(Run, Infrastructure)
    runs <- unique(sf_data_clean$Run)
    number_runs <- 1#length(runs)
    for(r in 1:number_runs){
        sf_data_run <-
            sf_data_clean %>%
            filter(Run == 20)# runs[r])
        storm <-
            mystorms %>%
            filter(Run == 20)#runs[r])
        if(storm$HurricaneStrength == 1){
            TH <- storm$RecoveryTime + 6 * 1440 + 8 * 1440
        } else if (storm$HurricaneStrength == 2){
            TH <- storm$RecoveryTime + 6 * 1440 + 8 * 1440
        } else if (storm$HurricaneStrength == 2){
            TH <- storm$RecoveryTime + 18 * 1440 + 8 * 1440
        } else {
            TH <- storm$RecoveryTime + 75 * 1440 + 8 * 1440
        }
        rising_need2yr <-
            rising_need2yr %>%
            mutate(Y2 = BL + (Y2 - BL) * (TH /5256000))
        sf_data_testneed2yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = TH,
                                           stormlist = storm,
                                           need_inf = rising_need2yr)
        storm_run_TN2yr <-
            sf_data_testneed2yr %>%
            group_by(Run)
        sf_data_groups <- assignGroup_q(sf_data_testneed2yr)
        sf_EIR <- calc_EIR(sf_data_groups, 0)
        sf_EIR <- left_join(sf_EIR, storm_summary, by = "Run")
        R_EIR <- bind_rows(R_EIR, sf_EIR)
        ## Status quo need portion
        sf_data_SQ <- sf_data_run %>%
            mutate(Need = 1) %>%
            filter(Time < storm$RecoveryTime + 8*1440)
        storm_run_SQ <-
            sf_data_SQ %>%
            group_by(Run)
        sf_data_groups_SQ <- assignGroup_q(sf_data_SQ)
        sf_EIR_SQ <- calc_EIR(sf_data_groups_SQ, 0)
        sf_EIR_SQ<- left_join(sf_EIR_SQ, storm_summary, by = "Run")
        cat("\r", "Run ", runs[r], " complete")
        R_EIR_SQ <- bind_rows(R_EIR_SQ, sf_EIR_SQ)
        both_EIR <- list(R_EIR, R_EIR_SQ)
    }
    DF_EIR_SS <- bind_rows(DF_EIR_SS, both_EIR[[1]])
    DF_EIR_SS_SQ <- bind_rows(DF_EIR_SS_SQ, both_EIR[[2]])
    all_results <- list(rising_need = DF_EIR_SS, status_quo = DF_EIR_SS_SQ)
}

write.csv(all_results[[1]], "studyData/singlestormResults/risingNeed.csv")
write.csv(all_results[[2]], "studyData/singlestormResults/statusQuo.csv")

sq_data <- all_results[[2]] %>%
    mutate(Need_Profile = "Status Quo")
rn_data <- all_results[[1]] %>%
    mutate(Need_Profile = "Stakeholder Input")
all_results_tbl <- bind_rows(sq_data, rn_data)

write.csv(all_results_tbl, "studyData/singlestormResults/allData.csv")

######################################################################
## build the extra runs for each time horizon with the rising need
######################################################################

## This portion is unnecessary for the single storm case.


## no_fail_runs <- zero_storm_profile(DF = sf_data_clean,
##                                time_hor = max(sf_data_clean$Time),
##                                emptystormlist = noStorms,
##                                need_profile = rising_need2yr)
## 
## no_fail_runs_groups <- assignGroup(no_fail_runs)
## 
## no_fail_runs_EIR_24mo <- calc_EIR(no_fail_runs_groups, 0) %>%
##     mutate(TimeHorizon = 24)
## no_fail_runs_EIR_18mo <- calc_EIR(filter(no_fail_runs_groups,
##                                          Time < 525600 * 1.5), 0) %>%
##     mutate(TimeHorizon = 18)
## no_fail_runs_EIR_12mo <- calc_EIR(filter(no_fail_runs_groups,
##                                          Time < 525600), 0) %>%
##     mutate(TimeHorizon = 12)
## no_fail_runs_EIR_6mo <- calc_EIR(filter(no_fail_runs_groups,
##                                         Time < 525600 * .5), 0) %>%
##     mutate(TimeHorizon = 6)
## no_fail_runs_EIR <- bind_rows(no_fail_runs_EIR_24mo,
##                               no_fail_runs_EIR_18mo,
##                               no_fail_runs_EIR_12mo,
##                               no_fail_runs_EIR_6mo)
##     
## no_fail_runs_EIR <-
##     no_fail_runs_EIR%>%
##     mutate(Strongest_Storm = 0,
##            Worst_Failure = NA,
##            End_Rec_Level = NA,
##            Number_Storms = 0)
## 
## Qall_sf_runs <- add_nostorm_runs(DF_EIR_SS, no_fail_runs_EIR, 541)
## Qall_sf_runs$TimeHorizon <- as.factor(Qall_sf_runs$TimeHorizon)
## write.csv(Qall_sf_runs, "studyData/2yrFullRec/multTHwithNoStormsQ.csv")
######################################################################
## Build the status quo storm runs (everything equals 1)
## ######################################################################
## noStorms <- filter(mystorms, HurricaneStrength > 24)
##

## DF_EIR_SS_SQ <- tibble()
## for(d in 1:length(study_files)){
##     sf_data <- ingestHurrData(study_files[d])
##     mystorms <- ingest_run_profiles(run_profiles[d])
##     cat("\r", "Data file ", d, " ingested from Excel")
##     R_EIR <- tibble()
##     storm_summary24mo <-
##         mystorms %>%
##         group_by(Run) %>%
##         filter(FailTime < 525600 * 2) %>%
##         summarise(Strongest_Storm = max(HurricaneStrength),
##                   Worst_Failure = min(FailLevel),
##                   End_Rec_Level = min(RecoveryLevel),
##                   Number_Storms = n())
##     sf_data_clean <-
##         sf_data %>%
##         filter(Time > 0) %>%
##         gather(Infrastructure, Performance, -Run, -Time) %>%
##         mutate(Performance = round(Performance / 100, 2))  %>%
##         group_by(Run, Infrastructure)
##     runs <- unique(sf_data_clean$Run)
##     number_runs <- length(runs)
##     for(r in 1:number_runs){
##         sf_data_run <-
##             sf_data_clean %>%
##             filter(Run == runs[r])
##         storm <-
##             mystorms %>%
##             filter(Run == runs[r])
##         sf_data_testneed2yr <- bld_need_all_q(DF = sf_data_run,
##                                            time_h = 525600 * 2,
##                                            stormlist = storm)
##         storm_run_TN2yr <-
##             sf_data_testneed2yr%>%
##             group_by(Run)
##         sf_data_groups2yr <- assignGroup_q(sf_data_testneed2yr)
##         sf_EIR6mo <- calc_EIR(filter(sf_data_groups2yr, Time < 525600 * .5), 0)
##         sf_EIR6mo <- left_join(sf_EIR6mo, storm_summary6mo, by = "Run")
##         sf_EIR6mo <- mutate(sf_EIR6mo, TimeHorizon = 6)
##         sf_EIR1yr <- calc_EIR(filter(sf_data_groups2yr, Time < 525600), 0)
##         sf_EIR1yr <- left_join(sf_EIR1yr, storm_summary12mo, by = "Run")
##         sf_EIR1yr <- mutate(sf_EIR1yr, TimeHorizon = 12)
##         sf_EIR18mo <- calc_EIR(filter(sf_data_groups2yr, Time < 525600 * 1.5), 0)
##         sf_EIR18mo <- left_join(sf_EIR18mo, storm_summary18mo, by = "Run")
##         sf_EIR18mo <- mutate(sf_EIR18mo, TimeHorizon = 18)
##         sf_EIR2yr <- calc_EIR(sf_data_groups2yr, 0)
##         sf_EIR2yr <- left_join(sf_EIR2yr, storm_summary24mo, by = "Run")
##         sf_EIR2yr <- mutate(sf_EIR2yr, TimeHorizon = 24)
##         cat("\r", "Run ", runs[r], " complete")
##         R_EIR <- bind_rows(R_EIR, sf_EIR6mo, sf_EIR1yr, sf_EIR18mo, sf_EIR2yr)
##     }
##     DF_EIR_SS_SQ <- bind_rows(DF_EIR_SS_SQ, R_EIR)
## }
## 
## write.csv(DF_EIR_SS_SQ, "studyData/singlestormResults/statusQuo.csv")
## 
## noStorms <- filter(mystorms, HurricaneStrength > 24)
## 
## no_fail_runsSQ <- zero_storm_profile(DF = sf_data_clean,
##                                time_hor = max(sf_data_clean$Time),
##                                emptystormlist = noStorms)
## 
## no_fail_runsSQ_groups <- assignGroup(no_fail_runsSQ)
## 
## no_fail_runsSQ_EIR_24mo <- calc_EIR(no_fail_runsSQ_groups, 0) %>%
##     mutate(TimeHorizon = 24)
## no_fail_runsSQ_EIR_18mo <- calc_EIR(filter(no_fail_runsSQ_groups,
##                                          Time < 525600 * 1.5), 0) %>%
##     mutate(TimeHorizon = 18)
## no_fail_runsSQ_EIR_12mo <- calc_EIR(filter(no_fail_runsSQ_groups,
##                                          Time < 525600), 0) %>%
##     mutate(TimeHorizon = 12)
## no_fail_runsSQ_EIR_6mo <- calc_EIR(filter(no_fail_runsSQ_groups,
##                                         Time < 525600 * .5), 0) %>%
##     mutate(TimeHorizon = 6)
## no_fail_runsSQ_EIR <- bind_rows(no_fail_runsSQ_EIR_24mo,
##                               no_fail_runsSQ_EIR_18mo,
##                               no_fail_runsSQ_EIR_12mo,
##                               no_fail_runsSQ_EIR_6mo)
##     
## no_fail_runsSQ_EIR <-
##     no_fail_runsSQ_EIR%>%
##     mutate(Strongest_Storm = 0,
##            Worst_Failure = NA,
##            End_Rec_Level = NA,
##            Number_Storms = 0)
## 
## Qall_sf_runsSQ <- add_nostorm_runs(DF_EIR_SS_SQ, no_fail_runsSQ_EIR, 541)
## 
## all_2yr_runsSQ <- add_nostorm_runs(DF_EIR_SS_SQ, no_fail_runsSQ_EIR, 541)
## all_2yr_runsSQ$TimeHorizon <- as.factor(all_2yr_runsSQ$TimeHorizon)
## write.csv(all_2yr_runsSQ, "studyData/2yrFullRec/multTHwithNoStormsQ_SQ.csv")
## 

## write.csv(sf_data_groups, "studyData/2yrFullRec/2yr_levelNeedproportionaltotime_groupsFR.csv")
## write.csv(all_2yr_runs, "studyData/2yrFullRec/2yr_levelNeedproportionaltotime_resilienceFR.csv")

## Now do the same thing for Need is status quo
## sf_SQ__data_need <- mutate(sf_data_clean, Need = 1)
## 
## sf_SQ__data_groups <- assignGroup(sf_SQ__data_need)
## 
## sf_SQ__EIR <- calc_EIR(sf_SQ__data_groups, 0)
## 
## sf_SQ__EIR <- inner_join(sf_SQ__EIR, storm_summary, by = "Run")
## 
## ## Now build the no storm runs with a status quo need
## no_fail_runs_SQ <-
##     sf_data_clean %>%
##     filter(Run == 1) %>%
##     mutate(Performance = 1, Need = 1)
## 
## no_fail_runs_groups_SQ <- assignGroup(no_fail_runs_SQ)
## 
## no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups_SQ, 0)
## 
## no_fail_runs_EIR <-
##     no_fail_runs_EIR %>%
##     mutate(Strongest_Storm = 0,
##            Worst_Failure = NA,
##            End_Rec_Level = NA,
##            Number_Storms = 0)
## 
## all_2yr_runs_SQ <- add_nostorm_runs(sf_SQ__EIR, no_fail_runs_EIR, 541)
## 
## write.csv(sf_SQ__data_groups, "studyData/2yrFullRec/2yr_statusquoNeed_groupsFR.csv")
## write.csv(all_2yr_runs_SQ, "studyData/2yrFullRec/2yr_statusquoNeed_resilienceFR.csv")
##  
## ## Build plots that are useful and summary statistics
## 
## plot_EIR <- ggplot(sf_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
##     geom_boxplot() +
##     theme_bw(base_size = 12, base_family = "serif") +
##     theme(legend.margin=margin(t = 0, unit = 'cm'),
##           legend.position = "top",
##           legend.title = element_blank()) +
##     ylim(0, 1.2)
## 
## plot_storm_strength <- ggplot(sf_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
##     geom_boxplot() +
##     facet_grid(Strongest_Storm ~ Number_Storms) +
##     theme_bw(base_size = 12, base_family = "serif") +
##     theme(legend.margin=margin(t = 0, unit = 'cm'),
##           legend.position = "top",
##           legend.title = element_blank()) +
##     ylim(0, 1.2)
## 















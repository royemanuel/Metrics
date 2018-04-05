library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
source("stormDef.R")
## Commented out because build it once and be done.
##source("stormDef.R")
qt <- proc.time()


study_files <- c("HurrData547/01MCoutput.xlsx",
                 "HurrData547/02MCoutput.xlsx",
                 "HurrData547/03MCoutput.xlsx",
                 "HurrData547/04MCoutput.xlsx",
                 "HurrData547/05MCoutput.xlsx",
                 "HurrData547/06MCoutput.xlsx",
                 "HurrData547/07MCoutput.xlsx",
                 "HurrData547/08MCoutput.xlsx",
                 "HurrData547/09MCoutput.xlsx",
                 "HurrData547/10MCoutput.xlsx",
                 "HurrData547/11MCoutput.xlsx",
                 "HurrData547/12MCoutput.xlsx")
run_profiles <- c("HurrData547/01runProfile.xlsx",
                  "HurrData547/02runProfile.xlsx",
                  "HurrData547/03runProfile.xlsx",
                  "HurrData547/04runProfile.xlsx",
                  "HurrData547/05runProfile.xlsx",
                  "HurrData547/06runProfile.xlsx",
                  "HurrData547/07runProfile.xlsx",
                  "HurrData547/08runProfile.xlsx",
                  "HurrData547/09runProfile.xlsx",
                  "HurrData547/10runProfile.xlsx",
                  "HurrData547/11runProfile.xlsx",
                  "HurrData547/12runProfile.xlsx")
                  
 



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
                    Y2 = c(1.0, 1.04, .95, 0.96, 1.08, .94, 1.06, 1.02))


DF_EIR_PR <- tibble()
for(d in 1:length(study_files)){
    sf_data <- ingestHurrData(study_files[d])
    mystorms <- ingest_run_profiles(run_profiles[d])
    cat("\r", "Data file ", d, " ingested from Excel")
    R_EIR <- tibble()
    storm_summary24mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 2) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    storm_summary18mo <-
        mystorms %>%
        filter(FailTime < 525600 * 1.5) %>%
        group_by(Run) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    storm_summary12mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 1) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n()) 
    storm_summary6mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * .5) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    sf_data_clean <-
        sf_data %>%
        filter(Time > 0) %>%
        gather(Infrastructure, Performance, -Run, -Time) %>%
        mutate(Performance = round(Performance / 100, 2))  %>%
        group_by(Run, Infrastructure)
    runs <- unique(sf_data_clean$Run)
    number_runs <- length(runs)
    for(r in 1:number_runs){
        sf_data_run <-
            sf_data_clean %>%
            filter(Run == runs[r])
        storm <-
            mystorms %>%
            filter(Run == runs[r])
        sf_data_testneed2yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 2,
                                           stormlist = storm,
                                           need_inf = rising_need2yr)
        storm_run_TN2yr <-
            sf_data_testneed2yr%>%
            group_by(Run)
        sf_data_groups2yr <- assignGroup_q(sf_data_testneed2yr)
        sf_EIR6mo <- calc_EIR(filter(sf_data_groups2yr, Time < 525600 * .5), 0)
        sf_EIR6mo <- left_join(sf_EIR6mo, storm_summary6mo, by = "Run")
        sf_EIR6mo <- mutate(sf_EIR6mo, TimeHorizon = 6)
        sf_EIR1yr <- calc_EIR(filter(sf_data_groups2yr, Time < 525600), 0)
        sf_EIR1yr <- left_join(sf_EIR1yr, storm_summary12mo, by = "Run")
        sf_EIR1yr <- mutate(sf_EIR1yr, TimeHorizon = 12)
        sf_EIR18mo <- calc_EIR(filter(sf_data_groups2yr, Time < 525600 * 1.5), 0)
        sf_EIR18mo <- left_join(sf_EIR18mo, storm_summary18mo, by = "Run")
        sf_EIR18mo <- mutate(sf_EIR18mo, TimeHorizon = 18)
        sf_EIR2yr <- calc_EIR(sf_data_groups2yr, 0)
        sf_EIR2yr <- left_join(sf_EIR2yr, storm_summary24mo, by = "Run")
        sf_EIR2yr <- mutate(sf_EIR2yr, TimeHorizon = 24)
        cat("\r", "Run ", runs[r], " complete")
        R_EIR <- bind_rows(R_EIR, sf_EIR6mo, sf_EIR1yr, sf_EIR18mo, sf_EIR2yr)
    }
    DF_EIR_PR <- bind_rows(DF_EIR_PR, R_EIR)
}

write.csv(DF_EIR_PR, "studyData/2yrPRec/multTHonlyStormsQ.csv")

noStorms <- filter(mystorms, HurricaneStrength > 24)
perfect_performance <- tibble()
######################################################################
## build the extra runs for each time horizon with the rising need
######################################################################


no_fail_runs <- zero_storm_profile(DF = sf_data_clean,
                               time_hor = max(sf_data_clean$Time),
                               emptystormlist = noStorms,
                               need_profile = rising_need2yr)

no_fail_runs_groups <- assignGroup(no_fail_runs)

no_fail_runs_EIR_24mo <- calc_EIR(no_fail_runs_groups, 0) %>%
    mutate(TimeHorizon = 24)
no_fail_runs_EIR_18mo <- calc_EIR(filter(no_fail_runs_groups,
                                         Time < 525600 * 1.5), 0) %>%
    mutate(TimeHorizon = 18)
no_fail_runs_EIR_12mo <- calc_EIR(filter(no_fail_runs_groups,
                                         Time < 525600), 0) %>%
    mutate(TimeHorizon = 12)
no_fail_runs_EIR_6mo <- calc_EIR(filter(no_fail_runs_groups,
                                        Time < 525600 * .5), 0) %>%
    mutate(TimeHorizon = 6)
no_fail_runs_EIR <- bind_rows(no_fail_runs_EIR_24mo,
                              no_fail_runs_EIR_18mo,
                              no_fail_runs_EIR_12mo,
                              no_fail_runs_EIR_6mo)
    
no_fail_runs_EIR <-
    no_fail_runs_EIR%>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

Qall_sf_runs <- add_nostorm_runs(DF_EIR_PR, no_fail_runs_EIR, 541)
Qall_sf_runs$TimeHorizon <- as.factor(Qall_sf_runsSQ$TimeHorizon)
write.csv(Qall_sf_runs, "studyData/2yrPRec/multTHwithNoStormsQ.csv")
######################################################################
## Build the status quo storm runs (everything equals 1)
######################################################################

DF_EIR_PR_SQ <- tibble()
for(d in 1:length(study_files)){
    sf_data <- ingestHurrData(study_files[d])
    mystorms <- ingest_run_profiles(run_profiles[d])
    cat("\r", "Data file ", d, " ingested from Excel")
    R_EIR <- tibble()
    storm_summary24mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 2) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    storm_summary18mo <-
        mystorms %>%
        filter(FailTime < 525600 * 1.5) %>%
        group_by(Run) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    storm_summary12mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 1) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n()) 
    storm_summary6mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * .5) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    sf_data_clean <-
        sf_data %>%
        filter(Time > 0) %>%
        gather(Infrastructure, Performance, -Run, -Time) %>%
        mutate(Performance = round(Performance / 100, 2))  %>%
        group_by(Run, Infrastructure)
    runs <- unique(sf_data_clean$Run)
    number_runs <- length(runs)
    for(r in 1:number_runs){
        sf_data_run <-
            sf_data_clean %>%
            filter(Run == runs[r])
        storm <-
            mystorms %>%
            filter(Run == runs[r])
        sf_data_testneed2yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 2,
                                           stormlist = storm)
        storm_run_TN2yr <-
            sf_data_testneed2yr%>%
            group_by(Run)
        sf_data_groups2yr <- assignGroup_q(sf_data_testneed2yr)
        sf_EIR6mo <- calc_EIR(filter(sf_data_groups2yr, Time < 525600 * .5), 0)
        sf_EIR6mo <- left_join(sf_EIR6mo, storm_summary6mo, by = "Run")
        sf_EIR6mo <- mutate(sf_EIR6mo, TimeHorizon = 6)
        sf_EIR1yr <- calc_EIR(filter(sf_data_groups2yr, Time < 525600), 0)
        sf_EIR1yr <- left_join(sf_EIR1yr, storm_summary12mo, by = "Run")
        sf_EIR1yr <- mutate(sf_EIR1yr, TimeHorizon = 12)
        sf_EIR18mo <- calc_EIR(filter(sf_data_groups2yr, Time < 525600 * 1.5), 0)
        sf_EIR18mo <- left_join(sf_EIR18mo, storm_summary18mo, by = "Run")
        sf_EIR18mo <- mutate(sf_EIR18mo, TimeHorizon = 18)
        sf_EIR2yr <- calc_EIR(sf_data_groups2yr, 0)
        sf_EIR2yr <- left_join(sf_EIR2yr, storm_summary24mo, by = "Run")
        sf_EIR2yr <- mutate(sf_EIR2yr, TimeHorizon = 24)
        cat("\r", "Run ", runs[r], " complete")
        R_EIR <- bind_rows(R_EIR, sf_EIR6mo, sf_EIR1yr, sf_EIR18mo, sf_EIR2yr)
    }
    DF_EIR_PR_SQ <- bind_rows(DF_EIR_PR_SQ, R_EIR)
}

write.csv(DF_EIR_PR_SQ, "studyData/2yrPRec/multTHonlyStormsQ_SQ.csv")
no_fail_runsSQ <- zero_storm_profile(DF = sf_data_clean,
                               time_hor = max(sf_data_clean$Time),
                               emptystormlist = noStorms)

no_fail_runsSQ_groups <- assignGroup(no_fail_runsSQ)

no_fail_runsSQ_EIR_24mo <- calc_EIR(no_fail_runsSQ_groups, 0) %>%
    mutate(TimeHorizon = 24)
no_fail_runsSQ_EIR_18mo <- calc_EIR(filter(no_fail_runsSQ_groups,
                                         Time < 525600 * 1.5), 0) %>%
    mutate(TimeHorizon = 18)
no_fail_runsSQ_EIR_12mo <- calc_EIR(filter(no_fail_runsSQ_groups,
                                         Time < 525600), 0) %>%
    mutate(TimeHorizon = 12)
no_fail_runsSQ_EIR_6mo <- calc_EIR(filter(no_fail_runsSQ_groups,
                                        Time < 525600 * .5), 0) %>%
    mutate(TimeHorizon = 6)
no_fail_runsSQ_EIR <- bind_rows(no_fail_runsSQ_EIR_24mo,
                              no_fail_runsSQ_EIR_18mo,
                              no_fail_runsSQ_EIR_12mo,
                              no_fail_runsSQ_EIR_6mo)
    
no_fail_runsSQ_EIR <-
    no_fail_runsSQ_EIR%>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

Qall_sf_runsSQ <- add_nostorm_runs(DF_EIR_FR, no_fail_runsSQ_EIR, 541)

write.csv(Qall_sf_runsSQ, "studyData/2yrPRec/multTHwithNoStormsQ_SQ.csv")
## write.csv(all_2yr_runs_SQ, "2yrQfullrecrisingneed.csv")
## done_time <- proc.time() - qt
## print(done_time)


## write.csv(sf_data_groups, "studyData/2yrPRec/2yr_levelNeedproportionaltotime_groupsFR.csv")
## write.csv(all_2yr_runs, "studyData/2yrPRec/2yr_levelNeedproportionaltotime_resilienceFR.csv")

## Now do the same thing for Need is status quo
## sf_SQ__data_need <- mutate(sf_data_clean, Need = 1)
## 
## sf_SQ__data_groups <- assignGroup(sf_SQ__data_need)
## 
## sf_SQ__EIR <- calc_EIR(sf_SQ__data_groups, 0)
## 
## sf_SQ__EIR <- left_join(sf_SQ__EIR, storm_summary, by = "Run")
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
## write.csv(sf_SQ__data_groups, "studyData/2yrPRec/2yr_statusquoNeed_groupsFR.csv")
## write.csv(all_2yr_runs_SQ, "studyData/2yrPRec/2yr_statusquoNeed_resilienceFR.csv")
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















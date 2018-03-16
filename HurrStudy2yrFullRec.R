library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
## Commented out because build it once and be done.
##source("stormDef.R")


study_files <- c("hurrFullRec2yr/1MCoutput.xlsx",
                 "hurrFullRec2yr/2MCoutput.xlsx",
                 "hurrFullRec2yr/3MCoutput.xlsx",
                 "hurrFullRec2yr/4MCoutput.xlsx",
                 "hurrFullRec2yr/5MCoutput.xlsx",
                 "hurrFullRec2yr/6MCoutput.xlsx",
                 "hurrFullRec2yr/7MCoutput.xlsx",
                 "hurrFullRec2yr/8MCoutput.xlsx",
                 "hurrFullRec2yr/9MCoutput.xlsx",
                 "hurrFullRec2yr/10MCoutput.xlsx",
                 "hurrFullRec2yr/11MCoutput.xlsx",
                 "hurrFullRec2yr/12MCoutput.xlsx")
run_profiles <- c("hurrFullRec2yr/1runProfile.xlsx",
                  "hurrFullRec2yr/2runProfile.xlsx",
                  "hurrFullRec2yr/3runProfile.xlsx",
                  "hurrFullRec2yr/4runProfile.xlsx",
                  "hurrFullRec2yr/5runProfile.xlsx",
                  "hurrFullRec2yr/6runProfile.xlsx",
                  "hurrFullRec2yr/7runProfile.xlsx",
                  "hurrFullRec2yr/8runProfile.xlsx",
                  "hurrFullRec2yr/9runProfile.xlsx",
                  "hurrFullRec2yr/10runProfile.xlsx",
                  "hurrFullRec2yr/11runProfile.xlsx",
                  "hurrFullRec2yr/12runProfile.xlsx")
                  
 



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

rising_need6mo <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.01, .95, 0.915, 1.065, .91, 1.015, 1.05))

rising_need1yr <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.02, .95, 0.93, 1.07, .92, 1.03, 1.1))

rising_need18mo <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.03, .95, 0.945, 1.08, .93, 1.045, 1.15))


DF_EIR <- tibble()
for(d in 1:length(study_files)){
    sf_data <- ingestHurrData(study_files[d])
    mystorms <- ingest_run_profiles(run_profiles[d])
    cat("\r", "Data file ", d, " ingested from Excel")
    R_EIR <- tibble()
    storm_summary <-
        mystorms %>%
        group_by(Run) %>%
        summarize(Strongest_Storm = max(HurricaneStrength),
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
        sf_data_testneed6mo<- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 0.5,
                                           stormlist = storm,
                                           need_inf = rising_need6mo)
        sf_data_testneed1yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 1,
                                           stormlist = storm,
                                           need_inf = rising_need1yr)
        sf_data_testneed18mo <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 1.5,
                                           stormlist = storm,
                                           need_inf = rising_need18mo)
        sf_data_testneed2yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 2,
                                           stormlist = storm,
                                           need_inf = rising_need2yr)
        storm_run_TN6mo <-
            sf_data_testneed6mo %>%
            group_by(Run)
        storm_run_TN1yr <-
            sf_data_testneed1yr %>%
            group_by(Run)
        storm_run_TN18mo <-
            sf_data_testneed18mo %>%
            group_by(Run)
        storm_run_TN2yr <-
            sf_data_testneed2yr%>%
            group_by(Run)
        sf_data_groups6mo <- assignGroup_q(sf_data_testneed6mo)
        sf_EIR6mo <- calc_EIR(sf_data_groups6mo, 0)
        sf_EIR6mo <- inner_join(sf_EIR6mo, storm_summary, by = "Run")
        sf_EIR6mo <- mutate(sf_EIR6mo, TimeHorizon = 6)
        sf_data_groups1yr <- assignGroup_q(sf_data_testneed1yr)
        sf_EIR1yr <- calc_EIR(sf_data_groups1yr, 0)
        sf_EIR1yr <- inner_join(sf_EIR1yr, storm_summary, by = "Run")
        sf_EIR1yr <- mutate(sf_EIR1yr, TimeHorizon = 12)
        sf_data_groups18mo <- assignGroup_q(sf_data_testneed18mo)
        sf_EIR18mo <- calc_EIR(sf_data_groups18mo, 0)
        sf_EIR18mo <- inner_join(sf_EIR18mo, storm_summary, by = "Run")
        sf_EIR18mo <- mutate(sf_EIR18mo, TimeHorizon = 18)
        sf_data_groups2yr <- assignGroup_q(sf_data_testneed2yr)
        sf_EIR2yr <- calc_EIR(sf_data_groups2yr, 0)
        sf_EIR2yr <- inner_join(sf_EIR2yr, storm_summary, by = "Run")
        sf_EIR2yr <- mutate(sf_EIR2yr, TimeHorizon = 24)
        cat("\r", "Run ", runs[r], " complete")
        R_EIR <- bind_rows(R_EIR, sf_EIR6mo, sf_EIR1yr, sf_EIR18mo, sf_EIR2yr)
    }
    DF_EIR <- bind_rows(DF_EIR, R_EIR)
}
   
write.csv(DF_EIR, "studyData/2yrFullRec/multTHonlyStorms.csv")




noStorms <- filter(mystorms, HurricaneStrength > 24)
perfect_performance <- tibble()
no_fail_runs_testneed <- zero_storm_profile(DF = sf_data_clean,
                               time_hor = max(sf_data_clean$Time),
                               emptystormlist = noStorms,
                               need_profile = rising_need2yr)



no_fail_runs_groups_testneed <- assignGroup(no_fail_runs_testneed)

no_fail_runs_EIR_testneed <- calc_EIR(no_fail_runs_groups_testneed, 0)

no_fail_runs_EIR_testneed <-
    no_fail_runs_EIR_testneed%>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

Qall_sf_runs_testneed <- add_nostorm_runs(DF_EIR, no_fail_runs_EIR_testneed, 10)

noStorms <- filter(mystorms, HurricaneStrength > 24)

no_fail_runs <- zero_storm_profile(DF = sf_data_clean,
                                   time_hor = max(sf_data_clean$Time),
                                   emptystormlist = noStorms)#,
                                   # need_profile= rising_need2yr)

no_fail_runs_groups <- assignGroup(no_fail_runs)

no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups, 0)

no_fail_runs_EIR <-
    no_fail_runs_EIR %>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_2yr_runs <- add_nostorm_runs(DF_EIR, no_fail_runs_EIR, 541)
## write.csv(sf_data_groups, "studyData/2yrFullRec/2yr_levelNeedproportionaltotime_groupsFR.csv")
write.csv(all_2yr_runs, "studyData/2yrFullRec/2yr_levelNeedproportionaltotime_resilienceFR.csv")

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















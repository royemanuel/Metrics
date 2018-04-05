## The 10 year storm study
library("tidyverse")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")

study_files <- c("hurr10year/01MCoutput10yr.xlsx",
                      "hurr10year/02MCoutput10yr.xlsx",
                      "hurr10year/03MCoutput10yr.xlsx",
                      "hurr10year/04MCoutput10yr.xlsx",
                      "hurr10year/05MCoutput10yr.xlsx",
                      "hurr10year/06MCoutput10yr.xlsx",
                      "hurr10year/07MCoutput10yr.xlsx",
                      "hurr10year/08MCoutput10yr.xlsx",
                      "hurr10year/09MCoutput10yr.xlsx",
                      "hurr10year/10MCoutput10yr.xlsx",
                      "hurr10year/11MCoutput10yr.xlsx")
run_profiles <- c("hurr10year/01runProfile10yr.xlsx",
                  "hurr10year/02runProfile10yr.xlsx",
                  "hurr10year/03runProfile10yr.xlsx",
                  "hurr10year/04runProfile10yr.xlsx",
                  "hurr10year/05runProfile10yr.xlsx",
                  "hurr10year/06runProfile10yr.xlsx",
                  "hurr10year/07runProfile10yr.xlsx",
                  "hurr10year/08runProfile10yr.xlsx",
                  "hurr10year/09runProfile10yr.xlsx",
                  "hurr10year/10runProfile10yr.xlsx",
                  "hurr10year/11runProfile10yr.xlsx",
                  "hurr10year/12runProfile10yr.xlsx")

## sf_data10yr <- ingestHurrData(study_files_10yr)
## print("Data ingested from Excel")
## sf_data10yr_clean <-
##     sf_data10yr %>%
##     filter(Time > 0) %>%
##     gather(Infrastructure, Performance, -Run, -Time) %>%
##     mutate(Performance = round(Performance / 100, 2))  %>%
##     group_by(Run, Infrastructure)
## 
## my10yrstorms <- as.tibble(read.csv("run10yr_profiles_seed.csv"))

rising_need10yr <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.2, .95, 1.2, 1.2, 1.1, 1.3, 1.1))

DF_EIR_PR <- tibble()
for(d in 1:length(study_files)){
    sf_data <- ingestHurrData(study_files[d])
    mystorms <- ingest_run_profiles(run_profiles[d])
    cat("\r", "Data file ", d, " ingested from Excel")
    R_EIR <- tibble()
    storm_summary120mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 10) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    storm_summary60mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 5) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  Number_Storms = n())
    storm_summary24mo <-
        mystorms %>%
        group_by(Run) %>%
        filter(FailTime < 525600 * 2) %>%
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
        sf_data_need10yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 10,
                                           stormlist = storm,
                                           need_inf = rising_need10yr)
        storm_run_TN10yr <-
            sf_data_need10yr%>%
            group_by(Run)
        sf_data_groups10yr <- assignGroup_q(sf_data_need10yr)
        sf_EIR6mo <- calc_EIR(filter(sf_data_groups10yr, Time < 525600 * .5), 0)
        sf_EIR6mo <- left_join(sf_EIR6mo, storm_summary6mo, by = "Run")
        sf_EIR6mo <- mutate(sf_EIR6mo, TimeHorizon = 6)
        sf_EIR1yr <- calc_EIR(filter(sf_data_groups10yr, Time < 525600), 0)
        sf_EIR1yr <- left_join(sf_EIR1yr, storm_summary12mo, by = "Run")
        sf_EIR1yr <- mutate(sf_EIR1yr, TimeHorizon = 12)
        sf_EIR2yr <- calc_EIR(sf_data_groups10yr, 0)
        sf_EIR2yr <- left_join(sf_EIR2yr, storm_summary24mo, by = "Run")
        sf_EIR2yr <- mutate(sf_EIR2yr, TimeHorizon = 24)
        sf_EIR5yr <- calc_EIR(filter(sf_data_groups10yr, Time < 525600 * 5), 0)
        sf_EIR5yr <- left_join(sf_EIR5yr, storm_summary60mo, by = "Run")
        sf_EIR5yr <- mutate(sf_EIR5yr, TimeHorizon = 60)
        sf_EIR10yr <- calc_EIR(sf_data_groups10yr, 0)
        sf_EIR10yr <- left_join(sf_EIR10yr, storm_summary120mo, by = "Run")
        sf_EIR10yr <- mutate(sf_EIR10yr, TimeHorizon = 120)

        cat("\r", "Run ", runs[r], " complete")
        R_EIR <- bind_rows(R_EIR,
                           sf_EIR6mo,
                           sf_EIR1yr,
                           sf_EIR2yr,
                           sf_EIR5yr,
                           sf_EIR10yr)
    }
    DF_EIR_PR <- bind_rows(DF_EIR_PR, R_EIR)
}

write.csv(DF_EIR_PR, "studyData/10yrData/MultipleTimeHorizons10yrMax.csv")



######################################################################
## Old way of doing business
## sf_data10yr_need <- bld_need_all(DF = sf_data10yr_clean,
##                                  time_h = 525600 * 10,
##                                  stormlist = my10yrstorms,
##                                  need_inf = rising_need10yr)
## 
## storm_run_data10yr <-
##     sf_data10yr_need %>%
##     group_by(Run)
## 
## storm10yr_summary <-
##     my10yrstorms %>%
##     group_by(Run) %>%
##     summarize(Strongest_Storm = max(HurricaneStrength),
##               Worst_Failure = min(FailLevel),
##               End_Rec_Level = min(RecoveryLevel),
##               Number_Storms = n())
## 
## sf_data10yr_groups <- assignGroup(sf_data10yr_need)
## 
## sf_EIR10yr <- calc_EIR(sf_data10yr_groups, 0)
## 
## sf_EIR10yr <- inner_join(sf_EIR10yr, storm10yr_summary, by = "Run")
## 
## noStorms <- filter(my10yrstorms, HurricaneStrength > 24)
## 
## no_fail_runs <- zero_storm_profile(DF = sf_data10yr_clean,
##                                    time_hor = max(sf_data10yr_clean$Time),
##                                    emptystormlist = noStorms,
##                                    need_profile = rising_need10yr)
## 
## no_fail_runs_groups <- assignGroup(no_fail_runs)
## 
## no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups, 0)
## 
## no_fail_runs_EIR <-
##     no_fail_runs_EIR %>%
##     mutate(Strongest_Storm = 0,
##            Worst_Failure = NA,
##            End_Rec_Level = NA,
##            Number_Storms = 0)
## 
## all_10yr_runs <- add_nostorm_runs(sf_EIR10yr, no_fail_runs_EIR, 18)
## write.csv(sf_data10yr_groups, "studyData/10yrData/10yr_risingNeed_groups.csv")
## write.csv(all_10yr_runs, "studyData/10yrData/10yr_risingNeed_resilience.csv")
## ## Now do the same thing for Need is status quo
## sf_SQ__data10yr_need <- mutate(sf_data10yr_clean, Need = 1)
## 
## sf_SQ__data10yr_groups <- assignGroup(sf_SQ__data10yr_need)
## 
## sf_SQ__EIR <- calc_EIR(sf_SQ__data10yr_groups, 0)
## 
## sf_SQ__EIR <- inner_join(sf_SQ__EIR, storm10yr_summary, by = "Run")
## 
## ## Now build the no storm runs with a status quo need
## no_fail_runs_SQ <-
##     sf_data10yr_groups %>%
##     filter(Run == 1) %>%
##     select(-diff, -Grp) %>%
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
## all_10yr_runs_SQ <- add_nostorm_runs(sf_SQ__EIR, no_fail_runs_EIR, 18)
## 
## write.csv(sf_data10yr_groups, "studyData/10yrData/10yr_statusquoNeed_groups.csv")
## write.csv(all_10yr_runs_SQ, "studyData/10yrData/10yr_statusquoNeed_resilience.csv")
##  




## Build plots that are useful and summary statistics



plot_EIR10yr <- ggplot(sf_EIR10yr, aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() +
    theme_bw(base_size = 12, base_family = "serif") +
    theme(legend.margin=margin(t = 0, unit = 'cm'),
          legend.position = "top",
          legend.title = element_blank()) +
    ylim(0, 1.2)

plot_storm_strength <- ggplot(sf_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() +
    facet_grid(Strongest_Storm ~ Number_Storms) +
    theme_bw(base_size = 12, base_family = "serif") +
    theme(legend.margin=margin(t = 0, unit = 'cm'),
          legend.position = "top",
          legend.title = element_blank()) +
    ylim(0, 1.2)




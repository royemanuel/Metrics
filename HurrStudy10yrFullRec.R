library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
source("stormDef.R")
## Commented out because build it once and be done.
##source("stormDef.R")


study_files <- c( "hurrFullRec10yr/1MCoutput.xlsx",
                  "hurrFullRec10yr/2MCoutput.xlsx",
                  "hurrFullRec10yr/3MCoutput.xlsx",
                  "hurrFullRec10yr/4MCoutput.xlsx",
                  "hurrFullRec10yr/5MCoutput.xlsx",
                  "hurrFullRec10yr/6MCoutput.xlsx",
                  "hurrFullRec10yr/7MCoutput.xlsx",
                  "hurrFullRec10yr/8MCoutput.xlsx",
                  "hurrFullRec10yr/9MCoutput.xlsx",
                  "hurrFullRec10yr/10MCoutput.xlsx",
                 "hurrFullRec10yr/11MCoutput.xlsx")
run_profiles <- c("hurrFullRec10yr/1runProfile.xlsx",
                  "hurrFullRec10yr/2runProfile.xlsx",
                  "hurrFullRec10yr/3runProfile.xlsx",
                  "hurrFullRec10yr/4runProfile.xlsx",
                  "hurrFullRec10yr/5runProfile.xlsx",
                  "hurrFullRec10yr/6runProfile.xlsx",
                  "hurrFullRec10yr/7runProfile.xlsx",
                  "hurrFullRec10yr/8runProfile.xlsx",
                  "hurrFullRec10yr/9runProfile.xlsx",
                  "hurrFullRec10yr/10runProfile.xlsx",
                  "hurrFullRec10yr/11runProfile.xlsx")              
 



## mystorms <- as.tibble(read.csv("runyrFR_profiles_seed.csv"))

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
        sf_data_testneed <- bld_need_all_q(DF = sf_data_run,
                                           time_h = 525600 * 2,
                                           stormlist = storm,
                                           need_inf = rising_need10yr)
        storm_run_TN <-
            sf_data_testneed %>%
            group_by(Run)
        sf_data_groups <- assignGroup_q(sf_data_testneed)
        sf_EIR <- calc_EIR(sf_data_groups, 0)
        sf_EIR <- inner_join(sf_EIR, storm_summary, by = "Run")
        cat("\r", "Run ", runs[r], " complete")
        R_EIR <- bind_rows(R_EIR, sf_EIR)
    }
    DF_EIR <- bind_rows(DF_EIR, R_EIR)
}
   





noStorms <- filter(mystorms, HurricaneStrength > 24)
perfect_performance <- tibble()
no_fail_runs_testneed <- zero_storm_profile(DF = sf_data_clean,
                               time_hor = max(sf_data_clean$Time),
                               emptystormlist = noStorms,
                               need_profile = rising_need10yr)



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
                                   # need_profile= rising_need10yr)

no_fail_runs_groups <- assignGroup(no_fail_runs)

no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups, 0)

no_fail_runs_EIR <-
    no_fail_runs_EIR %>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_10yr_runs <- add_nostorm_runs(DF_EIR, no_fail_runs_EIR, 541)
## write.csv(sf_data_groups, "studyData/10yrFullRec/10yr_levelNeedproportionaltotime_groupsFR.csv")
write.csv(all_10yr_runs, "studyData/10yrFullRec/10yr_levelNeedproportionaltotime_resilienceFR.csv")

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
## all_10yr_runs_SQ <- add_nostorm_runs(sf_SQ__EIR, no_fail_runs_EIR, 541)
## 
## write.csv(sf_SQ__data_groups, "studyData/10yrFullRec/10yr_statusquoNeed_groupsFR.csv")
## write.csv(all_10yr_runs_SQ, "studyData/10yrFullRec/10yr_statusquoNeed_resilienceFR.csv")
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















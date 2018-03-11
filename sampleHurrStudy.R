ptm <- proc.time()
library("tidyverse")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
######################################################################
## 
## Commented out because build it once and be done.
##source("stormDef.R")


study_files <- c("HurrData547/01MCoutput.xlsx",
                 "HurrData547/02MCoutput.xlsx")
 
sample_data <- ingestHurrData(study_files)
print("Data ingested from Excel")
sample_data_clean <-
    sample_data %>%
    filter(Time > 0) %>%
    gather(Infrastructure, Performance, -Run, -Time) %>%
    mutate(Performance = round(Performance / 100, 2))  %>%
    group_by(Run, Infrastructure)

mystorms <- as.tibble(read.csv("run_profiles_seed.csv"))

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

sample_data_testneed <- bld_need_all(DF = sample_data_clean,
                                 time_h = 525600 * 2,
                                 stormlist = mystorms,
                                 need_inf = test_need)

storm_run_TN <-
    sample_data_testneed %>%
    group_by(Run)

storm_summary <-
    mystorms %>%
    group_by(Run) %>%
    summarize(Strongest_Storm = max(HurricaneStrength),
              Worst_Failure = min(FailLevel),
              End_Rec_Level = min(RecoveryLevel),
              Number_Storms = n())

sample_data_groups <- assignGroup(sample_data_testneed)

sample_EIR <- calc_EIR(sample_data_groups, 0)

sample_EIR <- inner_join(sample_EIR, storm_summary, by = "Run")

noStorms <- filter(mystorms, HurricaneStrength > 24)

perfect_performance <- tibble()

no_fail_runs_testneed <- zero_storm_profile(DF = sample_data_clean,
                                   time_hor = max(sample_data_clean$Time),
                                   emptystormlist = noStorms,
                                   need_profile = test_need)



no_fail_runs_groups_testneed <- assignGroup(no_fail_runs_testneed)

no_fail_runs_EIR_testneed <- calc_EIR(no_fail_runs_groups_testneed, 0)

no_fail_runs_EIR_testneed <-
    no_fail_runs_EIR_testneed%>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_sample_runs_testneed <- add_nostorm_runs(sample_EIR, no_fail_runs_EIR_testneed, 10)

## write.csv(sample_data_testneed, "studyData/SampleData//testneed_sample_data_testneed-bumpspike.csv")
## write.csv(all_sample_runs_testneed, "studyData/SampleData/sample_resilience_tesnteed-bumpspike.csv")

## Build a profile with all needs set to 1
## need_data <- sample_data_testneed
## 
## ## Make all the needs = 1
## need_data_statusquo <-
##     need_data %>%
##     mutate(Need = 1)
## 
## write.csv(need_data_statusquo, "studyData/SampleData/statusquo_need_sample.csv")
## 
## sample_statusquo_groups <- assignGroup(need_data_statusquo)
## sample_statusquo_EIR <- calc_EIR(sample_statusquo_groups, 0)
## 
## no_fail_runs_SQ <-
##     sample_data_clean %>%
##     filter(Run == 1) %>%
##     mutate(Need = 1, Performance = 1)
## 
## no_fail_runs_SQ_groups <- assignGroup(no_fail_runs_SQ)
## 
## no_fail_runs_SQ_EIR <- calc_EIR(no_fail_runs_SQ_groups, 0)
## 
## no_fail_runs_SQ_EIR <-
##     no_fail_runs_SQ_EIR %>%
##     mutate(Strongest_Storm = 0,
##            Worst_Failure = NA,
##            End_Rec_Level = NA,
##            Number_Storms = 0)
## 
## all_sample_runs_SQ <- add_nostorm_runs(sample_EIR, no_fail_runs_SQ_EIR, 10)
## 
## 
## all_sample_statusquo <- bind_rows(sample_statusquo_EIR, no_fail_runs_SQ_EIR)
## 
## write.csv(all_sample_statusquo, "studyData/SampleData/sample_resilience_SQ")
####################################################################
## Reading csv's instead of recalculating each time
source("hurr_plots_stats.R")
## sample_EIR <- as.tibble(read.csv("studyData/SampleData/sample_resilience.csv"))
## sample_need <- as.tibble(read.csv("studyData/SampleDate/need_sample_data.csv"))

                        
## plot_EIR <- ggplot(sample_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
##     geom_boxplot() +
##     theme_bw(base_size = 12, base_family = "serif") +
##     theme(legend.margin=margin(t = 0, unit = 'cm'),
##           legend.position = "top",
##           legend.title = element_blank()) +
##     ylim(0, 1.2)
## 
## plot_storm_strength <- ggplot(sample_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
##     geom_boxplot() +
##     facet_grid(Strongest_Storm ~ Number_Storms) +
##     theme_bw(base_size = 12, base_family = "serif") +
##     theme(legend.margin=margin(t = 0, unit = 'cm'),
##           legend.position = "top",
##           legend.title = element_blank()) +
##     ylim(0, 1.2)
## 
## needProf <- example_profiles(sample_need, 41)


end_time <- proc.time() - ptm













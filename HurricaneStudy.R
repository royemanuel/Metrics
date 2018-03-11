library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
## Commented out because build it once and be done.
##source("stormDef.R")


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

 
sf_data <- ingestHurrData(study_files)
print("Data ingested from Excel")
sf_data_clean <-
    sf_data %>%
    filter(Time > 0) %>%
    gather(Infrastructure, Performance, -Run, -Time) %>%
    mutate(Performance = round(Performance / 100, 2))  %>%
    group_by(Run, Infrastructure)

mystorms <- as.tibble(read.csv("run_profiles_seed.csv"))

rising_need2yr <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.04, .95, 0.96, 1.09, .94, 1.06, 1.02))

sf_data_need <- bld_need_all(DF = sf_data_clean,
                             time_h = 525600 * 2,
                             stormlist = mystorms)#,
                             #need_inf = rising_need2yr)


storm_run_data <-
    sf_data_need %>%
    group_by(Run)

storm_summary <-
    mystorms %>%
    group_by(Run) %>%
    summarize(Strongest_Storm = max(HurricaneStrength),
              Worst_Failure = min(FailLevel),
              End_Rec_Level = min(RecoveryLevel),
              Number_Storms = n())

sf_data_groups <- assignGroup(sf_data_need)

sf_EIR <- calc_EIR(sf_data_groups, 0)

sf_EIR <- inner_join(sf_EIR, storm_summary, by = "Run")

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

all_2yr_runs <- add_nostorm_runs(sf_EIR, no_fail_runs_EIR, 541)
write.csv(sf_data_groups, "studyData/2yrData/2yr_levelNeedproportionaltotime_groups.csv")
write.csv(all_2yr_runs, "studyData/2yrData/2yr_levelNeedproportionaltotime_resilience.csv")

## Now do the same thing for Need is status quo
sf_SQ__data_need <- mutate(sf_data_clean, Need = 1)

sf_SQ__data_groups <- assignGroup(sf_SQ__data_need)

sf_SQ__EIR <- calc_EIR(sf_SQ__data_groups, 0)

sf_SQ__EIR <- inner_join(sf_SQ__EIR, storm_summary, by = "Run")

## Now build the no storm runs with a status quo need
no_fail_runs_SQ <-
    sf_data_clean %>%
    filter(Run == 1) %>%
    mutate(Performance = 1, Need = 1)

no_fail_runs_groups_SQ <- assignGroup(no_fail_runs_SQ)

no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups_SQ, 0)

no_fail_runs_EIR <-
    no_fail_runs_EIR %>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_2yr_runs_SQ <- add_nostorm_runs(sf_SQ__EIR, no_fail_runs_EIR, 541)

write.csv(sf_SQ__data_groups, "studyData/2yrData/2yr_statusquoNeed_groups.csv")
write.csv(all_2yr_runs_SQ, "studyData/2yrData/2yr_statusquoNeed_resilience.csv")
 
## Build plots that are useful and summary statistics

plot_EIR <- ggplot(sf_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
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
















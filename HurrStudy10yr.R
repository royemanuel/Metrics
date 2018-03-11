## The 10 year storm study
library("tidyverse")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")

study_files_10yr <- c("hurr10year/01MCoutput10yr.xlsx",
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

sf_data10yr <- ingestHurrData(study_files_10yr)
print("Data ingested from Excel")
sf_data10yr_clean <-
    sf_data10yr %>%
    filter(Time > 0) %>%
    gather(Infrastructure, Performance, -Run, -Time) %>%
    mutate(Performance = round(Performance / 100, 2))  %>%
    group_by(Run, Infrastructure)

my10yrstorms <- as.tibble(read.csv("run10yr_profiles_seed.csv"))

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

sf_data10yr_need <- bld_need_all(DF = sf_data10yr_clean,
                                 time_h = 525600 * 10,
                                 stormlist = my10yrstorms,
                                 need_inf = rising_need10yr)

storm_run_data10yr <-
    sf_data10yr_need %>%
    group_by(Run)

storm10yr_summary <-
    my10yrstorms %>%
    group_by(Run) %>%
    summarize(Strongest_Storm = max(HurricaneStrength),
              Worst_Failure = min(FailLevel),
              End_Rec_Level = min(RecoveryLevel),
              Number_Storms = n())

sf_data10yr_groups <- assignGroup(sf_data10yr_need)

sf_EIR10yr <- calc_EIR(sf_data10yr_groups, 0)

sf_EIR10yr <- inner_join(sf_EIR10yr, storm10yr_summary, by = "Run")

noStorms <- filter(my10yrstorms, HurricaneStrength > 24)

no_fail_runs <- zero_storm_profile(DF = sf_data10yr_clean,
                                   time_hor = max(sf_data10yr_clean$Time),
                                   emptystormlist = noStorms,
                                   need_profile = rising_need10yr)

no_fail_runs_groups <- assignGroup(no_fail_runs)

no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups, 0)

no_fail_runs_EIR <-
    no_fail_runs_EIR %>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_10yr_runs <- add_nostorm_runs(sf_EIR10yr, no_fail_runs_EIR, 18)
write.csv(sf_data10yr_groups, "studyData/10yrData/10yr_risingNeed_groups.csv")
write.csv(all_10yr_runs, "studyData/10yrData/10yr_risingNeed_resilience.csv")
## Now do the same thing for Need is status quo
sf_SQ__data10yr_need <- mutate(sf_data10yr_clean, Need = 1)

sf_SQ__data10yr_groups <- assignGroup(sf_SQ__data10yr_need)

sf_SQ__EIR <- calc_EIR(sf_SQ__data10yr_groups, 0)

sf_SQ__EIR <- inner_join(sf_SQ__EIR, storm10yr_summary, by = "Run")

## Now build the no storm runs with a status quo need
no_fail_runs_SQ <-
    sf_data10yr_groups %>%
    filter(Run == 1) %>%
    select(-diff, -Grp) %>%
    mutate(Performance = 1, Need = 1)

no_fail_runs_groups_SQ <- assignGroup(no_fail_runs_SQ)

no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups_SQ, 0)

no_fail_runs_EIR <-
    no_fail_runs_EIR %>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_10yr_runs_SQ <- add_nostorm_runs(sf_SQ__EIR, no_fail_runs_EIR, 18)

write.csv(sf_data10yr_groups, "studyData/10yrData/10yr_statusquoNeed_groups.csv")
write.csv(all_10yr_runs_SQ, "studyData/10yrData/10yr_statusquoNeed_resilience.csv")
 




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




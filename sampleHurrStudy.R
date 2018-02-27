library("tidyverse")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
## Commented out because build it once and be done.
##source("stormDef.R")


study_files <- c("HurrData547/01MCoutput.xlsx",
                 "HurrData547/02MCoutput.xlsx")
## ,
##                  "HurrData547/03MCoutput.xlsx",
##                  "HurrData547/04MCoutput.xlsx",
##                  "HurrData547/05MCoutput.xlsx",
##                  "HurrData547/06MCoutput.xlsx",
##                  "HurrData547/07MCoutput.xlsx",
##                  "HurrData547/08MCoutput.xlsx",
##                  "HurrData547/09MCoutput.xlsx",
##                  "HurrData547/10MCoutput.xlsx",
##                  "HurrData547/11MCoutput.xlsx",
##                  "HurrData547/12MCoutput.xlsx")
## 
 
sample_data <- ingestHurrData(study_files)
print("Data ingested from Excel")
sample_data_clean <-
    sample_data %>%
    filter(Time > 0) %>%
    gather(Infrastructure, Performance, -Run, -Time) %>%
    mutate(Performance = round(Performance / 100, 2))  %>%
    group_by(Run, Infrastructure)

mystorms <- as.tibble(read.csv("run_profiles_seed.csv"))

sample_data_need <- bld_need_all(DF = sample_data_clean,
                                 time_h = 525600 * 2,
                                 stormlist = mystorms,
                                 need_inf = test_need)

storm_run_data <-
    sample_data_need %>%
    group_by(Run)

storm_summary <-
    mystorms %>%
    group_by(Run) %>%
    summarize(Strongest_Storm = max(HurricaneStrength),
              Worst_Failure = min(FailLevel),
              End_Rec_Level = min(RecoveryLevel),
              Number_Storms = n())

sample_data_groups <- assignGroup(sample_data_need)

sample_EIR <- calc_EIR(sample_data_groups, 0)

sample_EIR <- inner_join(sample_EIR, storm_summary, by = "Run")

noStorms <- filter(mystorms, HurricaneStrength > 24)

no_fail_runs <- zero_storm_profile(DF = sample_data_clean,
                                   time_hor = max(sample_data_clean$Time),
                                   emptystormlist = noStorms)

no_fail_runs_groups <- assignGroup(no_fail_runs)

no_fail_runs_EIR <- calc_EIR(no_fail_runs_groups, 0)

no_fail_runs_EIR <-
    no_fail_runs_EIR %>%
    mutate(Strongest_Storm = 0,
           Worst_Failure = NA,
           End_Rec_Level = NA,
           Number_Storms = 0)

all_sample_runs <- add_nostorm_runs(sample_EIR, no_fail_runs_EIR, 10)


## Build plots that are useful and summary statistics

plot_EIR <- ggplot(sample_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() +
    theme_bw(base_size = 12, base_family = "serif") +
    theme(legend.margin=margin(t = 0, unit = 'cm'),
          legend.position = "top",
          legend.title = element_blank()) +
    ylim(0, 1.2)

plot_storm_strength <- ggplot(sample_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() +
    facet_grid(Strongest_Storm ~ Number_Storms) +
    theme_bw(base_size = 12, base_family = "serif") +
    theme(legend.margin=margin(t = 0, unit = 'cm'),
          legend.position = "top",
          legend.title = element_blank()) +
    ylim(0, 1.2)
















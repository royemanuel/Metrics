library("tidyverse")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
#source("stormDef.R")

study_files <- c("HurricaneData1_with_StormSeed/001MCoutput.xlsx",
                  "HurricaneData1_with_StormSeed/002MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/003MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/004MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/005MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/006MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/007MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/008MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/009MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/010MCoutput.xlsx",
                    "HurricaneData1_with_StormSeed/011MCoutput.xlsx")
 
sf_data <- ingestHurrData(study_files)
print("Data ingested from Excel")
sf_data_clean <-
    sf_data %>%
    filter(Time > 0) %>%
    gather(Infrastructure, Performance, -Run, -Time) %>%
    mutate(Performance = round(Performance / 100, 2))  %>%
    group_by(Run, Infrastructure)

mystorms <- as.tibble(read.csv("run_profiles_seed.csv"))

sf_data_need <- bld_need_all(sf_data_clean, mystorms)

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
















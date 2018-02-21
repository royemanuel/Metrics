library("tidyverse")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")

study_files <- c("HurricaneData1_with_StormSeed/001MCoutput.xlsx",
##                   "HurricaneData1_with_StormSeed/002MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/003MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/004MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/005MCoutput.xlsx",
                     "HurricaneData1_with_StormSeed/006MCoutput.xlsx")#,
##                     "HurricaneData1_with_StormSeed/007MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/008MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/009MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/010MCoutput.xlsx",
##                     "HurricaneData1_with_StormSeed/011MCoutput.xlsx")
 
sf_data <- ingestHurrData(study_files)
print("Data ingested from Excel")
sf_data_clean <-
    sf_data %>%
    filter(Time > 0) %>%
    gather(Infrastructure, Performance, -Run, -Time) %>%
    mutate(Performance = round(Performance / 100, 2))  %>%
    group_by(Run, Infrastructure)

mystorms <- read_excel("HurricaneData1_with_StormSeed/stormsSeed.xlsx")

sf_data_need <- bld_need_all(sf_data_clean, mystorms)

storm_run_data <-
    sf_data_need %>%
    group_by(Run) %>%
    summarize(Biggest_Storm = mean(Biggest_Storm), Number_Storms = mean(Number_Storms))

sf_data_groups <- assignGroup(sf_data_need)

sf_EIR <- calc_EIR(sf_data_groups, 0)

## Build plots that are useful and summary statistics

plot_EIR <- ggplot(sf_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() +
    theme_bw(base_size = 12, base_family = "serif") +
    theme(legend.margin=margin(t = 0, unit = 'cm'),
          legend.position = "top",
          legend.title = element_blank()) +
    ylim(0, 1.2)


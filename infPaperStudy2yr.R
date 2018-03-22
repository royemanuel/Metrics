######################################################################
## building the plots and stats for the paper                       ##
######################################################################

library("tidyverse")
library("readxl")
library("Hmisc")
source("hurr_plots_stats.R")
source("HurricaneDataPull.R")

## need_data <- as.tibble(read.csv(paste0(wd, ".csv")))
## data key:
## res = resilience values
## Xyr or Xmo = Xyr or Xmo time horizon
## Rise = uses the rising endogenous need and spikes
## SQ = uses a flat endogenous need at 1
## PR = Partial Recovery options
## FR = Full Recovery


res2yrRisePR  <- read_csv("studyData/2yrPRec/multTHwithNoStormsQ.csv") %>%
    mutate(Need_Profile = "Rising", Recovery_Type = "Partial") %>%
    select(-X1)
res2yrRiseFR  <- read_csv("studyData/2yrFullRec/multTHwithNoStormsQ.csv") %>%
    mutate(Need_Profile = "Rising", Recovery_Type = "Full") %>%
    select(-X1)
res2yrSQPR    <- read_csv("studyData/2yrPRec/multTHwithNoStormsQ_SQ.csv") %>%
    mutate(Need_Profile = "SQ", Recovery_Type = "Partial") %>%
    select(-X1)
res2yrSQFR    <- read_csv("studyData/2yrFullRec/multTHwithNoStormsQ_SQ.csv") %>%
    mutate(Need_Profile = "SQ", Recovery_Type = "Full") %>%
    select(-X1)

res2yr <- bind_rows(res2yrRisePR, res2yrRiseFR, res2yrSQPR, res2yrSQFR)

res2yr$TimeHorizon <- as.factor(res2yr$TimeHorizon)

res2yr <- res2yr %>% replace_na(list(Strongest_Storm = 0,
                                     Number_Storms = 0))

need_and_recovery_plot <- ggplot(res2yr, aes(Infrastructure,
                                             ExtendedIntegralResilience,
                                             color = TimeHorizon,
                                             shape = TimeHorizon,
                                             dodge = TimeHorizon)) +
    geom_boxplot() +
    facet_grid(Need_Profile ~ Recovery_Type)


fix_inf_table <- function(inf_vec){
    inf_vec <-
        inf_vec %>%
        str_remove("(.)Function") %>%
        str_remove("ality") %>%
        str_remove(" Availability") %>%
        str_replace("_", " ") %>%
        str_replace("IT", "Information Technology")
}

summary_stats<- function(DF){
    DF <-
        DF %>%
        mutate(Infrastructure = fix_inf_table(Infrastructure)) %>%
        group_by(TimeHorizon, Need_Profile, Recovery_Type, Infrastructure) %>%
                                        #select(-X) %>%
        summarise(Max    = round(max(ExtendedIntegralResilience), 3),
                  Min    = round(min(ExtendedIntegralResilience), 3),
                  btm    = round(quantile(ExtendedIntegralResilience, probs = 0.25), 3),
                  median = round(median(ExtendedIntegralResilience),3),
                  top    = round(quantile(ExtendedIntegralResilience, probs = 0.75),3),
                  stdev  = round(sd(ExtendedIntegralResilience), 3),
                  Avg    = round(mean(ExtendedIntegralResilience), 3)) %>%
        mutate(Range = round(Max - Min), 3)
}



r2ss <- summary_stats(res2yr)

avg_range_table <-
    r2ss %>%
    select(-btm, -median, -top, -stdev) %>%
    select(Infrastructure,
           Recovery_Type,
           Need_Profile,
           TimeHorizon,
           Min,
           Max,
           Range,
           Avg) %>%
    arrange(Recovery_Type, Infrastructure, Need_Profile)

## Save a latex text file
latex(avg_range_table, file = "")

range_and_mean_plotFR <-
    ggplot(filter(avg_range_table, Recovery_Type == "Full"),
                  aes(TimeHorizon, Avg)) +
    geom_boxplot() +
    geom_errorbar(aes(ymin = Min, ymax = Max)) +
    facet_grid(Need_Profile ~ Infrastructure)

range_and_mean_plotPR <-
    ggplot(filter(avg_range_table, Recovery_Type == "Partial"),
                  aes(TimeHorizon, Avg)) +
    geom_boxplot() +
    geom_errorbar(aes(ymin = Min, ymax = Max)) +
    facet_grid(Need_Profile ~ Infrastructure)

FR_boxplot <-
    ggplot(filter(res2yr, Recovery_Type == "Full"),
           aes(TimeHorizon, ExtendedIntegralResilience)) +
    geom_boxplot() +
    facet_grid(Need_Profile ~ Infrastructure)

PR_boxplot <-
    ggplot(filter(res2yr, Recovery_Type == "Partial"),
           aes(TimeHorizon, ExtendedIntegralResilience)) +
    geom_boxplot() +
    facet_grid(Need_Profile ~ Infrastructure)

storm_number_and_strength_plot <-
    ggplot(filter(res2yr, Recovery_Type == "Partial" &
                          Need_Profile == "Rising" &
                          Infrastructure == "Electricity_Availability" &
                          TimeHorizon == 24),
           aes(Infrastructure)) +
    geom_bar() +
    facet_grid(Number_Storms ~ Strongest_Storm)

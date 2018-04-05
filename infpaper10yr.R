######################################################################
## Building new plots for the paper based on the infPaper.R

library("tidyverse")
library("readxl")
source("hurr_plots_stats.R")
source("HurricaneDataPull.R")
library("xtable")


wd <- "studyData/10yrData/"

all_10_year <- read_csv(paste0(wd, "MultipleTimeHorizons10yrMax.csv")) %>%
    select(-X1) %>%
    mutate(TimeHorizon = as.factor(TimeHorizon),
           Strongest_Storm = as.factor(Strongest_Storm),
           Number_Storms = as.factor(Number_Storms))

plot_10yr_res <- plot_EIR(all_10_year) +
    facet_wrap(~ TimeHorizon)

stat_10yr <- table_summary(all_10_year) %>%
    ungroup %>%
    mutate(Infrastructure = fix_infrastructure(Infrastructure))

st10yr2 <- summary_stats(all_10_year) %>%
    select(Infrastructure, TimeHorizon, Avg, Min, Max) %>%
    arrange(TimeHorizon)


print.xtable(xtable(st10yr2))
## The printout of the above straighted out for easy rectangle mode
## Yanking
## Communications           & 6   & 0.99 & 0.98\textendash0.99  \\ 
## Critical Manufacturing   & 6   & 0.99 & 0.77\textendash0.99  \\ 
## Electricity Availability & 6   & 1.00 & 0.86\textendash1.00  \\ 
## Emergency Services       & 6   & 1.00 & 1.00\textendash1.00  \\ 
## Healthcare               & 6   & 1.00 & 0.99\textendash1.00  \\ 
## Information Technology   & 6   & 1.00 & 0.87\textendash1.00  \\ 
## Transportation           & 6   & 0.94 & 0.64\textendash0.95  \\ 
## Water                    & 6   & 1.00 & 0.88\textendash1.00  \\ 
## Communications           & 12  & 0.99 & 0.98\textendash0.99  \\ 
## Critical Manufacturing   & 12  & 0.97 & 0.76\textendash0.98  \\ 
## Electricity Availability & 12  & 0.99 & 0.87\textendash1.00  \\ 
## Emergency Services       & 12  & 1.00 & 1.00\textendash1.00  \\ 
## Healthcare               & 12  & 1.00 & 0.99\textendash1.00  \\ 
## Information Technology   & 12  & 1.00 & 0.76\textendash1.00  \\ 
## Transportation           & 12  & 0.93 & 0.65\textendash0.95  \\ 
## Water                    & 12  & 0.99 & 0.87\textendash0.99  \\ 
## Communications           & 24  & 0.91 & 0.91\textendash0.91  \\ 
## Critical Manufacturing   & 24  & 0.80 & 0.63\textendash0.87  \\ 
## Electricity Availability & 24  & 0.96 & 0.75\textendash1.00  \\ 
## Emergency Services       & 24  & 0.99 & 0.95\textendash1.00  \\ 
## Healthcare               & 24  & 0.95 & 0.83\textendash0.96  \\ 
## Information Technology   & 24  & 0.96 & 0.25\textendash1.00  \\ 
## Transportation           & 24  & 0.80 & 0.56\textendash0.89  \\ 
## Water                    & 24  & 0.92 & 0.72\textendash0.95  \\ 
## Communications           & 60  & 0.95 & 0.95\textendash0.95  \\ 
## Critical Manufacturing   & 60  & 0.89 & 0.69\textendash0.93  \\ 
## Electricity Availability & 60  & 0.98 & 0.77\textendash1.00  \\ 
## Emergency Services       & 60  & 1.00 & 1.00\textendash1.00  \\ 
## Healthcare               & 60  & 1.00 & 0.90\textendash1.00  \\ 
## Information Technology   & 60  & 0.98 & 0.32\textendash1.00  \\ 
## Transportation           & 60  & 0.87 & 0.60\textendash0.92  \\ 
## Water                    & 60  & 0.96 & 0.76\textendash0.98  \\ 
## Communications           & 120 & 0.91 & 0.91\textendash0.91  \\ 
## Critical Manufacturing   & 120 & 0.80 & 0.63\textendash0.87  \\ 
## Electricity Availability & 120 & 0.96 & 0.75\textendash1.00  \\ 
## Emergency Services       & 120 & 0.99 & 0.95\textendash1.00  \\ 
## Healthcare               & 120 & 0.95 & 0.83\textendash0.96  \\ 
## Information Technology   & 120 & 0.96 & 0.25\textendash1.00  \\ 
## Transportation           & 120 & 0.80 & 0.56\textendash0.89  \\ 
## Water                    & 120 & 0.92 & 0.72\textendash0.95  \\ 



















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

stat_10yr <- table_summary(all_10_year) %>%
    ungroup %>%
    mutate(Infrastructure = fix_infrastructure(Infrastructure))

st10yr2 <- summary_stats(all_10_year) %>%
    select(Infrastructure, TimeHorizon, Avg, Min, Max) %>%
    arrange(TimeHorizon)

cln_all_10yr <-
    all_10_year %>%
    mutate(Infrastructure = fix_infrastructure(Infrastructure),
           Resilience = ExtendedIntegralResilience,
           Strongest_Storm = ifelse(is.na(Strongest_Storm), 0, Strongest_Storm), 
           Number_Storms = ifelse(is.na(Number_Storms), 0, Number_Storms)) %>%
    select(-ExtendedIntegralResilience) %>%
    group_by(TimeHorizon, Infrastructure)

ssr10yr <-
    cln_all_10yr %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

ssr10yr <- inner_join(cln_all_10yr, ssr10yr)

plot_10yr_res <-
    ## New method with whiskers that cover the range
    ggplot(ssr10yr,
           aes(x = TimeHorizon,
               y = Resilience)) +
    theme_bw() +
    geom_boxplot(position="dodge",
                 aes(ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    theme(axis.text.x = element_text(angle = -45,
                                        vjust = 1,
                                     hjust = 0),
          legend.position = c(.2, .2)) +
    ylim(c(0, 1.05)) +
    facet_wrap(~ TimeHorizon, ncol = 5) +
    theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))

    ## Old method
    ## plot_EIR(ssr10yr) +
    ## facet_wrap(~ TimeHorizon, ncol = 5) +
    ## theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0))

plot_10yr_by_inf <-
    ggplot(ssr10yr,
           aes(TimeHorizon, Resilience)) +
    geom_boxplot(position="dodge",
                 aes(ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    facet_wrap(~ Infrastructure, ncol = 4) +
    theme_bw() +
    ylim(c(0,1)) +
    xlab("Time Horizon (Months)")
    

plt_10yr_by_NS <- ggplot(cln_all_10yr,
                         aes(x = Infrastructure,
                             y = Resilience,
                             group = TimeHorizon,
                             fill = TimeHorizon)) +
    geom_boxplot(position = position_dodge()) +
    facet_wrap(~ Number_Storms)

plt_10yr_by_SS <- ggplot(cln_all_10yr,
                         aes(x = Infrastructure,
                             y = Resilience,
                             group = Infrastructure)) +
    geom_boxplot(position = position_dodge()) +
    facet_grid(TimeHorizon ~ Strongest_Storm)

num_storm_summ <-
    cln_all_10yr %>%
    group_by(TimeHorizon) %>%
    filter(Number_Storms == 0,
           Infrastructure == "Water") %>%
    summarise(nostorm = n())

storm_max <-
    cln_all_10yr %>%
    group_by(TimeHorizon) %>%
    filter(Infrastructure == "Water") %>%
    summarize(maxNum = max(Number_Storms),
              Average_Number = mean(Number_Storms),
              Average_Strength = mean(Strongest_Storm))

timehorizon_storm_date <- 
    cln_all_10yr  %>%
    filter(TimeHorizon == 120,
           Number_Storms == 4,
           Infrastructure == "Healthcare",
           Strongest_Storm == 4)



## This found the storm I wanted. I chose run 204
## Sourcing the following file builds the perf/pref profile

source("example_storm.R")

run204 <-
    sf_data_need10yr %>%
    mutate(Infrastructure = fix_infrastructure(Infrastructure),
           Years = Time / (1440 * 365)) %>%
    select(-Time) %>%
    #filter(Infrastructure == "Healthcare" |
    #       Infrastructure == "Electricity") %>%
    gather(Profile, Resilience, -Run, -Years, -Infrastructure)

ex_plot <- ggplot(run204, aes(Years, Resilience,
                              group = Profile,
                              linetype = Profile)) +
    geom_line() +
    facet_wrap(~ Infrastructure, ncol = 4) +
    geom_vline(xintercept = 0.5, alpha = 0.5) +
    geom_vline(xintercept = 1, alpha = 0.5) +
    geom_vline(xintercept = 2, alpha = 0.5) +
    geom_vline(xintercept = 5, alpha = 0.5) +
    geom_vline(xintercept = 10, alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank())


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



















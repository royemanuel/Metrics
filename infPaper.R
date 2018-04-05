######################################################################
## building the plots and stats for the paper                       ##
######################################################################
######################################################################
######################################################################
######################################################################
######################################################################
## This is old ##
######################################################################
######################################################################
######################################################################

library("tidyverse")
library("readxl")
source("hurr_plots_stats.R")
source("HurricaneDataPull.R")

## First I'm going to do this with sample data, so it is not too unwieldy,
## then put the proper files in to get what I want out of the 10 and 2 year
## studies'
wd <- "studyData/2yrData/"
## need_data <- as.tibble(read.csv(paste0(wd, ".csv")))
res2yrRise  <- as.tibble(read.csv(paste0("studyData/2yrData/2yr_risingNeedproportionaltotime_resilience.csv")))
res2yrSQ    <- as.tibble(read.csv(paste0("studyData/2yrData/2yr_levelNeedproportionaltotime_resilience.csv")))
res10yrRise <- as.tibble(read.csv("studyData/10yrData/10yr_risingNeed_resilience.csv"))
res10yrSQ   <- as.tibble(read.csv("studyData/10yrData/10yr_statusquoNeed_resilience.csv"))
                         
## Infrastructure plots of Resilience
res2yrRise$Infrastructure  <- abbrev_inf(fix_infrastructure(res2yrRise$Infrastructure))
res2yrSQ$Infrastructure    <- abbrev_inf(fix_infrastructure(res2yrSQ$Infrastructure))
res10yrRise$Infrastructure <- abbrev_inf(fix_infrastructure(res10yrRise$Infrastructure))
res10yrSQ$Infrastructure   <- abbrev_inf(fix_infrastructure(res10yrSQ$Infrastructure))


resPlot_byInf_res2yrRise  <- plot_EIR(res2yrRise ) +
    labs(title = "2 Year Rising") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))
resPlot_byInf_res2yrSQ    <- plot_EIR(res2yrSQ   ) +
    labs(title = "2 Year Status Quo") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))

resPlot_byInf_res10yrRise <- plot_EIR(res10yrRise) +
    labs(title = "10 Year Rising") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))

resPlot_byInf_res10yrSQ   <- plot_EIR(res10yrSQ  ) +
    labs(title = "10 Year Status Quo") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))

ggsave(filename = "resPlot_byInf_res2yrRise.png", plot = resPlot_byInf_res2yrRise , width = 3, height = 2, units = "in")
ggsave(filename = "resPlot_byInf_res2yrSQ.png", plot = resPlot_byInf_res2yrSQ   , width = 3, height = 2, units = "in")
ggsave(filename = "resPlot_byInf_res10yrRise.png", plot = resPlot_byInf_res10yrRise, width = 3, height = 2, units = "in")
ggsave(filename = "resPlot_byInf_res10yrSQ.png", plot = resPlot_byInf_res10yrSQ  , width = 3, height = 2, units = "in")

######################################################################
## drawings with the full recovery of the electrical system
######################################################################

FRres2yrRise  <- as.tibble(read.csv(paste0("studyData/2yrFullRec/2yr_RN_resFR.csv")))
FRres2yrSQ    <- as.tibble(read.csv(paste0("studyData/2yrFullRec/2yr_levelNeed_resFR.csv")))
FRres10yrRise <- as.tibble(read.csv("studyData/10yrData/10yr_risingNeed_resilience.csv"))
FRres10yrSQ   <- as.tibble(read.csv("studyData/10yrData/10yr_statusquoNeed_resilience.csv"))
                         
## Infrastructure plots of Resilience
FRres2yrRise$Infrastructure  <- abbrev_inf(fix_infrastructure(FRres2yrRise$Infrastructure))
FRres2yrSQ$Infrastructure    <- abbrev_inf(fix_infrastructure(FRres2yrSQ$Infrastructure))
FRres10yrRise$Infrastructure <- abbrev_inf(fix_infrastructure(FRres10yrRise$Infrastructure))
FRres10yrSQ$Infrastructure   <- abbrev_inf(fix_infrastructure(FRres10yrSQ$Infrastructure))


FRresPlot_byInf_res2yrRise  <- plot_EIR(FRres2yrRise ) +
    labs(title = "2 Year Rising") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))
FRresPlot_byInf_res2yrSQ    <- plot_EIR(FRres2yrSQ   ) +
    labs(title = "2 Year Status Quo") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))

FRresPlot_byInf_res10yrRise <- plot_EIR(res10yrRise) +
    labs(title = "10 Year Rising") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))

FRresPlot_byInf_res10yrSQ   <- plot_EIR(res10yrSQ  ) +
    labs(title = "10 Year Status Quo") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = .5))

ggsave(filename = "FRresPlot_byInf_res2yrRise.png",  plot = FRresPlot_byInf_res2yrRise , width = 3, height = 2, units = "in")
ggsave(filename = "FRresPlot_byInf_res2yrSQ.png",    plot = FRresPlot_byInf_res2yrSQ   , width = 3, height = 2, units = "in")
ggsave(filename = "FRresPlot_byInf_res10yrRise.png", plot = FRresPlot_byInf_res10yrRise, width = 3, height = 2, units = "in")
ggsave(filename = "FRresPlot_byInf_res10yrSQ.png",   plot = FRresPlot_byInf_res10yrSQ  , width = 3, height = 2, units = "in")




fix_inf_table <- function(inf_vec){
    inf_vec <-
        inf_vec %>%
        str_remove("(.)Function") %>%
        str_remove("ality") %>%
        str_remove(" Availability") %>%
        str_replace("_", " ") %>%
        str_replace("IT", "Information Technology")
}

## want to edit the names for something more useful

ggsave(filename = paste0(wd, "resiliencePlot.svg"),
       plot = resPlot_byInf,
       width = 6,
       height = 3,
       units = "in")

summary_stats<- function(DF){
    DF <-
        DF %>%
        mutate(Infrastructure = fix_inf_table(Infrastructure)) %>%
        group_by(Infrastructure) %>%
                                        #select(-X) %>%
        summarise(Max    = round(max(ExtendedIntegralResilience), 3),
                  Min    = round(min(ExtendedIntegralResilience), 3),
                  btm    = round(quantile(ExtendedIntegralResilience, probs = 0.25), 3),
                  median = round(median(ExtendedIntegralResilience),3),
                  top    = round(quantile(ExtendedIntegralResilience, probs = 0.75),3),
                  stdev  = round(sd(ExtendedIntegralResilience), 3),
                  Avg    = round(mean(ExtendedIntegralResilience), 3)) %>%
        mutate(Range = Max - Min)
}

r2sq  <- stat_summary(res2yrSQ) %>% mutate(Scenario = "A2sq")
r2r   <- stat_summary(res2yrRise) %>% mutate(Scenario = "B2r")
r10sq <- stat_summary(res10yrSQ) %>% mutate(Scenario = "C10sq")
r10r  <- stat_summary(res10yrRise) %>% mutate(Scenario = "D10r")

all_summaries <- bind_rows(r2sq, 
                           r2r,  
                           r10sq,
                           r10r) 

allsumAvg <- select(all_summaries, Infrastructure, Avg, Scenario)
asaAvg <- spread(allsumAvg, Scenario, Avg)

asaMax <-
    all_summaries %>%
    select(Infrastructure, Max, Scenario) %>%
    spread(Scenario, Max)

asaMin <-
    all_summaries %>%
    select(Infrastructure, Min, Scenario) %>%
    spread(Scenario, Min)

summ_table <-
    tibble(
        Infrastructure = asaAvg$Infrastructure,
        AAvg = asaAvg$A2sq,
        AMax = asaMax$A2sq,
        AMin = asaMin$A2sq,
        BAvg = asaAvg$B2r,
        BMax = asaMax$B2r,
        BMin = asaMin$B2r,
        CAvg = asaAvg$C10sq,
        CMax = asaMax$C10sq,
        CMin = asaMin$C10sq,
        DAvg = asaAvg$D10r,
        DMax = asaMax$D10r,
        DMin = asaMin$D10r
    )


## Build the stats for the systems given a storm

res_data_given_storm <-
    res2yrRise %>%
    filter(Number_Storms > 0)

resPlot_byInf_given_storm <- plot_EIR(res_data_given_storm)

ggsave(filename = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                         "Journal Articles/Infrastructure Paper/",
                         "InfPaperOverLeaf/res2yrRise.pdf"),
       plot = resPlot_byInf_res2yrRise,
       width = 6,
       height = 3,
       units = "in")

stat_summary_given_storm <-
    res_data_given_storm %>%
    group_by(Infrastructure) %>%
    summarize(Max = max(ExtendedIntegralResilience),
              Min = min(ExtendedIntegralResilience),
              btm = quantile(ExtendedIntegralResilience, probs = 0.25),
              median = median(ExtendedIntegralResilience),
              top = quantile(ExtendedIntegralResilience, probs = 0.75),
              stdev = sd(ExtendedIntegralResilience))


## Summarize by storm strength

stat_summ_storm_strength <- function(DF){
    DF <-
        DF %>%
        mutate(Infrastructure = fix_inf_table(Infrastructure)) %>%
        group_by(Infrastructure, Strongest_Storm) %>%
                                        #select(-X) %>%
        summarise(Max    = round(max(ExtendedIntegralResilience), 3),
                  Min    = round(min(ExtendedIntegralResilience), 3),
                  btm    = round(quantile(ExtendedIntegralResilience, probs = 0.25), 3),
                  median = round(median(ExtendedIntegralResilience),3),
                  top    = round(quantile(ExtendedIntegralResilience, probs = 0.75),3),
                  stdev  = round(sd(ExtendedIntegralResilience), 3),
                  Avg    = round(mean(ExtendedIntegralResilience), 3)) %>%
        mutate(Range = Max - Min)
}

r2sqSS  <- stat_summ_storm_strength(res2yrSQ)
r2rSS   <- stat_summ_storm_strength(res2yrRise)
r10sqSS <- stat_summ_storm_strength(res10yrSQ)
r10rSS  <- stat_summ_storm_strength(res10yrRise)

all_summaries_SS<- bind_rows(r2sqSS, 
                           r2rSS,  
                           r10sqSS,
                           r10rSS) 
    
## All the data together:

res2yrRise  <- res2yrRise  %>% mutate(Scenario = "Cres2yrRise"  )
res2yrSQ    <- res2yrSQ    %>% mutate(Scenario = "Ares2yrSQ"    )
res10yrRise <- res10yrRise %>% mutate(Scenario = "Dres10yrRise" )
res10yrSQ   <- res10yrSQ   %>% mutate(Scenario = "Bres10yrSQ"   )

all_res_data <- bind_rows(res2yrRise,res2yrSQ ,res10yrRise, res10yrSQ) %>%
    mutate(Infrastructure = fix_infrastructure(Infrastructure))



inf_by_scen_plot <- ggplot(all_res_data,
                           aes(Strongest_Storm,
                               ExtendedIntegralResilience,
                               group = Strongest_Storm)) +
    geom_boxplot() +
    facet_grid(Infrastructure ~ Scenario) +
    theme_bw(base_size = 12,
             base_family = "serif")

ggsave(plot = inf_by_scen_plot, filename = "inf_byScenTest.png",
       width = 3.25, height = 9)


















######################################################################
## Building the plots for the paper now that I have firm data produced
## by the workingFleetData.R file

library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience")

if(!exists("PMdata") | !exists("COGraddata") | !exists("COGraddata")){
    print("loading data...")
    currentData <- "9-8-2018--9-24.csv"
    PMdata <- read_csv(paste0("PMRes", currentData),
                       col_types = list(col_character(),
                                        col_double(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_double(),
                                        col_character())) %>%
        mutate(ExpInt = as.integer(Exp),
               Surge = ifelse(ExpInt < 4, "No Surge", "Surge"),
               ExpDesc = ifelse(ExpInt == 1 | ExpInt == 4,
                                "No SLEP",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Big SLEP")))
    COGraddata <- read_csv(paste0("COGrad", currentData),
                       col_types = list(col_character(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_character())) %>%
        mutate(ExpInt = as.integer(Exp),
               Surge = ifelse(ExpInt < 4, "No Surge", "Surge"),
               ExpDesc = ifelse(ExpInt == 1 | ExpInt == 4,
                                "No SLEP",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Big SLEP")))
    COSatdata <- read_csv(paste0("COSat", currentData),
                       col_types = list(col_character(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character())) %>%
        mutate(ExpInt = as.integer(Exp),
               Surge = ifelse(ExpInt < 4, "No Surge", "Surge"),
               ExpDesc = ifelse(ExpInt == 1 | ExpInt == 4,
                                "No SLEP",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Big SLEP")))
}

## Prep the data for use. CO's, Exp, Runs, Seed should all be characters
## Quick plot of CO's Resilience satisfaction

COSatdataPrepped <-
    COSatdata %>%
    group_by(Seed, Exp, Run, SqCO)

COSatPlot <- ggplot(COSatdata, aes(SqCO, Resilience, color = Exp)) +
    geom_boxplot(position = "dodge")

+
    facet_wrap(~ Exp, ncol = 1)



















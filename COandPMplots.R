######################################################################
## Building the plots for the paper now that I have firm data produced
## by the workingFleetData.R file

library("tidyverse")
setwd("d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience")


if(!exists("PMdata") | !exists("COGraddata") | !exists("COGraddata")){
    print("loading data...")
    currentData <- "9-9-2018--8-54.csv"
    PMdata <- read_csv(paste0("PMRes", currentData),
                       col_types = list(col_character(),
                                        col_double(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_double(),
                                        col_character())) %>%
        mutate(ExpInt = as.integer(Experiment),
               Surge = ifelse(ExpInt < 4, "No Surge", "Surge"),
               ExpDesc = ifelse(ExpInt == 1 | ExpInt == 4,
                                "No SLEP",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Big SLEP")))
    COGraddata <- read_csv(paste0("COGrad", currentData),
                       col_types = list(col_integer(),
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
                                "Big SLEP")),
               SqCO = LETTERS[SqCO + 1])
    COSatdata <- read_csv(paste0("COSat", currentData),
                       col_types = list(col_integer(),
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
                                "Big SLEP")),
               SqCO = LETTERS[SqCO + 1])
}

## I have a bunch of seed starts that did not complete every run
## Sorting through to find these seeds

seedExpCount <-
    PMdata %>%
    group_by(Seed, Experiment) %>%
    summarise(numRuns = n())

seedDiffExpCounts <-
    seedExpCount %>%
    summarise(ExpWithDiffRunNumber = length(unique(numRuns))) %>%
    filter(ExpWithDiffRunNumber > 1)

'%ni%' <- Negate('%in%')

PMdata <-
    PMdata %>%
    filter(Seed %ni% seedDiffExpCounts$Seed)

COSatdata <-
    COSatdata %>%
    filter(Seed %ni% seedDiffExpCounts$Seed)

COGraddata <-
    COGraddata %>%
    filter(Seed %ni% seedDiffExpCounts$Seed)

## Check that the job was done
checkRuns <- 
    PMdata %>%
    group_by(Seed, Experiment) %>%
    summarise(numRuns = n()) 

checkSeedDiffExpCounts <-
    checkRuns %>%
    summarise(ExpWithDiffRunNumber = length(unique(numRuns))) %>%
    filter(ExpWithDiffRunNumber > 1)

TotalRuns <-
    checkRuns %>%
    ungroup(.) %>%
    group_by(Experiment) %>%
    summarise(RunsPerExp = sum(numRuns) / 14)

if(dim(checkSeedDiffExpCounts)[1] == 0){
    print(paste("You got rid of all the asymmetric runs!!! You have",
                TotalRuns$RunsPerExp, "total runs per experiment"))
} else {
    print("Something is WRONG!!! :'(")
}

## Prep the data for use. CO's, Exp, Runs, Seed should all be characters
## Quick plot of CO's Resilience satisfaction


COSatdataPrepped <-
    COSatdata %>%
    group_by(Seed, Exp, Run, SqCO)

COSatPlot <- ggplot(COSatdata, aes(SqCO, Resilience)) +
    geom_boxplot(position = "dodge") +
    facet_grid(ExpDesc ~ Surge)

COGrad0and1 <-
    COGraddata %>%
    filter(Chi == "1" | Chi == "11" | Chi == "14") %>%
    mutate(Chi = ifelse(Chi == "1", "Ephemeral",
                        ifelse(Chi == "11", "Adjacent", "Permanent")))
COGradPlot <- ggplot(COGrad0and1, aes(SqCO, Resilience, color = Chi)) +
    geom_boxplot(position = "dodge") +
    facet_grid(ExpDesc ~ Surge)




















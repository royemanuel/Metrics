######################################################################
## Building the plots for the paper now that I have firm data produced
## by the workingFleetData.R file

library("tidyverse")
library("xtable")
setwd("d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience")


if(!exists("PMdata") | !exists("COGraddata") | !exists("COSatdata") |!exists("PMdata")){
    print("loading data...")
    currentData <- "9-15-2018--9-56.csv"
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
                                "As-Is",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Large SLEP")))
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
                                "As-Is",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Large SLEP")),
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
                                "As-Is",
                         ifelse(ExpInt == 2 | ExpInt == 5,
                                "Small SLEP",
                                "Large SLEP")),
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

COSatPlot <- ggplot(COSatdata, aes(SqCO, Resilience, color = ExpDesc)) +
    geom_boxplot(position = "dodge") +
    facet_grid(Surge ~ .)

COGrad0and1 <-
    COGraddata %>%
    filter(Chi == "1" | Chi == "11" | Chi == "14") %>%
    mutate(Chi = ifelse(Chi == "1", "Ephemeral",
                 ifelse(Chi == "11", "Adjacent", "Permanent")))

COGradPlot <- ggplot(COGrad0and1, aes(SqCO, Resilience, color = ExpDesc)) +
    geom_boxplot(position = "dodge") +
    facet_grid(Chi ~ Surge)

## Build the percentiles for each so you can boxplot them.

COGradBPdata_E <-
    COGraddata %>%
    ungroup(.) %>%
    select(-ExpInt, -Exp) %>%
    group_by(SqCO, ExpDesc, Surge, Chi) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75)) %>%
    filter(Chi == "1" | Chi == "13" | Chi == "14") %>%
    mutate(Chi = ifelse(Chi == "1", "Ephemeral",
                 ifelse(Chi == "13", "Adjacent", "Permanent"))) %>%
    filter(SqCO == "E")

COGradBPdata <-
    COGraddata %>%
    ungroup(.) %>%
    select(-ExpInt, -Exp) %>%
    group_by(SqCO, ExpDesc, Surge, Chi) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75),
              meanY = mean(Resilience)) %>%
    ## Going to only ephemeral to keep with everything and make it
    ## viewable
    filter(Chi == "1") # %>%
    ## mutate(Chi = ifelse(Chi == "1", "Ephemeral",
    ##              ifelse(Chi == "13", "C4", "Permanent")))
    

COGradsumPlot_CO_E <-
    ggplot(COGradBPdata_E) +
    geom_boxplot(position="dodge",
                 aes(x = Chi,
                     ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    facet_grid(Surge ~ ExpDesc) +
    theme_bw() 
                      )

COGradsumPlot_CO <-
    ggplot(COGradBPdata) +
    geom_boxplot(
                 aes(x = SqCO,
                     ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    facet_grid(ExpDesc ~ Surge) +
    theme_bw() +
    labs(x = "Squadron Commanding Officers",
         y = "Resilience",
         fill = "Intertemporal\nSubstitutability")

## Plots for selected adjacentness



COSatBPdata <-
    COSatdata %>%
    ungroup(.) %>%
    select(-ExpInt, -Exp) %>%
    group_by(SqCO, ExpDesc, Surge) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

COSatsumPlot <-
    ggplot(COSatBPdata) +
    geom_boxplot(position="dodge",
                 aes(x = SqCO,
                     ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    facet_grid(ExpDesc ~ Surge) +
    theme_bw()



PMSatBPdata <-
    PMdata %>%
    ungroup(.) %>%
    filter(Chi == 1) %>%
    mutate(Resilience = SAT) %>%
    select(-ExpInt, -SAT, -GRAD, -Ao) %>%
    group_by( TimeHorizon, ExpDesc, Surge) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

PMSatsumPlot <-
    ggplot(PMSatBPdata) +
    geom_boxplot(
                 aes(x = TimeHorizon,
                     ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    facet_grid(Surge ~ ExpDesc) +
    theme_bw()



PMGradBPdata <-
    PMdata %>%
    ungroup(.) %>%
    filter(Chi == 1 | Chi == 13 | Chi == 14) %>%
    mutate(Chi = ifelse(Chi == "1", "Ephemeral",
                 ifelse(Chi == "13", "Adjacent", "Permanent"))) %>%
    mutate(Resilience = GRAD) %>%
    select(-ExpInt,  -SAT, -GRAD, -Ao) %>%
    group_by( TimeHorizon, ExpDesc, Surge, Chi) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

PMGradsumPlot <-
    ggplot(PMGradBPdata) +
    geom_boxplot(
        aes(x = TimeHorizon,
            ymin = minY,
            ymax = maxY,
            lower = lowerY,
            middle = middleY,
            upper = upperY,
            fill = Chi,),
        stat = "identity") +
    facet_grid(Surge ~ ExpDesc) +
    theme_bw()


PMAoBPdata <-
    PMdata %>%
    ungroup(.) %>%
    filter(Chi == 1) %>%
    mutate(Resilience = Ao) %>%
    select(-ExpInt,  -SAT, -GRAD, -Ao) %>%
    group_by( TimeHorizon, ExpDesc, Surge) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

PMAosumPlot <-
    ggplot(PMAoBPdata) +
    geom_boxplot(
                 aes(x = TimeHorizon,
                     ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upperY),
                     stat = "identity") +
    facet_grid(Surge ~ ExpDesc) +
    theme_bw()

######################################################################
## Building the "ratings" for courses of action. That is, which slep
## decision works best for each scenario and functional output

PMGradRating <-
    PMGradBPdata %>%
    ungroup(.) %>%
    group_by(Surge, TimeHorizon, Chi) %>%
    filter(middleY == max(middleY)) %>%
    arrange(
            Surge, TimeHorizon, Chi) %>%
    select(Surge, TimeHorizon, Chi, ExpDesc, middleY)

PMAoRating <-
    PMAoBPdata %>%
    ungroup(.) %>%
    group_by(Surge, TimeHorizon) %>%
    filter(middleY == max(middleY)) %>%
    arrange(
            Surge, TimeHorizon) %>%
    select(Surge, TimeHorizon,ExpDesc, middleY)

PMSatRating <-
    PMSatBPdata %>%
    ungroup(.) %>%
    group_by(Surge, TimeHorizon) %>%
    filter(middleY == max(middleY)) %>%
    arrange(
            Surge, TimeHorizon) %>%
    select(Surge, TimeHorizon,ExpDesc, middleY)

######################################################################
## Let's build some LaTeX tables


PMAo <- xtable(PMAoBPdata)
ltxPMGr <-
    PMGradBPdata %>%
    select( Surge, ExpDesc, Chi, minY, middleY, maxY) %>%
    mutate(Chi = ifelse(Chi == "1", "Ephemeral",
                 ifelse(Chi == "13", "Adjacent", "Permanent"))) %>%
    arrange(Surge)
PMGr <- xtable(ltxPMGr)

PMSa <- xtable(PMSatBPdata)

ltxCOGR_E <-
    COGradBPdata_E %>%
    select( Surge, ExpDesc, Chi, minY, middleY, maxY) %>%
    mutate(Chi = ifelse(Chi == "1", "Ephemeral",
                 ifelse(Chi == "13", "Adjacent", "Permanent"))) %>%
    arrange(Surge)
COGr_E<- xtable(ltxCOGR_E)


######################################################################
## Chi analysis

    
PMGradChidata <-
    PMdata %>%
    ungroup(.) %>%
    mutate(Resilience = GRAD) %>%
    select(-ExpInt,  -SAT, -GRAD, -Ao) %>%
    group_by( TimeHorizon, ExpDesc, Surge, Chi) %>%
    filter(Surge == "Surge",
           Chi != "6") %>%
    filter(ExpDesc == "Large SLEP" | ExpDesc == "Small SLEP") %>%
    filter(TimeHorizon == 30) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75)) %>%
    mutate(Chi = ifelse(Chi == "1", "E",
                 ifelse(Chi == "2", "A1",
                 ifelse(Chi == "3", "A2",
                 ifelse(Chi == "4", "A3",
                 ifelse(Chi == "5", "A4",
                 ifelse(Chi == "7", "B1",
                 ifelse(Chi == "8", "B2",
                 ifelse(Chi == "9", "B3",
                 ifelse(Chi == "10", "B4",
                 ifelse(Chi == "11", "C1",
                 ifelse(Chi == "12", "C2",
                 ifelse(Chi == "13", "C3", "P"  )))))))))))))


PMGradChiPlot <-
    ggplot(PMGradChidata) +
    geom_boxplot(
        aes(x = Chi,
            ymin = minY,
            ymax = maxY,
            lower = lowerY,
            middle = middleY,
            upper = upperY,
            fill = ExpDesc),
        stat = "identity") +
    theme_bw()

COGradChidata <-
    COGraddata %>%
    ungroup(.) %>%
    select(-ExpInt) %>%
    group_by( SqCO, ExpDesc, Surge, Chi) %>%
    filter(Surge == "Surge",
           Chi != "6") %>%
    filter(Chi != "5") %>%
    filter(Chi != "10") %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75)) %>%
    mutate(Chi = ifelse(Chi == "1", "E",
                 ifelse(Chi == "2", "A1",
                 ifelse(Chi == "4", "A2",
                 ifelse(Chi == "3", "A3",
                 ifelse(Chi == "5", "A4",
                 ifelse(Chi == "7", "B1",
                 ifelse(Chi == "9", "B2",
                 ifelse(Chi == "8", "B3",
                 ifelse(Chi == "10", "B4",
                 ifelse(Chi == "11", "C1",
                 ifelse(Chi == "12", "C2",
                 ifelse(Chi == "13", "C3", "P"  )))))))))))))    

COGradChiPlot <-
    ggplot(COGradChidata) +
    geom_boxplot(
        aes(x = Chi,
            ymin = minY,
            ymax = maxY,
            lower = lowerY,
            middle = middleY,
            upper = upperY,
            fill = ExpDesc),
        stat = "identity") +
    theme_bw() 


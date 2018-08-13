## Plots for the proposed fleet resilience paper.

library("tidyverse")

setwd("d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience")
ar0 <- read_csv("allRes6JUL.csv", col_types = list(col_character(),
                                        col_double(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_double(),
                                        col_character()))
ar1 <- read_csv("allRes7JUL.csv", col_types = list(col_character(),
                                        col_double(),
                                        col_double(),
                                        col_character(),
                                        col_character(),
                                        col_character(),
                                        col_double(),
                                        col_character()))

ar0 <-
    bind_rows(ar0, ar1) %>%
    mutate(ExpInt = as.integer(Experiment),
           Surge = ifelse(ExpInt < 4, "No Surge", "Surge"),
           ExpDesc = ifelse(ExpInt == 1 | ExpInt == 4,
                            "No SLEP",
                     ifelse(ExpInt == 2 | ExpInt == 5,
                            "Small SLEP",
                            "Big SLEP")))
######################################################################
## Summarize the statistics
## First the stats without the graduates

arNoGrad <-
    ar0 %>%
    filter(Chi == 1) %>%
    select(SAT, Ao, Experiment, TimeHorizon) 

arNoGradStat <-
    arNoGrad %>%
    group_by(Experiment, TimeHorizon) %>%
    summarise(SAT_MEAN = mean(SAT),
              Ao_MEAN = mean(Ao),
              SAT_MAX = max(SAT),
              Ao_MAX = max(Ao),
              SAT_MIN = min(SAT),
              Ao_MIN = min(Ao))

## Now with graduates
arGrad <-
    ar0 %>%
    select(GRAD, Experiment, TimeHorizon, Chi)

arGradStat <-
    arGrad %>%
    group_by(Experiment, TimeHorizon, Chi) %>%
    summarise(GRAD_MEAN = mean(GRAD),
              GRAD_MAX = max(GRAD),
              GRAD_MIN = min(GRAD))
######################################################################
## Plots with no graduate data             

arNoGradGather <-
    arNoGrad %>%
    gather(Type, Resilience, -Experiment, -TimeHorizon)
        
pltNoGrad <-
    ggplot(arNoGradGather, aes(Experiment, Resilience)) +
    geom_boxplot(position="dodge") +
    facet_grid(Type ~ TimeHorizon)

## Graduate Data Plot

arGradGather <-
    arGrad %>%
    gather(Type, Resilience, -Experiment, -TimeHorizon, -Chi)

pltGrad <-
    ggplot(arGradGather,
           aes(Chi, Resilience, fill = Chi)) +
    geom_boxplot(position = "dodge") +
    facet_grid(Type ~ Experiment)

arGradChi1379 <-
    arGradGather %>%
    filter(Chi == "1" |
           Chi == "3" |
           Chi == "7" |
           Chi == "9" )

arGradChi1379stat <-
    arGradChi1379 %>%
    group_by(TimeHorizon, Experiment, Chi) %>%
    summarise(MAX = max(Resilience),
              MIN = min(Resilience),
              MEAN = mean(Resilience),
              tukMED = fivenum(Resilience)[3],
              tukBH = fivenum(Resilience)[2],
              tukUH = fivenum(Resilience)[4],
              ) %>%
    gather(Stat, Value, -TimeHorizon, -Experiment, -Chi)

aGCsPlot <-
    ggplot(arGradChi1379stat,
           aes(Experiment, Value, colour = Stat, shape = Chi)) +
    geom_point() +
    facet_grid(Chi ~ TimeHorizon )


pltGrad1379byExp <-
    ggplot(arGradChi1379, aes(Experiment, Resilience, fill = Chi)) +
    geom_boxplot(position = "dodge") +
    facet_wrap(~ TimeHorizon)

pltGrad1379byChi <-
    ggplot(arGradChi1379, aes(Chi, Resilience, fill = Experiment)) +
    geom_boxplot(position = "dodge") +
    facet_wrap(~ TimeHorizon)
######################################################################
## Plots for availability and student satisfaction

ar0forSatAo <-
    ar0 %>%
    select(SAT, Ao, Experiment, TimeHorizon)

AoStudRes <-
    ar0forSatAo %>%
    mutate(TimeHorizon = paste(TimeHorizon, "years")) %>%
    mutate(Experiment = as.character(Experiment)) %>%
    gather(Type, Resilience, -TimeHorizon, -Experiment)


AoStudPlot <-
    ggplot(AoStudRes,
           aes(Experiment, Resilience, group=Experiment)) +
    geom_boxplot() +
    facet_grid(Type ~ TimeHorizon) +
    theme_bw()

######################################################################
## Plots for graduation rates
## I don't know what the purpose of the folloowing is, so I am commenting
## it out'
## chiRes <-
##     bind_rows(ar0, ar1,  ar0forSatAo) %>%
##     mutate(Experiment = as.character(Experiment)) %>%
##     select(-Run, -Seed, -Ao, -SAT)

## allResPrep <-
##     chiRes %>%
##     mutate(TimeHorizon = paste(TimeHorizon, "years")) %>%
##     # select(-Run, -Seed) %>%
##     group_by(Chi, TimeHorizon, Experiment) %>%
##     gather(Type, Resilience, -TimeHorizon, -Chi, -Experiment, -Surge, -ExpDesc)

## allParamPlot <-
##     ggplot(allResPrep) +
##     theme_bw() +
##     geom_boxplot(aes(x = Experiment,
##                      y = Resilience,
##                      group = Experiment),
##                  position = "dodge") +
##     scale_colour_manual(c("grey90", "grey70", "grey50", "grey30")) +
##     facet_grid(Type ~ TimeHorizon)

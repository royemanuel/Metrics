library("tidyverse")
library("readxl")

skedTracker1 <- read_csv("skedTrackerExp1Run0RS605702648.csv") %>%
    mutate(Exp = "1")
skedTracker2 <- read_csv("skedTrackerExp2Run0RS605702648.csv") %>%
    mutate(Exp = "2")
skedTracker3 <- read_csv("skedTrackerExp3Run0RS605702648.csv") %>%
    mutate(Exp = "3")
skedTracker4 <- read_csv("skedTrackerExp4Run0RS605702648.csv") %>%
    mutate(Exp = "4")
skedTracker5 <- read_csv("skedTrackerExp5Run0RS605702648.csv") %>%
    mutate(Exp = "5")
skedTracker6 <- read_csv("skedTrackerExp6Run0RS605702648.csv") %>%
    mutate(Exp = "6")

skedTracker <- bind_rows(skedTracker1,
                         skedTracker2,
                         skedTracker3,
                         skedTracker4,
                         skedTracker5,
                         skedTracker6)

skedTrackerPlotter <-
    skedTracker %>%
    select(-X1, -Day, -instructors) %>%
    gather(Category, Count, -Time, -Exp) %>%
    mutate(Time = Time / (24*365))

studPlot <-
    ggplot(skedTrackerPlotter,
           aes(Time, Count,
               group = Category,
               color = Category)) +
    geom_line() +
    facet_wrap(~ Exp, ncol = 1)

skedTrackerNG <-
    skedTrackerPlotter %>%
    filter(Category != "graduates",
           Category != "attrites",
           Category != "students",
           Category != "boneYard",
           Category != "flightLine") %>%
    mutate(Perce ntage = Count / 50,
           Exp = map(nameExperiments(Exp)) %>%
    select( -Count)

studPlotNoGrads <-
    ggplot(skedTrackerNG,
           aes(Time, Percentage,
               group = Category,
               linetype = Category)) +
    geom_line() +
    facet_wrap(~ Exp, ncol = 1) +
    geom_hline(yintercept = 0.85,
               colour = "grey70") +
    theme_bw()

nameExperiments <- function(expList){
    switch(expList,
           1 == "No SLEP, Constant Student Rate",
           2 == "SLEP to 14,400 Hours, Constant Student Rate",
           3 == "SLEP to 18,000 Hours, Constant Student Rate",
           4 == "No SLEP, Student Surge",
           5 == "SLEP to 14,400 Hours, Student Surge",
           6 == "SLEP to 18,000 Hours, Student Surge")
}


studPlotNoGrads



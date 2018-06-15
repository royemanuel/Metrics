library("tidyverse")
library("readxl")

skedTracker <- read_csv("skedTrackerrun6.csv")

skedTracker <-
    skedTracker %>%
    select(-X1, -Day, -instructors) %>%
    gather(Category, Count, -Time)

studPlot <-
    ggplot(skedTracker,
           aes(Time, Count,
               group = Category,
               color = Category)) +
    geom_line()

skedTrackerNG <-
    skedTracker %>%
    filter(Category != "graduates",
           Category != "attrites")

studPlotNoGrads <-
    ggplot(skedTrackerNG,
           aes(Time, Count,
               group = Category,
               color = Category)) +
    geom_line()





















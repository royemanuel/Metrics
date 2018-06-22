library("tidyverse")
library("readxl")

skedTracker <- read_csv("skedTrackerExp16Run1.csv")

skedTracker <-
    skedTracker %>%
    select(-X1, -Day, -instructors) %>%
    gather(Category, Count, -Time) %>%
    mutate(Time = Time / (24*365))

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





















library("tidyverse")
library("readxl")

skedTracker <- read_csv("skedTrackerExp1Run1.csv")

skedTrackerPlotter <-
    skedTracker %>%
    select(-X1, -Day, -instructors) %>%
    gather(Category, Count, -Time) %>%
    mutate(Time = Time / (24*365))

studPlot <-
    ggplot(skedTrackerPlotter,
           aes(Time, Count,
               group = Category,
               color = Category)) +
    geom_line()

skedTrackerNG <-
    skedTrackerPlotter %>%
    filter(Category != "graduates",
           Category != "attrites",
           Category != "students")

studPlotNoGrads <-
    ggplot(skedTrackerNG,
           aes(Time, Count,
               group = Category,
               color = Category)) +
    geom_line()

studPlotNoGrads



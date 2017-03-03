## A script to pull in the data from the AnyLogic Model
mdl <- read.csv("singleRunOutput.csv")
mdl <- mdl %>% select(-Electric.Degrade) %>%
    melt(id.vars = "Time", na.rm = TRUE)

infraPlot <- ggplot(mdl, aes(Time, value), colors = variable) + facet_wrap(~  variable, ncol = 2) +
    geom_line()

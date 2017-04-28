## This is obsolete to the buildElecDegTimePlot.R file
f20 <- read.csv("20percentFailsingleRunOutput.csv")
f20$Profile <- "Fail 20"
rec100 <- read.csv("100percentRecoverysingleRunOutput.csv")
rec100$Profile <- "100 Percent Recovery"
asIsOut <- read.csv("AsIsSingleRunOutput.csv")
asIsOut$Profile <- "As Is"
f5000 <- read.csv("fail5000singleRunOutput.csv")
f5000$Profile <- "Recover at 5000"
stpRec <- read.csv("SteppedRecoverysingleRunOutput.csv")
stpRec$Profile <- "Stepped Recovery"
allOutput <- bind_rows(f20,
                       rec100,
                       asIsOut,
                       f5000,
                       stpRec)
elecAvail <- allOutput %>%
    select(Time, Electricity.Availability, Profile) %>%
        mutate(Time = Time - 41,
               Electricity.Availability = Electricity.Availability / 100)

######################################################################
## Build the plot of the electric performance

elecAvail <- melt(elecAvail, id.vars = c("Time", "Profile"))

elecAvailPlot <- ggplot(elecAvail, aes(Time, value, group = Profile)) +
    facet_wrap(~ Profile, nrow = 5) + geom_line()

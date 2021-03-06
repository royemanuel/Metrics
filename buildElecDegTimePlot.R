## Pull in the .csv's to build a time plot of the electricity degrade
## for the papers to have the visualization

## Read the .csv's and keep only the time and electricity profiles.
## Add a column so they are identified.

## List the files you want to put in.
## I am  going to brute force this one. This is a specific file. Do
## not change the inputs. This is related to the particular files in
## the folder this is in.

a <- read.csv("bigAsIs.csv")
cola <- colnames(a)
cola[1] <- "Time"
colnames(a) <- cola
a <- mutate(a, Scenario = "A")

b <- read.csv("big16kRec.csv")
colb <- colnames(b)
colb[1] <- "Time"
colnames(b) <- colb
b <- mutate(b, Scenario = "B")

c <- read.csv("big100percentRec.csv")
colc <- colnames(c)
colc[1] <- "Time"
colnames(c) <- colc
c <- mutate(c, Scenario = "C")


d <- read.csv("bigRob.csv")
cold <- colnames(d)
cold[1] <- "Time"
colnames(d) <- cold
d <- mutate(d, Scenario = "D")

e <- read.csv("bigStep.csv")
cole <- colnames(e)
cole[1] <- "Time"
colnames(e) <- cole
e <- mutate(e, Scenario = "E")

allCSV <- bind_rows(a, b, c, d, e)
allCSV <- allCSV %>%
        mutate(Time = Time - 40) %>%
            filter(Time <= 39000) %>%
                mutate(Days = Time / 1440) %>%
                    select(-Time)

allCSVed <- allCSV %>%
    select(Days, Electricity.Availability, Scenario) %>%
                melt(id.vars = c("Days", "Scenario")) %>%
                    mutate(Functionality = value) %>%
                        select(-value)

plotED <- ggplot(allCSVed, aes(Days, Functionality, group = Scenario)) +
    geom_line() + scale_linetype_discrete(name = "Scenario") +
        facet_wrap(~ Scenario, ncol = 5) +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = c(.85, .15))

## Only activate this when you need to save a new image#
## ggsave(filename = "EDplot.png",
##        plot = last_plot(),
##        width = 6.5,
##        height = 2.5)
#####################################################################
##
## Now we pull all the performance values for each of the infrastructure
## functions and build a plot

allCSVfunc <- allCSV %>%
    select(2:9, 18, 19) %>%
        mutate(Electricity = Electricity.Availability,
               Communications = Communications.Function,
               IT = IT.Function,
               Healthcare = Healthcare.Function,
               Transportation = Transportation.Function,
               Emergency = Emergency.Services.Functionality,
               Manufacturing = Critical.Manufacturing.Functionality,
               Water = Water.Functionality) %>%
                   select(9:18) %>%
                       melt(id.vars = c("Days", "Scenario")) %>%
                           mutate(Functionality = value) %>%
                               select(-value)

plotInfFunc <- ggplot(allCSVfunc, aes(Days, Functionality)) +
    facet_grid(variable ~ Scenario) +
        geom_line() +
            theme_bw(base_size = 8, base_family = "serif")

## ggsave(filename = "InfPerfplot.png",
##        plot = last_plot(),
##        width = 6.5,
##        height = 6)

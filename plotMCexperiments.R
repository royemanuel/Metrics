## I want to plot the MC experiments from AnyLogic in order to see
## what they look like

source("metrics.R")
source("anyLogicDataPull.R")

## You have to put in the file name here
k <- read.csv("JUL31rand50sim.csv")
k <- melt(k, id.vars = c("Run", "Time"))

kPlot <- ggplot(k, aes(Time, value, group = Run)) + geom_line() +
    facet_wrap(~ variable)

eDegrade <- filter(k, variable == "Electric.Degrade")

edPlot <- ggplot(eDegrade, aes(Time, value, group = Run)) + geom_line()

k <- k %>% mutate(Infrastructure = variable) %>% select(-variable)
timeMaxMin <- summarize(group_by(k, Infrastructure, Time),
                        MAX = max(value),
                        MIN = min(value),
                       MEAN = mean(value))

timeMaxMin <- melt(timeMaxMin, id.vars = c("Infrastructure", "Time"))

tmmPlot <- ggplot(timeMaxMin, aes(Time, value, group = variable)) +
    geom_line() + facet_wrap(~ Infrastructure)

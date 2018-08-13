library("tidyverse")
library("readxl")
source("fleetRes.R")
xp1 <- read_csv("fleetData/RFR2/skedTrackerExp1Run0RS68330191.csv") %>%
    mutate(Exp = "1")
xp2 <- read_csv("fleetData/RFR2/skedTrackerExp2Run0RS68330191.csv") %>%
    mutate(Exp = "2")
xp3 <- read_csv("fleetData/RFR2/skedTrackerExp3Run0RS68330191.csv") %>%
    mutate(Exp = "3")
xp4 <- read_csv("fleetData/RFR2/skedTrackerExp4Run0RS68330191.csv") %>%
    mutate(Exp = "4")
xp5 <- read_csv("fleetData/RFR2/skedTrackerExp5Run0RS68330191.csv") %>%
    mutate(Exp = "5")
xp6 <- read_csv("fleetData/RFR2/skedTrackerExp6Run0RS68330191.csv") %>%
    mutate(Exp = "6")

xpAll <- bind_rows(xp1, xp2, xp3, xp4, xp5, xp6)

xp <-
    xpAll %>%
    select(Time, flightLine, SLEPlist, boneYard, Exp) %>%
    mutate(Time = Time / (24*365)) %>%
    gather(AClocation, numAC, -Time, -Exp)

xpACPlot <- ggplot(xp, aes(Time, numAC, colour = AClocation)) +
    geom_line() +
    geom_hline(aes(yintercept = 42.5)) +
    facet_wrap(~ Exp, ncol = 1)

xQ <- list()
for (x in 1:length(unique(xpAll$Exp))){
    xDF <- filter(xpAll, Exp == paste(x))
    xQ[[x]] <- qrtrly_grads(xDF) %>% mutate(Exp = paste(x))
}

xpGrad <- bind_rows(xQ) %>%
    mutate(Time = Time / (24*365))
    
xpGradSurge <-
    xpGrad %>%
    filter(Exp == "4" | Exp == "5" | Exp == "6")

xpGPlot <- ggplot(xpGrad, aes(Time, Performance)) +
    geom_point() +
    geom_hline(yintercept = 65) +
    facet_grid(Exp ~ .)

surge <- tibble(t = c(0, 10, 10, 12, 12, 35), s = c(65, 65, 85, 85, 65, 65))

xpGPlotSurge <- ggplot(xpGradSurge, aes(Time, Performance)) +
    geom_point() +
    facet_grid(Exp ~ .)

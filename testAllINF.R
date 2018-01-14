source("metrics.R")
source("anyLogicDataPull.R")

######################################################################
## Build the data and the plots for the water story
######################################################################
## The output files from the model

nameList <- c("AsIs2Week.csv", "RobustOnly.csv", "TTR.csv",
              "RecLevel.csv")

## The needs for the stakeholder
lowest <- .1
middle <- .5
high <- 1.0


need <- data.frame(func = "constantNeed",
                        cLevel = c(lowest, middle, high),
                        startTime = NA,
                        slope = NA)

## The sigma for the stakeholder
lowS <- 0
highS <- 1

rf <- data.frame(tDelta = 30,
                         decay = 0,
                         sigma = c(lowS))

## Calculate the resilience for all combinations
if ("tResilience" %in% ls()){
} else {
    tResilience <- multScenarioFast(fileNames = nameList,
                                        N = need,
                                        R = rf,
                                        TH = 00) # Note the time can
                                                    # be changed
}

tResilienceClean <- tResilience %>%
    ## filter(nRun == rRun ) %>%
        mutate(Performance = Performance / 100,
               Preference = ifelse(Need == lowest, "lowest",
                   ifelse(Need == middle, "middle",
                          ifelse(Need == high, "high",
                          ifelse(Need == 0.01, "Low", "StatusQuo")))))  
print(colnames(tResilienceClean))

tResilienceMelt <- tResilienceClean %>%
    select(Scenario, QR, EQR, TQR, ETQR,## Rho, extRho,
           statQuoResilience, extResilience, Preference, Infrastructure) %>%
               melt(id.vars = c("Scenario", "Preference", "Infrastructure"))

## Plot of  the resilience values at time 20160
tResPointPlot <- ggplot(tResilienceMelt, aes(Scenario, value, colors = variable)) +
    facet_grid(Preference ~ Infrastructure) +
        geom_point()

## waterPerformance <- cleanAnyLogic(waterNameList) %>%
##     filter(variable == "Water.Functionality" |
##                variable == "Electricity.Availability") %>%
##                    mutate(value = value / 100,
##                           Time = Time / 1440)
## 
## ## Plot of the electric availability inputs and the water outputs
## ## Need to clean u pthe legend and such, but about the 80% solution right
## ## Now
## waterElecPerfPlot <- ggplot(waterPerformance, aes(Time,
##                                    value,
##                                    group = variable,
##                                    linetype = variable)) +
##     geom_line() +
##         facet_grid(variable ~ Scenario) +
##             scale_linetype_manual(values = c("dashed", "solid")) +
##                 theme_bw(base_size = 12, base_family = "serif") +
##                     theme(legend.position = "top",
##                           legend.margin = margin(t = 0, unit = "cm"),
##                           legend.title = element_blank())
## 
## 
## ######################################################################
## ## Working out what is going on with integral resilience
## 
## electricResilienceClean <- waterResilience %>%
##     filter(nRun == rRun ) %>%
##         mutate(Performance = Performance / 100,
##                Preference = ifelse(Need == fireNeed, "FireFighting",
##                    ifelse(Need == generalNeed, "Non-Potable",
##                           "Potable"))) %>%
##                               filter(Infrastructure == "Electricity.Availability")  
## 
## electricResilienceMelt <- electricResilienceClean %>%
##     select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
##            statQuoResilience, extResilience, Preference) %>%
##                melt(id.vars = c("Scenario", "Preference"))
## 
## ## Plot of  the resilience values at time 20160
## electricResPointPlot <- ggplot(electricResilienceMelt, aes(Scenario, value)) +
##     facet_grid(Preference ~ variable) +
##         geom_point()
## 

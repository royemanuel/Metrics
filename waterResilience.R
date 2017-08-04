source("metrics.R")
source("anyLogicDataPull.R")

######################################################################
## Build the data and the plots for the water story
######################################################################
## The output files from the model

waterNameList <- c("AsIs2Week.csv", "RobustOnly.csv", "TTR.csv",
              "RecLevel.csv")

## The needs for the stakeholder
fireNeed <- 0.3
generalNeed <- .9
potableNeed <- 0.7


waterNeed <- data.frame(func = "constantNeed",
                        cLevel = c(fireNeed, generalNeed, potableNeed),
                        startTime = NA,
                        slope = NA)

## The sigma for the stakeholder
fireSigma <- 0.0
generalSigma <- 0.2
potableSigma <- 1

waterSigma <- data.frame(tDelta = 30,
                         decay = 0,
                         sigma = c(fireSigma,generalSigma, potableSigma))

## Calculate the resilience for all combinations
if ("waterResilience5" %in% ls()){
} else {
    waterResilience7 <- multScenarioFast(fileNames = waterNameList,
                                        N = waterNeed,
                                        R = waterSigma,
                                        TH = (20200 -40) / 1440) # Note the time can
                                                    # be changed
}

waterResilienceClean7 <- waterResilience7 %>%
    filter(nRun == rRun ) %>%
                mutate(Preference = ifelse(Need == fireNeed, "FireFighting",
                   ifelse(Need == generalNeed, "Non-Potable",
                          ifelse(Need == potableNeed, "Potable",
                                 ifelse(Need == 0.01, "Low", "StatusQuo"))))) #%>%
                                     filter(waterResilience7, Infrastructure == "Water.Functionality")  
print(colnames(waterResilienceClean))

waterResilienceMelt7 <- waterResilienceClean7 %>%
    select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
           statQuoResilience, extResilience, Preference, Infrastructure) %>%
               melt(id.vars = c("Scenario", "Preference", "Infrastructure"))

waterResilienceMeltonlyinRes <- waterResilienceClean7 %>%
    select(Scenario, statQuoResilience, extResilience, Preference, Infrastructure) %>%
               melt(id.vars = c("Scenario", "Preference", "Infrastructure"))
                       
## Plot of  the resilience values at time 20160
waterResPointPlotintres <- ggplot(waterResilienceMeltonlyinRes, aes(Scenario, value)) +
    facet_grid(Preference ~ Infrastructure) +
        geom_point(aes(color = variable))

waterPerformance <- cleanAnyLogic(waterNameList)# %>%
    filter(variable == "Water.Functionality" |
               variable == "Electricity.Availability") %>%
                   mutate(value = value / 100,
                          Time = Time / 1440)

## Plot of the electric availability inputs and the water outputs
## Need to clean u pthe legend and such, but about the 80% solution right
## Now
waterElecPerfPlot <- ggplot(waterPerformance, aes(Time,
                                   value,
                                   group = variable,
                                   linetype = variable)) +
    geom_line() +
        facet_grid(variable ~ Scenario) +
            #scale_linetype_manual(values = c("dashed", "solid")) +
                theme_bw(base_size = 12, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())


######################################################################
## Working out what is going on with integral resilience

electricResilienceClean <- waterResilience %>%
    filter(nRun == rRun ) %>%
        mutate(Performance = Performance / 100,
               Preference = ifelse(Need == fireNeed, "FireFighting",
                   ifelse(Need == generalNeed, "Non-Potable",
                          "Potable"))) %>%
                              filter(Infrastructure == "Electricity.Availability")  

electricResilienceMelt <- electricResilienceClean %>%
    select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
           statQuoResilience, extResilience, Preference) %>%
               melt(id.vars = c("Scenario", "Preference"))

## Plot of  the resilience values at time 20160
electricResPointPlot <- ggplot(electricResilienceMelt, aes(Scenario, value)) +
    facet_grid(Preference ~ variable) +
        geom_point()


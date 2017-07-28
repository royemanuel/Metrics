source("metrics.R")
source("anyLogicDataPull.R")

######################################################################
## Build the data and the plots for the water story
######################################################################
## The output files from the model

waterNameList <- c("bigAsIs.csv", "big16kRec.csv", "big100percentRec.csv",
              "bigRob.csv", "bigStep.csv")

## The needs for the stakeholder
fireNeed <- 0.2
generalNeed <- 0.5
potableNeed <- 0.8

waterNeed <- data.frame(func = "constantNeed",
                        cLevel = c(fireNeed, generalNeed, potableNeed),
                        startTime = NA,
                        slope = NA)

## The sigma for the stakeholder
fireSigma <- 0
generalSigma <- 0.0
potableSigma <- 0.0

waterSigma <- data.frame(tDelta = 30,
                         decay = 0,
                         sigma = c(fireSigma, generalSigma, potableSigma))

## Calculate the resilience for all combinations
if ("waterResilience" %in% ls()){
} else {
    waterResilience <- multScenarioFast(fileNames = waterNameList,
                                        N = waterNeed,
                                        R = waterSigma,
                                        TH = 39000) # Note the time can
                                                    # be changed
}

waterResilienceClean <- waterResilience %>%
    filter(nRun == rRun ) %>%
        mutate(Performance = Performance / 100,
               Preference = ifelse(Need == fireNeed, "FireFighting",
                   ifelse(Need == generalNeed, "Non-Potable",
                          "Potable"))) %>%
                              filter(Infrastructure == "Water.Functionality")  
print(colnames(waterResilienceClean))

waterResilienceMelt <- waterResilienceClean %>%
    select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
           statQuoResilience, extResilience, Preference) %>%
               melt(id.vars = c("Scenario", "Preference"))

## Plot of  the resilience values at time 39000
waterResPointPlot <- ggplot(waterResilienceMelt, aes(Scenario, value)) +
    facet_grid(Preference ~ variable) +
        geom_point()

waterPerformance <- cleanAnyLogic(waterNameList) %>%
    filter(variable == "Water.Functionality" |
               variable == "Electricity.Availability") %>%
                   mutate(value = value / 100,
                          Time = Time / 1440)

## Plot of the electric availability inputs and the water outputs
waterElecPerfPlot <- ggplot(waterPerformance, aes(Time,
                                   value,
                                   group = variable,
                                   linetype = variable)) +
    geom_line() +
        facet_grid(variable ~ Scenario) +
            scale_linetype_manual(values = c("dashed", "solid")) +
                theme_bw(base_size = 12, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())

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
generalNeed <- .7
potableNeed <- 0.9


waterNeed <- data.frame(func = "constantNeed",
                        cLevel = c(fireNeed, generalNeed, potableNeed),
                        startTime = NA,
                        slope = NA)

## The sigma for the stakeholder
fireSigma <- 0.0
generalSigma <- 0.2
potableSigma <- 0.5

waterSigma <- data.frame(tDelta = 30,
                         decay = 0,
                         sigma = c(fireSigma,generalSigma, potableSigma))

## Calculate the resilience for all combinations
if ("waterResilience7" %in% ls()){
} else {
    waterResilience7 <- multScenarioFast(fileNames = waterNameList,
                                        N = waterNeed,
                                        R = waterSigma,
                                        TH = (20200 -40) / 1440) # Note the time can
                                        # be changed, changed to a days value here    
}

waterResilienceClean7 <- waterResilience7 %>%
                mutate(Preference = ifelse(Need == fireNeed, "Fire Fighting",
                   ifelse(Need == generalNeed, "Non-Potable",
                          ifelse(Need == potableNeed, "Potable",
                                 ifelse(Need == 0.01, "Low", "StatusQuo"))))) %>%
                                     filter(Infrastructure == "Water.Functionality")
waterResFile <- waterResilienceClean7 %>%
    select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
           statQuoResilience, extResilience, Preference, Infrastructure)

write.csv(waterResFile, "waterResFile.csv")


waterResilienceMelt7 <- waterResilienceClean7 %>%
    select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
           statQuoResilience, extResilience, Preference, Infrastructure) %>%
           melt(id.vars = c("Scenario", "Preference", "Infrastructure")) %>%
           mutate(O.or.E = ifelse(tolower(substr(variable, 1, 1)) == "e",
                      "Extended",
                      "Original")) %>%
           mutate(ResType = ifelse((variable == "QR" |
                                    variable == "EQR"),
                                    "Quotient Resilience",
                              ifelse((variable == "Rho" |
                                      variable == "extRho"),
                                                  "Resilience Factor",
                                ifelse((variable == "statQuoResilience" |
                                        variable == "extResilience"),
                                                    "Integral Resilience",
                                  ifelse((variable == "TQR" |
                                          variable == "ETQR"),
                                                      "Total\nQuotient Resilience",
                                         0))))) %>%
                                    mutate(Resilience = value) %>%
                                        select(-value)
waterResilienceMelt7$Scenario <- sub("AsIs2Week.csv", "A", waterResilienceMelt7$Scenario)
waterResilienceMelt7$Scenario <- sub("RobustOnly.csv", "B", waterResilienceMelt7$Scenario)
waterResilienceMelt7$Scenario <- sub("TTR.csv", "C", waterResilienceMelt7$Scenario)
waterResilienceMelt7$Scenario <- sub("RecLevel.csv", "D", waterResilienceMelt7$Scenario)

## Plot all resilience values at time 2160
waterResPointPlot <- ggplot(waterResilienceMelt7, aes(Scenario, Resilience)) +
    facet_grid(Preference ~ ResType) +
        geom_point(aes(shape = factor(O.or.E)), size = 2.5, color = "grey50") +
            geom_point(color = "white", size = 1, aes(shape = O.or.E)) +
                theme_bw(base_size = 12, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())

ggsave(plot = waterResPointPlot,
       filename = paste0("waterResPointPlot.png",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 6.5, height = 6)

## Plot of  the integral resilience values at time 20160
## waterResPointPlotintres <- ggplot(waterResilienceMeltonlyinRes, aes(Scenario, value)) +
##     facet_grid(Preference ~ Infrastructure) +
##         geom_point(aes(color = variable)) 
## 
waterPerformance <- cleanAnyLogic(waterNameList) %>%
    filter(variable == "Water.Functionality" |
               variable == "Electricity.Availability") %>%
                   mutate(Resilience = value) %>%
                       select(-value)
waterPerformance$Scenario <- sub("AsIs2Week.csv", "(A) Current System", waterPerformance$Scenario)
waterPerformance$Scenario <- sub("RobustOnly.csv", "(B) Improved\nRobustness", waterPerformance$Scenario)
waterPerformance$Scenario <- sub("TTR.csv", "(C) Improved Time\nto Recover", waterPerformance$Scenario)
waterPerformance$Scenario <- sub("RecLevel.csv", "(D) Full Recovery", waterPerformance$Scenario)
waterPerformance$variable <- sub("Electricity.Availability",
                                       "Electricity",
                                       waterPerformance$variable)
waterPerformance$variable <- sub("Water.Functionality",
                                       "Water",
                                       waterPerformance$variable)


## Plot of the electric availability inputs and the water outputs
## Need to clean u pthe legend and such, but about the 80% solution right
## Now
waterElecPerfPlot <- ggplot(waterPerformance, aes(Time,
                                   Resilience,
                                   group = variable,
                                   linetype = variable)) +
    geom_line() +
        facet_grid(variable ~ Scenario) +
            #scale_linetype_manual(values = c("dashed", "solid")) +
                theme_bw(base_size = 12, base_family = "serif") +
                    theme(legend.position = "none")

ggsave(plot = waterElecPerfPlot,
       filename = paste0("waterElecPefPlot.png",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 6.5, height = 4)


######################################################################
## Working out what is going on with integral resilience

electricResilienceClean <- waterResilience7 %>%
    mutate(Preference = ifelse(Need == fireNeed, "Fire Fighting",
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

## Build the .csv with the resilience values grouped by infrastructure,
## metric and in order of most resilient to least resilient
wrFactor <- waterResilienceMelt7 %>% group_by(variable, Infrastructure) %>%
    arrange(Preference, variable, -Resilience)
wrFactor$Resilience <- round(wrFactor$Resilience, 3)
## write.csv(wrFactor, "waterRes.csv")

source("metrics.R")
source("anyLogicDataPull.R")

infNameList <-  c("AsIs2Week.csv", "RobustOnly.csv", "TTR.csv",
                  "RecLevel.csv")
nl <- c(.9, .5, .3, .9, .75, .95, .8, .3)
sl <- c(.0, .0, .0, .05, .2, .0, .0, 0)

## build the need data.frame for input into infraResAll
nMat <- data.frame(func = "constantNeed",
                   cLevel = nl,
                   startTime = NA,
                   slope = NA)

## build the resilience factor data.frame for input into infraResAll
rMat <-data.frame(tDelta = 30,
                decay = 0,
                  sigma = sl)

allInfrastructures <- multScenarioFast(fileNames = infNameList,
                                       N = nMat,
                                       R = rMat,
                                       TH = (20200 - 40) / 1440)

infFactor <- allInfrastructures %>%
    filter(Infrastructure == "Electricity.Availability" & nRun == 1 |
               Infrastructure == "Communications.Function" & nRun == 2 |
                   Infrastructure == "IT.Function" & nRun == 3 |
                       Infrastructure == "Healthcare.Function" & nRun == 4 |
                           Infrastructure == "Transportation.Function" & nRun == 5 |
                               Infrastructure == "Critical.Manufacturing.Functionality" & nRun == 6 |
                                   Infrastructure == "Emergency.Services.Functionality" & nRun == 7 |
                                       Infrastructure == "Water.Functionality" & nRun == 8)
infFactor <- infFactor %>%
    select(Scenario, QR, EQR, TQR, ETQR, Rho, extRho,
           statQuoResilience, extResilience, Infrastructure) %>%
           melt(id.vars = c("Scenario", "Infrastructure")) %>%
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
infFactor$Scenario <- sub("AsIs2Week.csv", "A", infFactor$Scenario)
infFactor$Scenario <- sub("RobustOnly.csv", "B", infFactor$Scenario)
infFactor$Scenario <- sub("TTR.csv", "C", infFactor$Scenario)
infFactor$Scenario <- sub("RecLevel.csv", "D", infFactor$Scenario)
infFactor$Infrastructure <- sub("Electricity.Availability",
                                "Electricity",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Communications.Function",
                                "Communications",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("IT.Function",
                                "Information\nTechnology",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Healthcare.Function",
                                "Healthcare",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Transportation.Function",
                                "Transportation",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Critical.Manufacturing.Functionality",
                                "Critical\nManufacturing",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Emergency.Services.Functionality",
                                "Emergency\nServices",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Water.Functionality",
                                "Water",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- as.factor(infFactor$Infrastructure)
infFactor$Infrastructure <- fct_relevel(infFactor$Infrastructure,
                                        "Electricity")
               
infResPointPlot <- ggplot(infFactor, aes(Scenario, Resilience)) +
    facet_grid(Infrastructure ~ ResType) +
        geom_point(aes(shape = factor(O.or.E)), size = 2.5, color = "grey50") +
            geom_point(color = "white", size = 1, aes(shape = O.or.E)) +
                theme_bw(base_size = 11, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())

ggsave(plot = infResPointPlot,
       filename = paste0("infFactResPointPlot.png",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 6.5, height = 9)

infResPointPlotPres <- ggplot(infFactor, aes(Scenario, Resilience)) +
    facet_grid(ResType ~ Infrastructure) +
        geom_point(aes(shape = factor(O.or.E)), size = 2.5, color = "grey50") +
            geom_point(color = "white", size = 1, aes(shape = O.or.E)) +
                theme_bw(base_size = 11, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())

ggsave(plot = infResPointPlotPres,
       filename = paste0("infFactResPointPlotPres",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 9, height = 6)


## Plot of the electric availability inputs and the water outputs
## Need to clean u pthe legend and such, but about the 80% solution right
## Now
infPerformance <- cleanAnyLogic(infNameList) %>%
    mutate(Performance = value) %>% select(-value)
infPerformance$Scenario <- sub("AsIs2Week.csv", "(A) Current System", infPerformance$Scenario)
infPerformance$Scenario <- sub("RobustOnly.csv", "(B) Improved\nRobustness", infPerformance$Scenario)
infPerformance$Scenario <- sub("TTR.csv", "(C) Improved Time\nto Recover", infPerformance$Scenario)
infPerformance$Scenario <- sub("RecLevel.csv", "(D) Full Recovery", infPerformance$Scenario)
infPerformance$variable <- sub("Electricity.Availability",
                                "Electricity",
                                infPerformance$variable)
infPerformance$variable <- sub("Communications.Function",
                                "Communications",
                                infPerformance$variable)
infPerformance$variable <- sub("IT.Function",
                                "Information\nTechnology",
                                infPerformance$variable)
infPerformance$variable <- sub("Healthcare.Function",
                                "Healthcare",
                                infPerformance$variable)
infPerformance$variable <- sub("Transportation.Function",
                                "Transportation",
                                infPerformance$variable)
infPerformance$variable <- sub("Critical.Manufacturing.Functionality",
                                "Critical\nManufacturing",
                                infPerformance$variable)
infPerformance$variable <- sub("Emergency.Services.Functionality",
                                "Emergency\nServices",
                                infPerformance$variable)
infPerformance$variable <- sub("Water.Functionality",
                                "Water",
                                infPerformance$variable)
infPerformance$variable <- as.factor(infPerformance$variable)
infPerformance$variable <- fct_relevel(infPerformance$variable,
                                        "Electricity")

elecPerfPlot <- ggplot(infPerformance, aes(Time,
                                   Performance,
                                   group = variable,
                                   linetype = variable)) +
    geom_line() +
        facet_grid(variable ~ Scenario) +
                theme_bw(base_size = 11, base_family = "serif") +
                    theme(legend.position = "none")

ggsave(plot = elecPerfPlot,
       filename = paste0("ElecPefPlot.png",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 6.5, height = 8.5)

elecPerfPlotPres <- ggplot(infPerformance, aes(Time,
                                   Performance,
                                   group = variable,
                                   linetype = variable)) +
    geom_line() +
        facet_grid(Scenario ~ variable) +
                theme_bw(base_size = 11, base_family = "serif") +
                    theme(legend.position = "none")

ggsave(plot = elecPerfPlotPres,
       filename = paste0("ElecPefPlotPres",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 11, height = 5.5 )


## Build the .csv with the resilience values grouped by infrastructure,
## metric and in order of most resilient to least resilient
arrInfFactor <- infFactor %>% group_by(variable, Infrastructure) %>% arrange(Infrastructure, variable, -Resilience)
arrInfFactor$Resilience <- round(arrInfFactor$Resilience, 3)

source("metrics.R")
source("anyLogicDataPull.R")

infNameList <-  c("AsIs2Week.csv", "RobustOnly.csv", "TTR.csv",
                  "RecLevel.csv")
nl <- c(.9, .5, .3, .9, .75, .95, .8, .9)
sl <- c(.0, .0, .0, .05, .2, .0, .0, .2)

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
                                "Energy",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("Communications.Function",
                                "Communications",
                                infFactor$Infrastructure)
infFactor$Infrastructure <- sub("IT.Function",
                                "IT",
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

               
infResPointPlot <- ggplot(infFactor, aes(Scenario, Resilience)) +
    facet_grid(Infrastructure ~ ResType) +
        geom_point(aes(shape = factor(O.or.E)), size = 2.5, color = "grey50") +
            geom_point(color = "white", size = 1, aes(shape = O.or.E)) +
                theme_bw(base_size = 12, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())

ggsave(plot = infResPointPlot,
       filename = paste0("infFactResPointPlot.png",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 6.5, height = 9)


## Plot of the electric availability inputs and the water outputs
## Need to clean u pthe legend and such, but about the 80% solution right
## Now
infPerformance <- cleanAnyLogic(infNameList)

elecPerfPlot <- ggplot(infPerformance, aes(Time,
                                   value,
                                   group = variable,
                                   linetype = variable)) +
    geom_line() +
        facet_grid(variable ~ Scenario) +
                theme_bw(base_size = 12, base_family = "serif") +
                    theme(legend.position = "top",
                          legend.margin = margin(t = 0, unit = "cm"),
                          legend.title = element_blank())

ggsave(plot = elecPerfPlot,
       filename = paste0("ElecPefPlot.png",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 6.5, height = 8)



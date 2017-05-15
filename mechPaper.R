## Plots for journal paper number one.

## MUST AGGREGATE THE PLOTS FROM THE RAMS PAPER TO REPEAT IT. PUT THAT
## HERE
######################################################################
source("metrics.R")



######################################################################
######################################################################
######################################################################
######################################################################
## These metrics are stolen from RAMS.R and renamed to make more sense
resilienceVersusNeed <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
            select(Time, Need, QR, EQR, Rho,
                   extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time", "Need"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    ## print(colnames(workDF))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    plt <- ggplot(workDF, aes(Need, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
        ## scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 8, base_family = "serif") +
                theme(legend.position = "top",
                      legend.margin = margin(t = 0, unit = "cm"),
                      legend.title = element_blank()) + ylim(0, 1.2)
}

## Plot Substitution (sigma) for each metric
resilienceVersusSigma <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
        select(Time, QR, EQR, Rho,
                   extRho, statQuoResilience, extResilience, Sigma)
    workDF <- melt(data = workDF, id = c("Time", "Sigma"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    ## print(colnames(workDF))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    plt <- ggplot(workDF, aes(Sigma, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable))  +
                                      ## May or may not want to facet
                                      ## this one. Looked pretty good
                                      ## on one plot, but for consistency
                                      ## probably fact it.
                                      facet_grid(ResType ~ .)
    plt <- plt +
        ## scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 8, base_family = "serif") +
                theme(legend.margin=margin(t = 0, unit = 'cm'),
                      legend.position = "top",
                      legend.title = element_blank()) + ylim(0, 1.2)
}
## Plot resilience as the time horizon changes
resilienceVersusTimeHorizon <- function(df){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                   extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Time, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
    ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}
## Plot resilience as the time to fail changes
resilienceVersusTimeToFail <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(FailTime = pRun + 19) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("FailTime"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(FailTime, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
    ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}

## Plot resilience as the time horizon changes
resilienceVersusFailLevel <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(FailLevel = (pRun - 1) / 100) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("FailLevel"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(FailLevel, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
    ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}

## Plot resilience as the recovery level changes
resilienceVersusRecoveryLevel <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(RecoveryLevel = (((pRun - 1) / 100) + 0.1)) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("RecoveryLevel"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(RecoveryLevel, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
    ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}
## Plot resilience as the recovery time changes
resilienceVersusRecoveryTime <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(RecoveryTime = (pRun + 20)) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("RecoveryTime"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" | variable == "EQR"),
                   "Quotient\nResilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "Resilience\nFactor",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral\nResilience", 0))))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(RecoveryTime, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ .)
    plt <- plt +
        ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) +
                      ylim(0, 1.2)
}

## A plot of performance against need to show the general behavior
## of the system
pltPerf <- function(df){
    df <- df %>%
        select(Time, Performance, Need) %>%
                melt(id = "Time")
    plt <- ggplot(data = df, aes(Time, value,
                                 group = variable,
                      linetype = variable)) +
                          geom_line(size=1) +
                              scale_linetype_manual(values=c("solid",
                                                        "dotted"),
                                                    (name = ""))+
        theme_bw(base_size = 8, base_family = "serif") +
            ## scale_linetype_discrete(name = "") +
                theme(legend.position = c(.85, .25)) +
                    labs(y = "Performance") + ylim(0, 1.2)
}

######################################################################
## Build the values used for the plotting                           ##
######################################################################

######################################################################
## Time data.frames
######################################################################
## The detailed time level
t <- data.frame(endTime = 100,
                resolution = .1)

## The rough scale time level to prove things out
timeRough <- data.frame(endTime = 100,
                        resolution = 5)
######################################################################
## Need data.frames
######################################################################
## Constant need at 0.9
needConstant <- data.frame(func = "constantNeed",
                cLevel = 0.8,
                startTime = NA,
                 slope = NA)
nLinearVary <- data.frame(func = "constantNeed",
                          cLevel = seq(from = 0.01,
                                       to = 1.0,
                                       by = .01),
                          cLevel = 1.0,
                          startTime = NA,
                          slope = NA)

## Motivating example of need where there is a bump between times
needBump <- data.frame(func = "stepNeed",
                       startLevel = .9,
                       step1Time = 200,
                       step1Level = 1.2,
                       step2Time = 600,
                       step2Level = .9,
                       endTime = 1001)
######################################################################
## Performance data.frames
######################################################################

## Steady Performance without a failure
noFailure <- data.frame(func = "step",
                        failTime = 20,
                        recTime = 60,
                        preLevel = 1.0,
                        failLevel = 1.0,
                        recLevel = 1.0)


## Stepped recovery performance
steppedRecovery <- data.frame(func = "step",
                failTime = 20,
                recTime = 60,
                preLevel = 1.0,
                failLevel = 0.1,
                recLevel = 0.9)

## Stepped recovery where the robustness or fail level varies from
## 0 to 1

steppedRecoveryVaryFailLevel <- data.frame(func = "step",
                                           failTime = 20,
                                           recTime = 60,
                                           preLevel = 1.0,
                                           failLevel = seq(from = 0,
                                               to = 1.0,
                                               by = .01),
                                           recLevel = 1.0)

steppedRecoveryVaryRecoveryLevel <- data.frame(func = "step",
                                           failTime = 20,
                                           recTime = 60,
                                           preLevel = 1.0,
                                           failLevel = 0.1,
                                           recLevel =  seq(from = 0.1,
                                               to = 1.2,
                                               by = .01))

## Linear recovery - resilience triangle
linearRecovery <- data.frame(func = "resTriangle",
                             failTime = 20,
                             recTime = 60,
                             preLevel = 1.0,
                             failLevel = 0.1,
                             recLevel = 0.9)

## No Recovery and the time to fail changes from 20 to 59
varyTimeToFail <- data.frame(func = "step",
                             failTime = c(20:59),
                             recTime = 60,
                             preLevel = 1.0,
                             failLevel = 0.1,
                             recLevel = 0.1)

## No Recovery and the time to recover changes from 21 to 60
varyTimeToRecover <- data.frame(func = "step",
                             failTime = 20,
                             recTime = c(21:60),
                             preLevel = 1.0,
                             failLevel = 0.1,
                             recLevel = 0.9)

noRecovery <- data.frame(func = "step",
                         failTime = 20,
                         recTime = 60,
                         preLevel = 1.0,
                         failLevel = 0.1,
                         recLevel = 0.1)


######################################################################
## Resilience Paramater data.frames
######################################################################

r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)

## steps through sigma values from 0 to 1
rSigmaVary <- data.frame(tDelta = 30,
                decay = 0,
                 sigma = seq(from = 0,
                     to = 1.0,
                     by = .01))


######################################################################
## Build the plots using a stepped recovery                         ##
######################################################################

## The time horizon plot for the stepped recovery. First build the
## resilience matrix
steppedRecoveryTimeHorizonData <- resLoop(t,
                                          needConstant,
                                          steppedRecovery,
                                          r)

## Then plot it.
plotSteppedRecoveryTimeHorizon <- resilienceVersusTimeHorizon(steppedRecoveryTimeHorizonData)
ggsave(plot = plotSteppedRecoveryTimeHorizon,
       filename = paste0("plotSteppedRecoveryTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## Plot the general performance vs co nstant need
steppedRecoveryPerformance <- pltPerf(steppedRecoveryTimeHorizonData)
ggsave(plot = steppedRecoveryPerformance,
       filename = paste0("steppedRecoveryPerformance",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## The plot of resilience versus changing need level. First build the
## resilience matrix

need0to1SteppedRecoveryData <- resLoop(t,
                                       nLinearVary,
                                       steppedRecovery,
                                       r)

## Then plot it at time = 80 or after the recovery
plotNeed0to1SteppedRecovery <- resilienceVersusNeed(need0to1SteppedRecoveryData, 80)
ggsave(plot = plotNeed0to1SteppedRecovery,
       filename = paste0("plotNeed0to1SteppedRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## The plot of changing sigma from 0 to 1 on stepped recovery. First build
## the resilience matrix

sigma0to1SteppedRecoveryData <- resLoop(t,
                                        needConstant,
                                        steppedRecovery,
                                        rSigmaVary)

## Then plot it at time = 80 or after the recovery
plotSigma0to1SteppedRecovery <- resilienceVersusSigma(sigma0to1SteppedRecoveryData,
                                                      80)
ggsave(plot = plotSigma0to1SteppedRecovery,
       filename = paste0("plotSigma0to1SteppedRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## Build a stepped recovery where the fail level varies from 0 to 1
failLevel0to1Data <- resLoop(t,
                             needConstant,
                             steppedRecoveryVaryFailLevel,
                             r)

## Then plot it at time 80

plotFailLevel0to1 <- resilienceVersusFailLevel(failLevel0to1Data, 80)
ggsave(plot = plotFailLevel0to1,
       filename = paste0("plotFailLevel0to1",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## Build a stepped recovery where the recovery level varies from 0.1 to 1.2
recoveryLevel0to1Data <- resLoop(t,
                             needConstant,
                             steppedRecoveryVaryRecoveryLevel,
                             r)

## Then plot it at time 80

plotRecoveryLevel0to1 <- resilienceVersusRecoveryLevel(recoveryLevel0to1Data, 80)
ggsave(plot = plotRecoveryLevel0to1,
       filename = paste0("plotRecoveryLevel0to1",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## Build a stepped recovery where the recovery time varies from 21 to 60
recoveryTime21to60Data <- resLoop(t,
                             needConstant,
                             varyTimeToRecover,
                             r)

## Then plot it at time 80

plotRecoveryTime21to60 <- resilienceVersusRecoveryTime(recoveryTime21to60Data, 80)
ggsave(plot = plotRecoveryTime21to60,
       filename = paste0("plotRecoveryTime21to60",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)


######################################################################
## Build the plots using a linear recovery                         ##
######################################################################

## The time horizon plot for the stepped recovery. First build the
## resilience matrix
linearRecoveryTimeHorizonData <- resLoop(t,
                                          needConstant,
                                          linearRecovery,
                                          r)

## Then plot it.
plotLinearRecoveryTimeHorizon <- resilienceVersusTimeHorizon(linearRecoveryTimeHorizonData)

ggsave(plot = plotLinearRecoveryTimeHorizon,
       filename = paste0("plotLinearRecoveryTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## Plot the general performance vs constant need
linearRecoveryPerformance <- pltPerf(linearRecoveryTimeHorizonData)
ggsave(plot = linearRecoveryPerformance,
       filename = paste0("linearRecoveryPerformance",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## The plot of resilience versus changing need level. First build the
## resilience matrix

need0to1LinearRecoveryData <- resLoop(t,
                                       nLinearVary,
                                       linearRecovery,
                                       r)

## Then plot it at time = 80 or after the recovery
plotNeed0to1LinearRecovery <- resilienceVersusNeed(need0to1LinearRecoveryData, 80)
ggsave(plot = plotNeed0to1LinearRecovery,
       filename = paste0("plotNeed0to1LinearRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## The plot of changing sigma from 0 to 1 on stepped recovery. First build
## the resilience matrix

sigma0to1LinearRecoveryData <- resLoop(t,
                                        needConstant,
                                        linearRecovery,
                                        rSigmaVary)

## Then plot it at time = 80 or after the recovery
plotSigma0to1LinearRecovery <- resilienceVersusSigma(sigma0to1LinearRecoveryData,
                                                     80)
ggsave(plot = plotSigma0to1LinearRecovery,
       filename = paste0("plotSigma0to1LinearRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

######################################################################
## Build the plots using no recovery                                ##
######################################################################

## This one is different from the previous. First we need to plot
## Resilience against failtime varying from 20 to 59. To make my code
## work, I cheated and made recovery tiny-tiny-tiny. This is close enough

noRecoveryTimeToFailData <- resLoop(t,
                                    needConstant,
                                    varyTimeToFail,
                                    r)

## Then plot it.
plotnoRecoveryTimeToFail <- resilienceVersusTimeToFail(noRecoveryTimeToFailData, 80)
ggsave(plot = plotnoRecoveryTimeToFail,
       filename = paste0("plotnoRecoveryTimeToFail",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
## Plot the general performance vs constant need
noRecoveryFailTime20 <- filter(noRecoveryTimeToFailData, pRun == 1)
noRecoveryPerformance <- pltPerf(noRecoveryFailTime20)
ggsave(plot = noRecoveryPerformance,
       filename = paste0("noRecoveryPerformance",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## Build the data Time horizon data with a failure at 20 and no recovery
noRecoveryTimeHorizonData <- resLoop(t,
                                     needConstant,
                                     noRecovery,
                                     r)
## Then plot it
plotNoRecoveryTimeHorizon <- resilienceVersusTimeHorizon(noRecoveryTimeHorizonData)
ggsave(plot = plotNoRecoveryTimeHorizon,
       filename = paste0("plotNoRecoveryTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
## The plot of resilience versus changing need level. First build the
## resilience matrix

need0to1NoRecoveryData <- resLoop(t,
                                  nLinearVary,
                                  noRecovery,
                                  r)

## Then plot it at time = 80 or after the recovery
plotNeed0to1NoRecovery <- resilienceVersusNeed(need0to1NoRecoveryData, 80)
ggsave(plot = plotNeed0to1NoRecovery,
       filename = paste0("plotNeed0to1NoRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
## The plot of changing sigma from 0 to 1 on stepped recovery. First build
## the resilience matrix

sigma0to1NoRecoveryData <- resLoop(t,
                                   needConstant,
                                   noRecovery,
                                   rSigmaVary)

## Then plot it at time = 80 or after the recovery
plotSigma0to1NoRecovery <- resilienceVersusSigma(sigma0to1NoRecoveryData,
                                                     80)
ggsave(plot = plotSigma0to1NoRecovery,
       filename = paste0("plotSigma0to1NoRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)

## The motivating example where there is no failure, but the demand
## changes. This results in a shortfall
## First, the time horizon plot
noFailureTimeHorizonData <- resLoop(t, needBump, noFailure, r)
## Then plot it
performanceNoFailureNeedBump <- pltPerf(noFailureTimeHorizonData)
ggsave(plot = performanceNoFailureNeedBump,
       filename = paste0("performanceNoFailureNeedBump",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
plotNoFailureTimeHorizon <- resilienceVersusTimeHorizon(noFailureTimeHorizonData)
ggsave(plot = plotNoFailureTimeHorizon,
       filename = paste0("plotNoFailureTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
## Now the sigma plot
noFailureSigma0to1Data <- resLoop(t, needBump, noFailure, rSigmaVary)
## Then plot it
plotNoFailureSigma0to1 <- resilienceVersusSigma(noFailureSigma0to1Data, 80)
ggsave(plot = plotNoFailureSigma0to1,
       filename = paste0("plotNoFailureSigma0to1",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
######################################################################
######################################################################
## Bank of ggsaves that can be commented out after being saved      ##
######################################################################
######################################################################

######################################################################\
## Calculate values for use in the paper

compareNoRecovery <- noRecoveryTimeHorizonData %>%
    filter(Time == 100) %>%
        select(Time, Rho, extRho, statQuoResilience, extResilience)
ESDFcompNoRecovery <- compareNoRecovery$extRho - compareNoRecovery$Rho
intRescompNoRecovery <- compareNoRecovery$extResilience -
    compareNoRecovery$statQuoResilience

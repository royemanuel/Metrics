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
            select(Time, Need, QR, EQR, Rho, TQR, ETQR,
                   extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time", "Need"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                 "Total \nQuotient \nResilience",
                                 0)))))
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
            theme_bw(base_size = 12, base_family = "serif") +
                theme(legend.position = "top",
                      legend.margin = margin(t = 0, unit = "cm"),
                      legend.title = element_blank()) + ylim(0, 1.2)
}

## Plot Substitution (sigma) for each metric
resilienceVersusSigma <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
        select(Time, QR, EQR, Rho, TQR, ETQR,
                   extRho, statQuoResilience, extResilience, Sigma)
    workDF <- melt(data = workDF, id = c("Time", "Sigma"))
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                 "Total \nQuotient \nResilience",
                                 0)))))
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
            theme_bw(base_size = 12, base_family = "serif") +
                theme(legend.margin=margin(t = 0, unit = 'cm'),
                      legend.position = "top",
                      legend.title = element_blank()) + ylim(0, 1.2)
}
## Plot resilience as the time horizon changes
resilienceVersusTimeHorizon <- function(df, title){
    workDF <- df %>%
         select(Time, QR, EQR, Rho, TQR, ETQR,
                   extRho, statQuoResilience, extResilience)
    workDF <- melt(data = workDF, id = c("Time"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up ,but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                "Total \nQuotient \nResilience",
                                0)))))
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
        theme_bw(base_size = 12, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}
## Plot resilience as the time to fail changes
resilienceVersusTimeToFail <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho, TQR, ETQR,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(FailTime = pRun + 19) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("FailTime"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                 "Total \nQuotient \nResilience",
                                 0)))))
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
    plt <- plt + labs(x = "Fail Time") +
    ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 12, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}

## Build the resilience type

## Plot resilience as the time horizon changes
resilienceVersusFailLevel <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho, TQR, ETQR,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(FailLevel = (pRun - 1) / 100) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("FailLevel"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                 "Total \nQuotient \nResilience",
                                 0)))))
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
    plt <- plt + labs(x = "Robustness") + 
    ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 12, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}

## Plot resilience as the recovery level changes
resilienceVersusRecoveryLevel <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho, TQR, ETQR,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(RecoveryLevel = (((pRun - 1) / 100) + 0.1)) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("RecoveryLevel"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                 "Total \nQuotient \nResilience",
                                 0)))))
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
        theme_bw(base_size = 12, base_family = "serif") +
            theme(legend.position = "top",
                  legend.title = element_blank()) + ylim(0, 1.2)
}
## Plot resilience as the recovery time changes
resilienceVersusRecoveryTime <- function(df, time){
    workDF <- df %>%
         select(Time, QR, EQR, Rho, TQR, ETQR,
                extRho, statQuoResilience, extResilience, pRun) %>%
                    mutate(RecoveryTime = (pRun + 20)) %>%
                        filter(Time == time) %>%
                        select(-pRun, -Time)
    workDF <- melt(data = workDF, id = c("RecoveryTime"))
    ## Assign a value to the pairings of extended and unextended values
    ## there might be a better way to do this that you might want to
    ## clear up, but for now, get it on the paper
    workDF <- workDF %>%
        mutate(ResType = ifelse((variable == "QR" |
                                 variable == "EQR"),
                                "Quotient\nResilience",
                         ifelse((variable == "Rho" |
                                 variable == "extRho"),
                                "Resilience\nFactor",
                         ifelse((variable == "statQuoResilience" |
                                 variable == "extResilience"),
                                "Integral\nResilience",
                         ifelse((variable == "TQR" |
                                 variable == "ETQR"),
                                 "Total \nQuotient \nResilience",
                                 0)))))
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
    plt <- plt + labs(x = "Recovery Time") +
        ## scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 12, base_family = "serif") +
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
                                                        "dashed"),
                                                    (name = ""))+
        theme_bw(base_size = 12, base_family = "serif") +
            ## scale_linetype_discrete(name = "") +
                theme(legend.position = "top") +
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
print("sRTHD")
steppedRecoveryTimeHorizonData <- resLoop(t,
                                          needConstant,
                                          steppedRecovery,
                                          r)

## Then plot it.
plotSteppedRecoveryTimeHorizon <- resilienceVersusTimeHorizon(steppedRecoveryTimeHorizonData)

## Plot the general performance vs co nstant need
steppedRecoveryPerformance <- pltPerf(steppedRecoveryTimeHorizonData)

## The plot of resilience versus changing need level. First build the
## resilience matrix

print("n0t1")
need0to1SteppedRecoveryData <- resLoop(t,
                                       nLinearVary,
                                       steppedRecovery,
                                       r)

## Then plot it at time = 80 or after the recovery
plotNeed0to1SteppedRecovery <- resilienceVersusNeed(need0to1SteppedRecoveryData, 80)

## The plot of changing sigma from 0 to 1 on stepped recovery. First build
## the resilience matrix
print("s0t1")
sigma0to1SteppedRecoveryData <- resLoop(t,
                                        needConstant,
                                        steppedRecovery,
                                        rSigmaVary)

## Then plot it at time = 80 or after the recovery
plotSigma0to1SteppedRecovery <- resilienceVersusSigma(sigma0to1SteppedRecoveryData,
                                                      80)
## Build a stepped recovery where the fail level varies from 0 to 1
print("fl0t1")
failLevel0to1Data <- resLoop(t,
                             needConstant,
                             steppedRecoveryVaryFailLevel,
                             r)

## Then plot it at time 80

plotFailLevel0to1 <- resilienceVersusFailLevel(failLevel0to1Data, 80)
## Build a stepped recovery where the recovery level varies from 0.1 to 1.2
print("rl0t1")
recoveryLevel0to1Data <- resLoop(t,
                             needConstant,
                             steppedRecoveryVaryRecoveryLevel,
                             r)

## Then plot it at time 80

plotRecoveryLevel0to1 <- resilienceVersusRecoveryLevel(recoveryLevel0to1Data, 80)

## Build a stepped recovery where the recovery time varies from 21 to 60
recoveryTime21to60Data <- resLoop(t,
                             needConstant,
                             varyTimeToRecover,
                             r)

## Then plot it at time 80

plotRecoveryTime21to60 <- resilienceVersusRecoveryTime(recoveryTime21to60Data, 80)

######################################################################
## Build the plots using a linear recovery                         ##
######################################################################

## The time horizon plot for the stepped recovery. First build the
## resilience matrix
print("lRTH")
linearRecoveryTimeHorizonData <- resLoop(t,
                                          needConstant,
                                          linearRecovery,
                                          r)

## Then plot it.
plotLinearRecoveryTimeHorizon <- resilienceVersusTimeHorizon(linearRecoveryTimeHorizonData)

## Plot the general performance vs constant need
linearRecoveryPerformance <- pltPerf(linearRecoveryTimeHorizonData)

## The plot of resilience versus changing need level. First build the
## resilience matrix
print("n0t1LR")
need0to1LinearRecoveryData <- resLoop(t,
                                       nLinearVary,
                                       linearRecovery,
                                       r)

## Then plot it at time = 80 or after the recovery
plotNeed0to1LinearRecovery <- resilienceVersusNeed(need0to1LinearRecoveryData, 80)

## The plot of changing sigma from 0 to 1 on stepped recovery. First build
## the resilience matrix
print("s0t1LR")
sigma0to1LinearRecoveryData <- resLoop(t,
                                        needConstant,
                                        linearRecovery,
                                        rSigmaVary)

## Then plot it at time = 80 or after the recovery
plotSigma0to1LinearRecovery <- resilienceVersusSigma(sigma0to1LinearRecoveryData,
                                                     80)
######################################################################
## Build the plots using no recovery                                ##
######################################################################

## This one is different from the previous. First we need to plot
## Resilience against failtime varying from 20 to 59. To make my code
## work, I cheated and made recovery tiny-tiny-tiny. This is close enough
print("nRTF")
noRecoveryTimeToFailData <- resLoop(t,
                                    needConstant,
                                    varyTimeToFail,
                                    r)

## Then plot it.
plotnoRecoveryTimeToFail <- resilienceVersusTimeToFail(noRecoveryTimeToFailData, 80)

## Plot the general performance vs constant need
noRecoveryFailTime20 <- filter(noRecoveryTimeToFailData, pRun == 1)
noRecoveryPerformance <- pltPerf(noRecoveryFailTime20)

## Build the data Time horizon data with a failure at 20 and no recovery
print("nRTH")
noRecoveryTimeHorizonData <- resLoop(t,
                                     needConstant,
                                     noRecovery,
                                     r)
## Then plot it
plotNoRecoveryTimeHorizon <- resilienceVersusTimeHorizon(noRecoveryTimeHorizonData)

## The plot of resilience versus changing need level. First build the
## resilience matrix
print("n0t1NR")
need0to1NoRecoveryData <- resLoop(t,
                                  nLinearVary,
                                  noRecovery,
                                  r)

## Then plot it at time = 80 or after the recovery
plotNeed0to1NoRecovery <- resilienceVersusNeed(need0to1NoRecoveryData, 80)

## The plot of changing sigma from 0 to 1 on stepped recovery. First build
## the resilience matrix
print("sig0t1NR")
sigma0to1NoRecoveryData <- resLoop(t,
                                   needConstant,
                                   noRecovery,
                                   rSigmaVary)

## Then plot it at time = 80 or after the recovery
plotSigma0to1NoRecovery <- resilienceVersusSigma(sigma0to1NoRecoveryData,
                                                     80)
## The motivating example where there is no failure, but the demand
## changes. This results in a shortfall
## First, the time horizon plot
print("nFTH")
noFailureTimeHorizonData <- resLoop(t, needBump, noFailure, r)
## Then plot it
plotNoFailureTimeHorizon <- resilienceVersusTimeHorizon(noFailureTimeHorizonData)

performanceNoFailureNeedBump <- pltPerf(noFailureTimeHorizonData)

## Now the sigma plot
print("nFs0t1")
noFailureSigma0to1Data <- resLoop(t, needBump, noFailure, rSigmaVary)
## Then plot it
plotNoFailureSigma0to1 <- resilienceVersusSigma(noFailureSigma0to1Data, 80)

######################################################################
## Build a plot for all the time horizon Data so I don't have to
## Photoshop every one
######################################################################

noFailureTimeHorizonData <- mutate(noFailureTimeHorizonData,
                                   dataSet = "(d) No Failure with Changing Need")
noRecoveryTimeHorizonData <- mutate(noRecoveryTimeHorizonData,
                                    dataSet = "(a) Step Failure without Recovery")
linearRecoveryTimeHorizonData <- mutate(linearRecoveryTimeHorizonData,
                                        dataSet = "(c) Step Failure with Linear Recovery")
steppedRecoveryTimeHorizonData <- mutate(steppedRecoveryTimeHorizonData,
                                         dataSet = "(b) Step Failure with Step Recovery")

allTimeHor <- bind_rows(noFailureTimeHorizonData,
                        noRecoveryTimeHorizonData,
                        linearRecoveryTimeHorizonData,
                        steppedRecoveryTimeHorizonData)
allTimeHor <- allTimeHor %>%
    select(Time, QR, EQR, Rho, TQR, ETQR, extRho, statQuoResilience,
           extResilience, dataSet)
allTimeHor <- melt(data = allTimeHor, id = c("Time", "dataSet"))
allTimeHor <- allTimeHor %>%
    mutate(ResType = ifelse((variable == "QR" |
                             variable == "EQR"),
                             "Quotient\nResilience",
                     ifelse((variable == "Rho" |
                             variable == "extRho"),
                             "Resilience\nFactor",
                     ifelse((variable == "statQuoResilience" |
                             variable == "extResilience"),
                             "Integral\nResilience",
                     ifelse((variable == "TQR" |
                             variable == "ETQR"),
                             "Total Quotient \nResilience",
                            0))))) %>%
    mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
               "Extended",
               "Original")) %>%
                   mutate(Resilience = value) %>%
    select(-value)

thPlot <- ggplot(allTimeHor, aes(Time, Resilience, group = variable)) +
    geom_line(aes(linetype = variable)) +
        facet_grid(ResType ~ dataSet) +
        ## scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 11, base_family = "serif") +
                theme(legend.margin=margin(t = 0, unit = 'cm'),
                      legend.position = "top",
                      legend.title = element_blank()) + ylim(0, 1.2)
ggsave(plot = thPlot,
       filename = paste0("TimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 9,
       height = 5.2)


######################################################################
## Grouped plots for Need


need0to1NoRecoveryData <- mutate(need0to1NoRecoveryData,
                                    dataSet = "(a) Step Failure without Recovery")
need0to1LinearRecoveryData <- mutate(need0to1LinearRecoveryData,
                                        dataSet = "(c) Step Failure with Linear Recovery")
need0to1SteppedRecoveryData <- mutate(need0to1SteppedRecoveryData,
                                         dataSet = "(b) Step Failure with Step Recovery")
allNeed <- bind_rows(need0to1LinearRecoveryData,
                     need0to1NoRecoveryData,
                     need0to1SteppedRecoveryData)
allNeed <- allNeed %>%
    select(Time, QR, EQR, Rho, TQR, ETQR, extRho, statQuoResilience,
           extResilience, dataSet, Need)
allNeed <- melt(data = allNeed, id = c("Time", "Need", "dataSet"))
allNeed <- allNeed %>%
    ## Note that the time is hard coded to 80 here
    filter(Time == 80) %>%
    mutate(ResType = ifelse((variable == "QR" |
                             variable == "EQR"),
                             "Quotient\nResilience",
                     ifelse((variable == "Rho" |
                             variable == "extRho"),
                             "Resilience\nFactor",
                     ifelse((variable == "statQuoResilience" |
                             variable == "extResilience"),
                             "Integral\nResilience",
                     ifelse((variable == "TQR" |
                             variable == "ETQR"),
                             "Total Quotient \nResilience",
                            0))))) %>%
    mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
               "Extended",
               "Original")) %>%
                   mutate(Resilience = value) %>%
    select(-value)

needPlot <- ggplot(allNeed, aes(Need, Resilience, group = variable)) +
    geom_line(aes(linetype = variable)) +
        facet_grid(ResType ~ dataSet) +
        ## scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 11, base_family = "serif") +
                theme(legend.margin=margin(t = 0, unit = 'cm'),
                      legend.position = "top",
                      legend.title = element_blank()) + ylim(0, 1.2)
ggsave(plot = needPlot,
       filename = paste0("Need",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 9,
       height = 5.2)
######################################################################
## Grouped plots for Sigma


sigma0to1NoRecoveryData <- mutate(sigma0to1NoRecoveryData,
                                    dataSet = "(a) Step Failure without Recovery")
sigma0to1LinearRecoveryData <- mutate(sigma0to1LinearRecoveryData,
                                        dataSet = "(c) Step Failure with Linear Recovery")
sigma0to1SteppedRecoveryData <- mutate(sigma0to1SteppedRecoveryData,
                                       dataSet = "(b) Step Failure with Step Recovery")
sigma0to1NoFailureData <- mutate(noFailureSigma0to1Data,
                                 dataSet = "(d) No Failure with Changing Need")
allSigma <- bind_rows(sigma0to1LinearRecoveryData,
                     sigma0to1NoRecoveryData,
                      sigma0to1SteppedRecoveryData,
                      sigma0to1NoFailureData)
allSigma <- allSigma %>%
    select(Time, QR, EQR, Rho, TQR, ETQR, extRho, statQuoResilience,
           extResilience, dataSet, Sigma)
allSigma <- melt(data = allSigma, id = c("Time", "Sigma", "dataSet"))
allSigma <- allSigma %>%
    ## Note that the time is hard coded to 80 here
    filter(Time == 80) %>%
    mutate(ResType = ifelse((variable == "QR" |
                             variable == "EQR"),
                             "Quotient\nResilience",
                     ifelse((variable == "Rho" |
                             variable == "extRho"),
                             "Resilience\nFactor",
                     ifelse((variable == "statQuoResilience" |
                             variable == "extResilience"),
                             "Integral\nResilience",
                     ifelse((variable == "TQR" |
                             variable == "ETQR"),
                             "Total Quotient \nResilience",
                            0))))) %>%
    mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
               "Extended",
               "Original")) %>%
                   mutate(Resilience = value) %>%
    select(-value)

sigmaPlot <- ggplot(allSigma, aes(Sigma, Resilience, group = variable)) +
    geom_line(aes(linetype = variable)) +
        facet_grid(ResType ~ dataSet) +
        ## scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 11, base_family = "serif") +
                theme(legend.margin=margin(t = 0, unit = 'cm'),
                      legend.position = "top",
                      legend.title = element_blank()) + ylim(0, 1.2)
ggsave(plot = sigmaPlot,
       filename = paste0("Sigma",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 9,
       height = 5.2)

######################################################################
## ggsave Bank
ggsave(plot = plotSteppedRecoveryTimeHorizon,
       filename = paste0("plotSteppedRecoveryTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = steppedRecoveryPerformance,
       filename = paste0("ShortsteppedRecoveryPerformance",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 2)
ggsave(plot = plotNeed0to1SteppedRecovery,
       filename = paste0("plotNeed0to1SteppedRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5.0)
ggsave(plot = plotSigma0to1SteppedRecovery,
       filename = paste0("plotSigma0to1SteppedRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotFailLevel0to1,
       filename = paste0("plotFailLevel0to1",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotRecoveryLevel0to1,
       filename = paste0("plotRecoveryLevel0to1",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotRecoveryTime21to60,
       filename = paste0("plotRecoveryTime21to60",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotLinearRecoveryTimeHorizon,
       filename = paste0("plotLinearRecoveryTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = linearRecoveryPerformance,
       filename = paste0("ShortlinearRecoveryPerformance",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 2)
ggsave(plot = plotNeed0to1LinearRecovery,
       filename = paste0("plotNeed0to1LinearRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5.0)
ggsave(plot = plotSigma0to1LinearRecovery,
       filename = paste0("plotSigma0to1LinearRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotnoRecoveryTimeToFail,
       filename = paste0("plotnoRecoveryTimeToFail",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = noRecoveryPerformance,
       filename = paste0("ShortnoRecoveryPerformance",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 3)
ggsave(plot = plotNoRecoveryTimeHorizon,
       filename = paste0("plotNoRecoveryTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotNeed0to1NoRecovery,
       filename = paste0("plotNeed0to1NoRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotSigma0to1NoRecovery,
       filename = paste0("plotSigma0to1NoRecovery",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
plotNoFailureTimeHorizon <- resilienceVersusTimeHorizon(noFailureTimeHorizonData)
performanceNoFailureNeedBump <- pltPerf(noFailureTimeHorizonData)
ggsave(plot = performanceNoFailureNeedBump,
       filename = paste0("ShortperformanceNoFailureNeedBump",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 2)
ggsave(plot = plotNoFailureTimeHorizon,
       filename = paste0("plotNoFailureTimeHorizon",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)
ggsave(plot = plotNoFailureSigma0to1,
       filename = paste0("plotNoFailureSigma0to1",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 3.5,
       height = 5)

######################################################################
######################################################################
## Bank of ggsaves that can be commented out after being saved      ##
######################################################################
######################################################################

## New Group of Plots. Put all the performance plots into one 7x4 plot
SR <- steppedRecoveryTimeHorizonData %>%
    mutate(Profile = "B")
NF <- noFailureTimeHorizonData %>%
    mutate(Profile = "D")
NR <- noRecoveryTimeHorizonData %>%
    mutate(Profile = "A")
LR <- linearRecoveryTimeHorizonData %>%
    mutate(Profile = "C")

allTH <- bind_rows(SR, NF, NR, LR)
allTH <- allTH %>%
    select(Time, Need, Performance, Profile) %>%
        melt(id.vars = c("Time", "Profile")) %>%
            mutate(Performance = value) %>%
                select(-value)

## Build a labeller to make the plot proper
scenarioNames <- list(
    'A' = 'Step Failure without Recovery',
    'B' = 'Step Failure with Step Recovery',
    'C' = 'Step Failure with Linear Recovery',
    'D' = 'No Failure with Changing Need'
)

scenLabeller <- function(variable, value){
    return(scenarioNames[value])
}

performancePlots <- ggplot(allTH, aes(Time,
                                      Performance,
                                      group = variable,
                                      linetype = variable))
performancePlots <- performancePlots +
    geom_line() +
        facet_wrap(~ Profile,
                   nrow = 2,
                   ncol = 2,
                   labeller = scenLabeller) +
                       theme_bw(base_size = 12, base_family = "serif") +
                           theme(legend.position = c(.85, .15),
                                 legend.title = element_blank()) +
                                     ylim(0, 1.2)
performancePlots <- performancePlots +
    scale_linetype_manual(values=c("dashed",
                              "solid"),
                          (name = ""))

ggsave(plot = performancePlots,
       filename = paste0("performancePlots",
           format(Sys.time(), "%Y-%m-%d-%I-%M"),
           ".png"),
       width = 7,
       height = 5)


######################################################################\
## Calculate values for use in the paper

compareNoRecovery <- noRecoveryTimeHorizonData %>%
    filter(Time == 100) %>%
        select(Time, Rho, extRho, statQuoResilience, extResilience)
ESDFcompNoRecovery <- compareNoRecovery$extRho - compareNoRecovery$Rho
intRescompNoRecovery <- compareNoRecovery$extResilience -
    compareNoRecovery$statQuoResilience

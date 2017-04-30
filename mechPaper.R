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
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
    ## print(colnames(workDF))
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    plt <- ggplot(workDF, aes(Need, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(. ~ ResType)
    plt <- plt +
        scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 20, base_family = "serif") +
                theme(legend.position = c(.95, .15))
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
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
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
                                      facet_grid(. ~ ResType)
    plt <- plt +
        scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 20, base_family = "serif") +
                theme(legend.position = c(.95, .15))
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
                   "Quotient Resilience",
                   ifelse((variable == "Rho" | variable == "extRho"),
                          "ESDF",
                          ifelse((variable == "statQuoResilience" |
                                      variable == "extResilience"),
                                 "Integral Resilience", 0))))
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
    scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 20, base_family = "serif") +
            theme(legend.position = c(.95, .15))
}

######################################################################
## Build the values used for the plotting                           ##
######################################################################

## The detailed time level
t <- data.frame(endTime = 100,
                resolution = .1)

## The rough scale time level to prove things out
timeRough <- data.frame(endTime = 100,
                        resolution = 5)
## Constant need at 0.9
needConstant <- data.frame(func = "constantNeed",
                cLevel = 0.9,
                startTime = NA,
                 slope = NA)

## Stepped recovery performance
steppedRecovery <- data.frame(func = "step",
                failTime = 20,
                recTime = 60,
                preLevel = 1.2,
                failLevel = 0.1,
                recLevel = 1.0)

## Linear recovery - resilience triangle
linearRecovery <- data.frame(func = "resTriangle",
                             failTime = 20,
                             recTime = 60,
                             preLevel = 1.2,
                             failLevel = 0.1,
                             recLevel = 1.0)

r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)

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


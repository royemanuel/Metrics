## Specific plotting functions for the RAMS presentation

######################################################################
######################################################################
## PLOTTING FUNCTIONS
######################################################################
######################################################################
## These are split up for individual plots of the different types of
## resilience metrics

## Plot Need for each metric

## 
RAMSpltMoveNeed <- function(df, time, restype){
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
    ## 
    workDF <- workDF %>%
        mutate(variable = ifelse(tolower(substr(variable, 1, 1)) == "e",
                   "Extended",
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    print(colnames(workDF))
    plt <- ggplot(filter(workDF, ResType == restype),
                         aes(Need, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable))
    plt <- plt +
        scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 8, base_family = "serif") +
                theme(legend.position = c(.85, .15)) + ylim(0, 1)
}




## Plot Substitution (sigma) for each metric
RAMSpltSubNeed <- function(df, time, restype){
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
    plt <- ggplot(filter(workDF, ResType == restype),
                  aes(Sigma, Resilience)) +
        geom_line(aes(linetype = variable)) +
                                      ## May or may not want to facet
                                      ## this one. Looked pretty good
                                      ## on one plot, but for consistency
                                      ## probably fact it.
        scale_linetype_discrete(name = "Metrics") +
            theme_bw(base_size = 8, base_family = "serif") +
                theme(legend.position = c(.85, .15)) + ylim(0, 1)
}

## Plot resilience as the time horizon changes
RAMSpltMoveTimeH <- function(df, restype){
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
    plt <- ggplot(filter(workDF, ResType == restype),
                  aes(Time, Resilience, group = variable)) +
                                  geom_line(aes(linetype = variable))
    plt <- plt +
    scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = c(.85, .15)) + ylim(0,1)
}



## Plot the need and performance of a resilience matrix when they are
## held constant. This allows you to look at what the performance profile
## looks like when analyzing the results.
pltNeedPerf <- function(df){
    if (dim(df)[2] > 26){
        wdf <- df %>% filter(tRun == 1, nRun == 1, pRun == 1, rRun == 1)
    }
    wdf <- df %>% select(Need, Time, Performance)
    wdf <- melt(data = wdf, id = c("Time"))
    wdf <- rename(wdf, Performance = value)
    plt <- ggplot(wdf, aes(Time, Performance, group = variable)) +
                               geom_line(aes(linetype = variable)) +
                               theme_bw(base_size = 8, base_family = "serif") +
                               theme(legend.position = c(.85, .15)) +
                               scale_linetype_discrete(name = "")
}

## Plot the effect of tDelta changes on rho and extRho when decay is
## zero. Need to finish this
pltMoveTDelta <- function(df, time){
    workDF <- df %>%
        filter(Time == time) %>%
            select(Rho,
                   extRho,
                   RF_DwellTime,
                   ERF_DwellTime,
                   RF_TDelta,
                   ERF_TDelta) %>%
                       mutate(SF = RF_TDelta / RF_DwellTime) %>%
                           select(-RF_DwellTime,
                                  -RF_TDelta,
                                  -ERF_DwellTime, 
                                  -ERF_TDelta)
    ## Everything after this is wrong
    workDF <- melt(data = workDF, id = c("SF"))
    ## print(head(workDF))
    plt <- ggplot(data = workDF, aes(SF, value, group = variable,
                      color = variable)) + geom_line()
}

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
        theme_bw(base_size = 20, base_family = "serif") +
            ## scale_linetype_discrete(name = "") +
                theme(legend.position = c(.85, .15)) +
                    labs(y = "Performance") + ylim(0, 1.5)
}

RAMSPlotSave <- function(name){
    ggsave(filename = name, plot = last_plot(), width = 9.8, height = 5.7)
}

######################################################################
######################################################################
######################################################################
######################################################################
## Plot Need for each metric
WDpltMoveNeed <- function(df, time){
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
WDpltSubNeed <- function(df, time){
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
WDpltMoveTimeH <- function(df){
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
                                      facet_grid(. ~ ResType)
    plt <- plt +
    scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 20, base_family = "serif") +
            theme(legend.position = c(.95, .15))
}

######################################################################
######################################################################
## The variables and plots using the values above
## Data.frame with variable level of constant need.
nLinearVary <- data.frame(func = "constantNeed",
                          cLevel = seq(from = 0.01,
                                       to = 1.0,
                                       by = .01),
                          cLevel = 1.0,
                          startTime = NA,
                          slope = NA)
n2 <- data.frame(func = "constantNeed",
                cLevel = 0.9,
                startTime = NA,
                 slope = NA)

p2 <- data.frame(func = "step",
                failTime = 20,
                recTime = 60,
                preLevel = 1.2,
                failLevel = 0.1,
                recLevel = 1.0)

pRAMS <- data.frame(func = "step",
                failTime = 20,
                recTime = 60,
                preLevel = 1.0,
                failLevel = 0.1,
                recLevel = 1.2)

r2 <- data.frame(tDelta = 30,
                decay = 0,
                 sigma = seq(from = 0,
                     to = 1.0,
                     by = .01))

t <- data.frame(endTime = 100,
                resolution = .1)
t2 <- data.frame(endTime = 100,
                resolution = 5)
n <- data.frame(func = "constantNeed",
                cLevel = 1.0,
                startTime = NA,
                slope = NA)
p <- data.frame(func = "step",
                failTime = 10,
                recTime = 60,
                preLevel = 1,
                failLevel = .1,
                recLevel = 1)
r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)


## Vary the constant need from 0 to 1.0
nVFS <- resLoop(t, nLinearVary, p2, r)
nVFSplotRecQR <- RAMSpltMoveNeed(nVFS, 80, "Quotient Resilience")
nVFSplotFailQR <- RAMSpltMoveNeed(nVFS, 30, "Quotient Resilience")
nVFSplotRecIR <- RAMSpltMoveNeed(nVFS, 80, "Integral Resilience")
nVFSplotFailIR <- RAMSpltMoveNeed(nVFS, 30, "Integral Resilience")
nVFSplotRecE <- RAMSpltMoveNeed(nVFS, 80, "ESDF")
nVFSplotFailE <- RAMSpltMoveNeed(nVFS, 30, "ESDF")

sVFS <- resLoop(t, n2, p2, r2)
QRsVFSplotRec <- RAMSpltSubNeed(sVFS, 80, "Quotient Resilience")
QRsVFSplotFail <- RAMSpltSubNeed(sVFS, 30, "Quotient Resilience")
IRsVFSplotRec <- RAMSpltSubNeed(sVFS, 80, "Integral Resilience")
IRsVFSplotFail <- RAMSpltSubNeed(sVFS, 30, "Integral Resilience")
EsVFSplotRec <- RAMSpltSubNeed(sVFS, 80, "ESDF")
EsVFSplotFail <- RAMSpltSubNeed(sVFS, 30, "ESDF")

tVFS <- resLoop(t, n2, p2, r)
QtVFSplot <- RAMSpltMoveTimeH(tVFS, "Quotient Resilience")
ItVFSplot <- RAMSpltMoveTimeH(tVFS, "Integral Resilience")
EtVFSplot <- RAMSpltMoveTimeH(tVFS, "ESDF")

## The RAMS performance data and plot
examplePerf <- list(sigma = 0, tDelta = 20, decay =  0)

examplePerfData <- buildResMatrix(t, n, p2, r)


## The RAMS presentation plot varying time horizon
WDtVFSplot <- WDpltMoveTimeH(tVFS)

## The RAMS presentation plot varying need at time 80
WDnVFSplotRec <- WDpltMoveNeed(nVFS, 80)

## The RAMS presentation plot varying need at time 30
WDnVFSplotFail <- WDpltMoveNeed(nVFS, 30)

## The RAMS presentation plot varying substition (sigma) at time 80
WDsVFSplotRec <- WDpltSubNeed(sVFS, 80)

## The RAMS presentation plot varying substition (sigma) at time 30
WDsVFSplotFail <- WDpltSubNeed(sVFS, 30)

## I have a crummy way of naming these files. I am forgetting everything
## the letters mean. I'll try to do better

## The RAMS presentation plot of the performance profile with constant
## need profile
perfPlotRAMSPres <- pltPerf(sVFS)

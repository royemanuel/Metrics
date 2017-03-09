## A script to pull in the data from the AnyLogic Model
mdl <- read.csv("singleRunOutputsimple.csv")
mdl <- mdl %>% select(-Electric.Degrade) %>%
    mutate(Time = Time - Time[1]) %>%
    melt(id.vars = "Time", na.rm = TRUE) %>%
    mutate(value = value/100)

infraPlot <- ggplot(mdl, aes(Time, value)) +
    facet_wrap(~  variable, ncol = 2) +
    geom_line() +
    theme_bw(base_size = 12, base_family = "serif") +
    scale_linetype_discrete(name = "") +
    theme(legend.position = c(.85, .25)) +
    labs(y = "Performance") + ylim(0, 1)


## Build the resilience matrix for each of these

mdlGroup <- group_by(mdl, variable)


## Add a need value
## Here will will make a constant need to be bound to the data.frame

El.Av = mdl %>% filter(variable == "Electricity.Availability") %>%
    select(-variable, Performance = value)
El.Av = cbind(El.Av, Need = .8) ## Stakeholder need of 0.8

r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)

simResilience <- function(TPmatrix, needList, resFactors){
    ## add the Need column to the whole thing
    if (!is.null(dim(needList))){
        TPmatrix <- switch(as.character(needList$func),
                         constantNeed = constantNeed(TPmatrix,
                                                     needList$cLevel),
                         linearNeed = linearNeed(
                             resMat,
                             needList$cLevel,
                             needList$startTime,
                             needList$slope),
                         ## fullDef should bind a fully defined need
                         ## vector
                         fullDef = cbind(fullDef, resMat))
    }
    resMat <- quotRes(TPmatrix)
    resMat <- extQuotRes(resMat, sigma = resFactors$sigma)
    resMat <- resFac(resMat,
                     tDelta = resFactors$tDelta,
                     decay = resFactors$decay)
    resMat <- extResFac(resMat,
                        tDelta = resFactors$tDelta,
                        decay = resFactors$decay,
                        sigma = resFactors$sigma)
    resMat <- intRes(resMat, sigma = resFactors$sigma)
    resMat <- tidyDF(resMat)
    return(resMat)
}

needExample <- data.frame(func = c("constantNeed","constantNeed"),
                          cLevel = c(.8, .4),
                          startTime = c(NA, NA),
                          slope = c(NA, NA))


## Run through the whole data.frame for each variable like resLoop and
## resLoopShrink
infraResAll <- function(cleanData, need, resFactors){
    resMat <- data.frame()
    ## if this isn't a list of infrastructure, build a function list
    ## so it runs.
    if (is.null(unique(cleanData$variable))){
        cleanData <- cleanData %>% mutate(variable = "One List")
    }
    funList <- unique(cleanData$variable)
    ## print(funList)
    ## if need is already part of the cleanData, build a need data.frame
    ## so it will run
    if (is.null(dim(need)[1])){
        need <- data.frame(0)
    }
    needStep <- dim(need)[1]
    resStep <- dim(resFactors)[1]
    for (fun in 1:length(funList)){
        TPmatrix <- cleanData %>%
            filter(variable == funList[fun]) %>%
            select(-variable, Performance = value)
        for (needRun in 1:needStep){
            for (resRun in 1:resStep){
                print(paste0("NR = ", needRun, ", ",
                             "RR = ", resRun, ", ",
                             "Infrastructure = ", funList[fun]))
                k <- simResilience(TPmatrix,
                                   need[needRun,],
                                   resFactors[resRun,])
                k <- cbind(k,
                           nRun = needRun,
                           rRun = resRun,
                           Infrastructure = funList[fun],
                           Decay = resFactors$decay[resRun],
                           Sigma = resFactors$sigma[resRun])
                resMat <- rbind(resMat, k)
            }
        }
    }
    resMat
}

m <- infraResAll(mdl, needExample, r)

## Plot all the performance curves except for Quotient Resilience
## I think I'm going to get rid of all the Quotient Resilience plotting?
## Maybe
pltMoveTimeInfra <- function(df){
    workDF <- df %>%
         select(Time, QR, EQR, Rho,
                extRho, statQuoResilience, extResilience,
                nRun, Infrastructure)
    workDF <- melt(data = workDF, id = c("Time", "Infrastructure", "nRun"))
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
                                 # "Extended",
                                        # this for plotting multiple
                                        # needs on one timeline
                                        # replace "Extended" with
                                         .4 * nRun/10,
                                 "Original"))
    workDF <- rename(workDF, Resilience = value)
    ## print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Time, Resilience,
                              group = variable)) +
        geom_line(aes(linetype = variable,
                      color = variable)) +
        facet_grid(Infrastructure ~ ResType)
    plt <- plt +
        scale_linetype_discrete(name = "Need") +
        scale_color_discrete(name = "Need") +
        theme_bw(base_size = 8, base_family = "serif")
}
n <- pltMoveTimeInfra(m)

######################################################################
## Build the model with the need level stepping from .1 to 1.0 by .1

nCnst <- data.frame(func = "constantNeed",
                          cLevel = seq(from = 0.1,
                                       to = 1.0,
                                       by = .1),
                          cLevel = 1.0,
                          startTime = NA,
                          slope = NA)

cnstNeed <- infraResAll(mdl, nCnst, r)

cnPlot <- pltMoveTimeInfra(cnstNeed)

######################################################################
## Build the data.frame with the resilience metrics according to each
## of the stakeholders.

## First, define the need of each stakeholder
nl <- c(.5, .9, .75, .95, .8)

## Second, define the sigmas for each stakeholder. I did not vary the
## decay parameter here, but I may want to when I get the SpeedFactor
## fixed. I wonder what I need to do for that. That can be our initial
## simplifying assumption
sl <- c(.1, .2, .4, .7, 0, .5)

## build the need data.frame for input into infraResAll
nMat <- data.frame(func = "constantNeed",
                   cLevel = nl,
                   startTime = NA,
                   slope = NA)

## build the resilience factor data.frame for input into infraResAll
rMat <-data.frame(tDelta = 30,
                decay = 0,
                  sigma = sl)

## Build the full data.frame of resilience. This is pretty darn big
stakeRes <- infraResAll(mdl, need = nMat, resFactors = rMat)
wf <- stakeRes %>%
    filter(Need == .8 &
                          Infrastructure == "Water.Functionality" &
                              Sigma == 0.5)

## shrink the data.frame for each stakeholder's perspective on their
## own function
es <- stakeRes %>%
    filter(Need == .8 &
           Infrastructure == "Emergency.Services.Functionality" &
           Sigma == 0)
cmf <- stakeRes %>%
    filter(Need == .95 &
           Infrastructure == "Critical.Manufacturing.Functionality" &
           Sigma == .7)
tf <- stakeRes %>%
    filter(Need == .75 &
           Infrastructure == "Transportation.Function" &
           Sigma == .4)
hf <- stakeRes %>%
    filter(Need == .9 &
           Infrastructure == "Healthcare.Function" &
           Sigma == .2)
itf <- stakeRes %>%
    filter(Need == .5 &
           Infrastructure == "IT.Function" &
           Sigma == .2)
commf <- stakeRes %>%
    filter(Need == .5 &
           Infrastructure == "Communications.Function" &
           Sigma == .1)
selectedStakeRes <- bind_rows(wf, es, cmf, tf, hf, itf, commf)

ssrPlot <- pltMoveTimeInfra(selectedStakeRes)

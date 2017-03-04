## A script to pull in the data from the AnyLogic Model
mdl <- read.csv("singleRunOutput.csv")
mdl <- mdl %>% select(-Electric.Degrade) %>%
    mutate(Time = Time - Time[1]) %>%
    melt(id.vars = "Time", na.rm = TRUE) %>%
    mutate(value = value/100)

infraPlot <- ggplot(mdl, aes(Time, value)) +
    facet_wrap(~  variable, ncol = 2) +
    geom_line()

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
    resMat <- switch(as.character(needList$func),
                     constantNeed = constantNeed(TPmatrix, needList$cLevel),
                     linearNeed = linearNeed(
                         resMat,
                         needList$cLevel,
                         needList$startTime,
                         needList$slope),
                     ## fullDef should bind a fully defined need
                     ## vector
                     fullDef = cbind(fullDef, resMat))
    resMat <- quotRes(resMat)
    resMat <- extQuotRes(resMat, sigma = resFactors$sigma)
    resMat <- resFac(resMat,
                     tDelta = resFactors$tDelta,
                     decay = resFactors$decay)
    resMat <- extResFac(resMat,
                        tDelta = resFactors$tDelta,
                        decay = resFactors$decay,
                        sigma = resFactors$sigma)
    resMat <- intRes(resMat, sigma = resFactors$sigma)
    ## resMat <- tidyDF(resMat)
    return(resMat)
}

needExample <- data.frame(func = c("constantNeed","constantNeed"),
                          cLevel = c(.8, 1.5),
                          startTime = c(NA, NA),
                          slope = c(NA, NA))

g <- simResilience(El.Av, needExample, r)
h <- pltMoveTimeH(g)
h

## Run through the whole data.frame for each variable like resLoop and
## resLoopShrink
infraResAll <- function(cleanData, need, resFactors){
    resMat <- data.frame()
    funList <- unique(cleanData$variable)
    ## print(funList)
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
                   nRun,
                   "Original"))
    workDF <- rename(workDF, Resilience = value)
    print(head(workDF))
    ## print(tail(workDF))
    ## print(colnames(workDF))
    plt <- ggplot(workDF, aes(Time, Resilience,
                              group = variable)) +
                                  geom_line(aes(linetype = variable)) +
                                      facet_grid(ResType ~ Infrastructure)
    plt <- plt +
    scale_linetype_discrete(name = "Metrics") +
        theme_bw(base_size = 8, base_family = "serif") +
            theme(legend.position = c(.85, .15))
}
n <- pltMoveTimeInfra(m)

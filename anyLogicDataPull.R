######################################################################
## Trying to consolidate the mess I have in bigInf.R, bigInf2.R,
## Infrastructure.R and infrastructure2.R.
######################################################################

## put the names of the files you want in a list
bigInfList <- c("bigAsIs.csv", 
                "big16kRec.csv",
                "big100percentRec.csv",
                "bigRob.csv",
                "bigStep.csv")

## When using this with multiple files, the multScenario and
## multScenarioFast functions are what you should call.
## First they clean up the .csv files so they can be used using the
## cleanAnyLogic function. Then they call multInfrastructure and
## multInfrastructureFast functions respectively. These make use of the
## resilienceFromData function which calculates the resilience for a time
## series when given a needList and a list of resFactors They return the full
## resilience matrix with all the data, interim and final.


## The files from AnyLogic have some strange column issues that Need
## to be sorted.

cleanAnyLogic <- function(fileNames){
    allDF <- data.frame()
    for (f in 1:length(fileNames)){
        print(fileNames[f])
        DF <- read.csv(fileNames[f])
        dfCol <- colnames(DF)
        dfCol[1] <- "Time"
        colnames(DF) <- dfCol
        DF <- DF %>% select(1:10) %>%
            select(-Electric.Degrade) %>%
                mutate(Time = Time - 40)
        DF$Scenario <- fileNames[f]
        DF <- melt(DF, id.vars = c("Time", "Scenario"))
        allDF <- bind_rows(allDF, DF)
    }
    return(allDF)
}


resilienceFromData <- function(TPmatrix, needList, resFactors){
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

multInfrastructure <- function(cleanData, need, resFactors){
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
        print(dim(TPmatrix))
        for (needRun in 1:needStep){
            for (resRun in 1:resStep){
                print(paste0("NR = ", needRun, ", ",
                             "RR = ", resRun, ", ",
                             "Infrastructure = ", funList[fun]))
                k <- resilienceFromData(TPmatrix,
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
    return(resMat)
}
## Same as above, but you need to specify the time that you want to
## pull the resilience. MUCH faster. Use the one above when you need
## make a time plot of the resilience. 
multInfrastructureFast <- function(cleanData, need, resFactors, timeHorizon){
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
        print(dim(TPmatrix))
        for (needRun in 1:needStep){
            for (resRun in 1:resStep){
                print(paste0("NR = ", needRun, ", ",
                             "RR = ", resRun, ", ",
                             "Infrastructure = ", funList[fun]))
                k <- resilienceFromData(TPmatrix,
                                   need[needRun,],
                                   resFactors[resRun,])
                k <- cbind(k,
                           nRun = needRun,
                           rRun = resRun,
                           Infrastructure = funList[fun],
                           Decay = resFactors$decay[resRun],
                           Sigma = resFactors$sigma[resRun])
                k <- filter(k, Time == timeHorizon)
                resMat <- rbind(resMat, k)
            }
        }
    }
    resMat
}

## Now we run this through multiple files for all times
multScenario <- function(fileNames, N, R){
    scenarioResilience <- data.frame()
    for (f in 1:length(fileNames)){
        cleanData <- cleanAnyLogic(fileNames[f])
        singleInf <- multInfrastructure(cleanData,
                                        need = N,
                                        resFactors = R)
        scenarioResilience <- bind_rows(scenarioResilience, singleInf)
    }
    return(scenarioResilience)
}
## Now we run this through multiple files for one time horizon
multScenarioFast<- function(fileNames, N, R, TH){
    scenarioResilience <- data.frame()
    for (f in 1:length(fileNames)){
        cd <- cleanAnyLogic(fileNames[f])
        singleInf <- multInfrastructureFast(cleanData = cd,
                                            need = N,
                                            resFactors = R,
                                            timeHorizon = TH)
        scenarioResilience <- bind_rows(scenarioResilience, singleInf)
    }
    return(scenarioResilience)
}

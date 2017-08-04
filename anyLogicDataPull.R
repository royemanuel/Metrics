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
        if ("Time" %in% dfCol){
        } else{
            dfCol[1] <- "Time"
        }
        colnames(DF) <- dfCol
        if (!("Run" %in% colnames(DF))){
            DF <- mutate(DF, Run = 1)
        }
        DFtimerun <- DF %>% select(Time,
                            Run) %>%
                                mutate(Time = (Time - 40)/1440)
        DFperf <- DF %>% select( Electricity.Availability,
                            Communications.Function,
                            IT.Function,
                            Healthcare.Function,
                            Transportation.Function,
                            Emergency.Services.Functionality,
                            Critical.Manufacturing.Functionality,
                                Water.Functionality)
        DFperf <- DFperf / 100
        DF <- cbind(DFtimerun, DFperf)
        DF$Scenario <- fileNames[f]
        DF <- melt(DF, id.vars = c("Time", "Scenario", "Run"))
        allDF <- bind_rows(allDF, DF)
    }
    return(allDF)
}

## This function calculates the resilience for a timeseries of performance
## for each infrastructure
resilienceFromData <- function(TPmatrix, needList, resFactors,
                               timeHorizon){
    ## add the Need column to the whole thing
    if (!is.null(dim(needList))){
        TPmatrix <- switch(as.character(needList$func),
                         constantNeed = constantNeed(TPmatrix,
                                                     Need = needList$cLevel),
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
    ## t <- proc.time()
    ## print(t)
    resMat <- totalQR(resMat, TH = timeHorizon)
    ## print(proc.time-t)
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
## Able to comment out the old multInfrastructure and the multScenario
## because I discovered optional arguments. With a default value for
## Time horizon, I can put it in the code, and make it fiture out whether
## it wants to go "fast" or not with an if(!is.null(timeHorizon)) statement
## multInfrastructure <- function(cleanData, need, resFactors){
##     resMat <- data.frame()
##     ## if this isn't a list of infrastructure, build a function list
##     ## so it runs.
##     if (is.null(unique(cleanData$variable))){
##         cleanData <- cleanData %>% mutate(variable = "One List")
##     }
##     funList <- unique(cleanData$variable)
##     ## print(funList)
##     ## if need is already part of the cleanData, build a need data.frame
##     ## so it will run
##     if (is.null(dim(need)[1])){
##         need <- data.frame(0)
##     }
##     needStep <- dim(need)[1]
##     resStep <- dim(resFactors)[1]
##     for (fun in 1:length(funList)){
##         TPmatrix <- cleanData %>%
##             filter(variable == funList[fun]) %>%
##                 select(-variable, Performance = value)
##         print(dim(TPmatrix))
##         for (needRun in 1:needStep){
##             for (resRun in 1:resStep){
##                 print(paste0("NR = ", needRun, ", ",
##                              "RR = ", resRun, ", ",
##                              "Infrastructure = ", funList[fun]))
##                 k <- resilienceFromData(TPmatrix,
##                                    need[needRun,],
##                                    resFactors[resRun,])
##                 k <- cbind(k,
##                            nRun = needRun,
##                            rRun = resRun,
##                            Infrastructure = funList[fun],
##                            Decay = resFactors$decay[resRun],
##                            Sigma = resFactors$sigma[resRun])
##                 resMat <- rbind(resMat, k)
##             }
##         }
##     }
##     return(resMat)
## }
## Same as above, but you need to specify the time that you want to
## pull the resilience. MUCH faster. Use the one above when you need
## make a time plot of the resilience. 
multInfrastructureFast <- function(cleanData, simRun, need, resFactors,
                                   timeHorizon=NULL){
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
                print(paste0("SR = ", simRun, ", ",
                             "NR = ", needRun, ", ",
                             "RR = ", resRun, ", ",
                             "Infrastructure = ", funList[fun]))
                k <- resilienceFromData(TPmatrix,
                                   need[needRun,],
                                        resFactors[resRun,],
                                        timeHorizon)
                k <- cbind(k,
                           nRun = needRun,
                           rRun = resRun,
                           Infrastructure = funList[fun],
                           Decay = resFactors$decay[resRun],
                           Sigma = resFactors$sigma[resRun])
                if (!is.null(timeHorizon)){
                    k <- filter(k, Time == timeHorizon)
                }
                ##ifelse(dim(resMat)[1] == 0, break,)
                resMat <- rbind(resMat, k)
            }
        }
    }
    return(resMat)
}

## Now we run this through multiple files for all times
## multScenario <- function(fileNames, N, R){
##     scenarioResilience <- data.frame()
##     for (f in 1:length(fileNames)){
##         cleanData <- cleanAnyLogic(fileNames[f])
##         singleInf <- multInfrastructure(cleanData,
##                                         need = N,
##                                         resFactors = R)
##         scenarioResilience <- bind_rows(scenarioResilience, singleInf)
##     }
##     return(scenarioResilience)
## }
## Now we run this through multiple files for one time horizon
multScenarioFast<- function(fileNames, N, R, TH=NULL){
    scenarioResilience <- data.frame()
    for (f in 1:length(fileNames)){
        cd <- cleanAnyLogic(fileNames[f])
        if(!is.null(TH)){
            if(TH > max(cd$Time)){
                stop("Time horizon is greater than simulation time.")
            }
        }
        simResilience <- data.frame()
        for (sim in 1:max(cd$Run)){
            cdSim <- cd %>%
                filter(Run == sim)
            head(cdSim)
            cdSim <- select(cdSim, -Run)
            singleInf <- multInfrastructureFast(cleanData = cdSim,
                                                simRun = sim,
                                                need = N,
                                                resFactors = R,
                                                timeHorizon = TH)
            singleInf <- mutate(singleInf, sRun = sim)
            simResilience <- bind_rows(simResilience, singleInf)
        }
        scenarioResilience <- bind_rows(scenarioResilience,
                                        simResilience)
    }
    return(scenarioResilience)
}
    

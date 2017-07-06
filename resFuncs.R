## A file that contains only functions. This is intended to pull everything
## together for documentation

## simResilience takes a a time and performance matrix, a needList
## and resilience factors. The first if statement sorts out the need list
## as per what kind of need is being modeled. It includes a constant need,
## a linear need, and a fully defined need that id bound to the TP matrix


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


## The csv's that come from the Austin model have a column we don't need
## and misspell Time, so we fix that here with tidyMat.
tidyMat <- function(resMat){
    resCol <- colnames(resMat)
    resCol[1] <- "Time"
    colnames(resMat) <- resCol
    resMat <- resMat %>%
        select(-Electric.Degrade) %>%
            melt(id.vars = "Time", na.rm = TRUE) %>%
                mutate(value = value/100)
}

## buildDF builds the dataframe to be used in a lot of the following
## functions rather than running through the same process for every
## different data set. Its input is a list of filenames.
buildDF <- function(fileNames){
    allDF <- data.frame()
    for (f in 1:length(fileNames)){
        print(fileNames[f])
        DF <- read.csv(fileNames[f])
        DF <- tidyMat(DF)
        ## dfCol <- colnames(DF)
        ## dfCol[1] <- "Time"
        ## colnames(DF) <- dfCol
        ## DF <- DF %>% select(1:10) %>%
        ##     select(-Electric.Degrade)
        DF$Scenario <- fileNames[f]
        allDF <- bind_rows(allDF, DF)
    }
    return(allDF)
}


## infraResall takes a cleaned up matrix from the output of the Austin model
## It includes every value for every timestep. For long runs, it becomes
## very slow. I need to think about a better way to bind all of these
## together. 
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
## Same as above, but you need to specify the time that you want to
## pull the resilience. MUCH faster. Use the one above when you need
## make a time plot of the resilience. 
infraResFast <- function(cleanData, need, resFactors, timeHorizon){
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
                k <- filter(k, Time == timeHorizon)
                resMat <- rbind(resMat, k)
            }
        }
    }
    resMat
}

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

## Make the data look how we want it to look
## round all values to the hundredths place
round_df <- function(df, digits){
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    (df)
}

######################################################################
## This is to redo infrastructure.R with the big files. Might not be worth it.

## Clean up the csv. The first column has a bad name
metricRollup <- function(names, need, resFactors, timeHorizon){
    ssr <- data.frame()
    for (csv in 1:length(names)){
        f <- read.csv(names[csv])
        ## fCol <- colnames(f)
        ## fCol[1] <- "Time"
        ## colnames(f) <- fCol
        ## f <- f %>% select(-Electric.Degrade) %>%
        ##     mutate(Time = Time - Time[1]) %>%
        ##         melt(id.vars = "Time", na.rm = TRUE) %>%
        ##             mutate(value = value/100)
        f <- tidyMat(f)
        fRes <- infraResFast(f, need, resFactors, timeHorizon)
        wf <- stakeRes %>%
            filter(Need == .8 &
                       Infrastructure == "Water.Functionality" &
                           Sigma == 0.5)
        es <- fRes %>%
            filter(Need == .8 &
                       Infrastructure == "Emergency.Services.Functionality" &
                           Sigma == 0)
        cmf <- fRes %>%
            filter(Need == .95 &
                       Infrastructure == "Critical.Manufacturing.Functionality" &
                           Sigma == .7)
        tf <- fRes %>%
            filter(Need == .75 &
                       Infrastructure == "Transportation.Function" &
                           Sigma == .4)
        hf <- fRes %>%
            filter(Need == .9 &
                       Infrastructure == "Healthcare.Function" &
                           Sigma == .2)
        itf <- fRes %>%
            filter(Need == .5 &
                       Infrastructure == "IT.Function" &
                           Sigma == .2)
        commf <- fRes %>%
            filter(Need == .5 &
                       Infrastructure == "Communications.Function" &
                           Sigma == .1)
        thisScenario <- bind_rows(wf, es, cmf, tf, hf, itf, commf)
        thisScenario$fileName <- names[csv]
        thisScenario$Scenario <- LETTERS[csv]
        ssr <- bind_rows(ssr, thisScenario)
        ## readline(prompt="Press [enter] to continue")
    }
    return(ssr)
}

## Build the performance plots for each of the Infrastructures
allPerfPlot <- function(fileNames){
   allDF <- buildDF(fileNames)
    resDF <- melt(allDF, id.vars = c("Time", "Scenario"))
    print(colnames(resDF))
    pPlot <- ggplot(resDF, aes(Time, value, group = variable)) +
        geom_line() + facet_grid(Scenario ~ variable)
}

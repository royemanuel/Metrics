######################################################################
## This is to redo infrastructure.R with the big files. Might not be worth it.

## Clean up the csv. The first column has a bad name
## metricRollup <- function(names, need, resFactors, timeHorizon){
##     ssr <- data.frame()
##     for (csv in 1:length(names)){
##         f <- read.csv(names[csv])
##         ## fCol <- colnames(f)
##         ## fCol[1] <- "Time"
##         ## colnames(f) <- fCol
##         ## f <- f %>% select(-Electric.Degrade) %>%
##         ##     mutate(Time = Time - Time[1]) %>%
##         ##         melt(id.vars = "Time", na.rm = TRUE) %>%
##         ##             mutate(value = value/100)
##         tidyMat(f)
##         fRes <- infraResFast(f, need, resFactors, timeHorizon)
##         wf <- stakeRes %>%
##             filter(Need == .8 &
##                        Infrastructure == "Water.Functionality" &
##                            Sigma == 0.5)
##         es <- fRes %>%
##             filter(Need == .8 &
##                        Infrastructure == "Emergency.Services.Functionality" &
##                            Sigma == 0)
##         cmf <- fRes %>%
##             filter(Need == .95 &
##                        Infrastructure == "Critical.Manufacturing.Functionality" &
##                            Sigma == .7)
##         tf <- fRes %>%
##             filter(Need == .75 &
##                        Infrastructure == "Transportation.Function" &
##                            Sigma == .4)
##         hf <- fRes %>%
##             filter(Need == .9 &
##                        Infrastructure == "Healthcare.Function" &
##                            Sigma == .2)
##         itf <- fRes %>%
##             filter(Need == .5 &
##                        Infrastructure == "IT.Function" &
##                            Sigma == .2)
##         commf <- fRes %>%
##             filter(Need == .5 &
##                        Infrastructure == "Communications.Function" &
##                            Sigma == .1)
##         thisScenario <- bind_rows(wf, es, cmf, tf, hf, itf, commf)
##         thisScenario$Scenario <- names[csv]
##         print(dim(thisScenario))
##         ssr <- bind_rows(ssr, thisScenario)
##         ## readline(prompt="Press [enter] to continue")
##     }
##     return(ssr)
## }

nameList <- c("bigAsIs.csv", "big16kRec.csv", "big100percentRec.csv",
              "bigRob.csv", "bigStep.csv")

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

## This builds the resilience for each electric failure scenario
## (particular to the SD model).
bigInfRFR <- metricRollup(nameList, need = nMat, resFactors = rMat, 39000)

## Writing the .csv. Leave it commented out unless you have new data and
## want to make it happen. I would recommend rewriting this part each
## time you ahve new data to put into it.
## write.csv(bigInfRFR, "bigInfrastructureRunsForRecord.csv")

## Pull out only the resilience metrics, Infrastructure and scenario
bigInfResilience <- select(bigInfRFR, QR, EQR, Rho, extRho,
                           statQuoResilience, extResilience,
                           Infrastructure, Scenario)
## write.csv(bigInfResilience, "bigInfrastructureResilience.csv")

bir <- melt(bigInfResilience, id.vars = c("Scenario", "Infrastructure"))

resPointPlot <- ggplot(bir, aes(Scenario, value)) +
    facet_grid(Infrastructure ~ variable) +
        geom_point()

buildDF <- function(fileNames){
    allDF <- data.frame()
    for (f in 1:length(fileNames)){
        print(fileNames[f])
        DF <- read.csv(fileNames[f])
        dfCol <- colnames(DF)
        dfCol[1] <- "Time"
        colnames(DF) <- dfCol
        DF <- DF %>% select(1:10) %>%
            select(-Electric.Degrade)
        DF$Scenario <- fileNames[f]
        allDF <- bind_rows(allDF, DF)
    }
    return(allDF)
}

allPerfPlot <- function(fileNames){
   allDF <- buildDF(fileNames)
    resDF <- melt(allDF, id.vars = c("Time", "Scenario"))
    print(colnames(resDF))
    pPlot <- ggplot(resDF, aes(Time, value, group = variable)) +
        geom_line() + facet_grid(Scenario ~ variable)
}

## I am not sure what this is meant to do
## bigInfraPlot <- ggplot(bigInfResilience, aes(, value)) +
##     facet_wrap(~  Infrastructure, ncol = 2) +
##     geom_line() +
##     theme_bw(base_size = 12, base_family = "serif") +
##     scale_linetype_discrete(name = "") +
##     theme(legend.position = c(.85, .25)) +
##         labs(y = "Performance") + ylim(0, 1)

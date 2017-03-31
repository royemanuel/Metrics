######################################################################
## This is to redo infrastructure.R with the big files. Might not be worth it.

## Clean up the csv. The first column has a bad name
metricRollup <- function(names, need, resFactors, timeHorizon){
    ssr <- data.frame()
    for (csv in 1:length(names)){
        f <- read.csv(names[csv])
        print(head(f))
        readline(prompt="Press [enter] to continue")
        fCol <- colnames(f)
        fCol[1] <- "Time"
        colnames(f) <- fCol
        f <- f %>% select(-Electric.Degrade) %>%
            mutate(Time = Time - Time[1]) %>%
                melt(id.vars = "Time", na.rm = TRUE) %>%
                    mutate(value = value/100)
        print(head(f))
        readline(prompt="Press [enter] to continue")
        fRes <- infraResFast(f, need, resFactors, timeHorizon)
        print(colnames(fRes))
        print(dim(fRes))
        print(head(fRes))
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
        print(tf)
        hf <- fRes %>%
            filter(Need == .9 &
                       Infrastructure == "Healthcare.Function" &
                           Sigma == .2)
        print(hf)
        itf <- fRes %>%
            filter(Need == .5 &
                       Infrastructure == "IT.Function" &
                           Sigma == .2)
        commf <- fRes %>%
            filter(Need == .5 &
                       Infrastructure == "Communications.Function" &
                           Sigma == .1)
        ssr <- bind_rows(ssr,
                         wf, es, cmf, tf,
                         hf, itf, commf)
        print(ssrs)
        readline(prompt="Press [enter] to continue")
        return(ssr)
    }
    return(ssr)
}

nameList <- c("bigAsIs.csv", "big16kRec.csv") #, "big100percentRec.csv",
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


tryItOut <- metricRollup(nameList, need = nMat, resFactors = rMat, 39960)

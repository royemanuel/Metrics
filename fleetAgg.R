## source("fleetRes.R")
## This file takes a folder of fleet results and calculates the resilience
## using a list of Chi values. 
source("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/fleetRes.R")
setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/fleetData/RFR2/8SEPData")
library("tidyverse")
library("readxl")
source("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/fleetSqdnCdr.R")
######################################################################
## This file takes all the output files from the simulation, sorts
## through them by seed and combines a single seed into 

starttime <- Sys.time()               
timetoGradFilesMstr <- list.files(path = ".", pattern = "aircrew")
skedFilesMstr <- list.files(path = ".", pattern = "sked")
timeHorizonList <- c(15, 20, 25, 30, 35) * 24 * 365
## If you want the big master schedule, set BIG to True

BIG <- FALSE

dataDate <- "8SEPscrubbed7SEP"
######################################################################
## Define different Chi for each requirement

chiSatPre <- c(0)
chiSatPost <- c(0)

chiAoPre <- c(0)
chiAoPost <- c(0)

######################################################################
## listChi for an expanded intertemporal substitutability study. 
listChi <- list(list(c(0), c(0)),
                list(c(0), c(.5)),
                list(c(0), c(1, .5)) ,
                list(c(0), c(.5, .25)) ,
                list(c(0), rep(1, 10000000)),
                list(c(0),             c(0)), 
                list(c(.5),            c(0)), 
                list(c(1, .5) ,        c(0)), 
                list(c(.5, .25) ,      c(0)), 
                list(rep(1, 10000000), c(0)), 
                list(c(.5), c(.5)),
                list(c(1, .5), c(1, .5)) ,
                list(c(1, .75, .5, .25), c(1, .75, .5, .25)) ,
                list(rep(1, 10000000), rep(1, 10000000)))
chiGradPre <- c(0)
chiGradPost <- c(0)

rseed <-
    list.files(path = ".", pattern = "ParametersRdm") %>%
    str_remove("ParametersRdmSd") %>%
    str_remove(".xlsx")


######################################################################
## Lists where the final resilience values are stored
satList <- c()
gradList <- c()
AoList <- c()
xpList <- c()
rList <- c()


######################################################################
## Student satisfaction values
timeToGraduate <- 1440
desiredPercentSatisfied <- .85
COorderlength <- 3
desiredGrads <- 65

######################################################################
## Big for loop to go through the directory where I put the data
## resilienceDF <- tibble(SAT = 0, GRAD = 0, Ao = 0,
##                        Run = '0', Experiment = '0', Seed = 0,
##                        TimeHorizon = 0)
## resilienceDF <- filter(resilienceDF, SAT == 1)
ttgDF <- tibble(MAX = 0, MIN = 0, MED = 0, SD = 0,
                Run = '0', Experiment = '0', Seed = 0,
                EndTime = 0)
ttgDF <- filter(ttgDF, MAX == 1)
k = 0
for(rs in 1:length(rseed)){
    resilienceDF <- tibble()
    print(rs)
    ttgBin <- str_detect(timetoGradFilesMstr, as.character(rseed[rs]))
    sfBin <- str_detect(skedFilesMstr, as.character(rseed[rs]))
    timetoGradFiles <- timetoGradFilesMstr[ttgBin]
    skedFiles <- skedFilesMstr[sfBin]
    mstrList = list()
    COSATlist = list()
    COgradList = list()
    for (ttg in 1:length(timetoGradFiles)){
        run <-
            skedFiles[ttg] %>%
            str_extract("(?<=Run?)\\d+")
        exprmnt <-
            skedFiles[ttg] %>%
            str_extract("(?<=Exp?)\\d+")
        if(as.integer(exprmnt) > 3){
            srg <- TRUE
        } else {
            srg <- FALSE
        }
        print(paste("Was there a surge?", srg))
        DFrun <- read_csv(timetoGradFiles[ttg],
                          col_types = list(col_integer(),
                                           col_character(),
                                           col_character(),
                                           col_character(),
                                           col_character(),
                                           col_character()))
        ##Squadron CO work, ensure the inputs match below
        runSatCO <- COsatRes(DFrun,
                             desiredPercentSatisfied,
                             timeToGraduate,
                             COorderlength) %>%
            mutate(Exp = exprmnt,
                   Run = run,
                   Seed = rseed[rs])
        k <- k + 1
        COSATlist[[k]] <- runSatCO
        sked <- read_csv(skedFiles[ttg],
                         col_types = list(
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer(),
                             col_integer()))
        gradChi <- list()
        for(chi in 1:length(listChi)){
            runGradCO <- COgradRes(sked,
                                   desiredGrads,
                                   listChi[[chi]][[1]],
                                   listChi[[chi]][[2]],
                                   srg,
                                   COorderlength)
            runGradCO <-
                runGradCO %>%
                mutate(Chi = chi,
                       Exp = exprmnt,
                       Run = run,
                       Seed = rseed[rs])                       
            gradChi[[chi]] <- runGradCO
        }
        COgradList[[k]] <- bind_rows(gradChi)
        for(th in 1:length(timeHorizonList)){
            TH <- timeHorizonList[th]
            DFrunTH <-
                DFrun %>%
                filter(exitDate < TH)
            skedTH <-
                sked %>%
                filter(Time < TH)
            DFrunsumTH <-
                DFrunTH %>%
                filter(Disp == 'G') %>%
                select(TimeInSqdn) %>%
                mutate(TimeInSqdn = as.double(TimeInSqdn)) %>%
                summarise(MAX = max(TimeInSqdn),
                          MIN = min(TimeInSqdn),
                          MED = median(TimeInSqdn),
                          MEAN = mean(TimeInSqdn),
                          SD = sd(TimeInSqdn)) %>%
                mutate(Run = run, Experiment = exprmnt, Seed = rs)
            satVal <- satRes(DFrunTH, desiredPercentSatisfied, timeToGraduate)
            AoVal <- AoRes(skedTH, 0.85)
            gTib <- tibble()
            for(c in 1:length(listChi)){
                gradVal <- gradRes(skedTH,
                                   desiredGrads,
                                   listChi[[c]][[1]],
                                   listChi[[c]][[2]],
                                   srg)
                gv <- tibble(GRAD = gradVal, Chi = c, Run = run)
                gTib <- bind_rows(gTib, gv)
            }
            xpVal <- exprmnt
            rVal <- run
            skedEnd <-
                skedTH %>%
                summarise(simEnd = max(Time))
            if (skedEnd$simEnd < TH){
                satVal <- satVal * skedEnd$simEnd / TH
                AoVal <- AoVal * skedEnd$simEnd / TH
                gTib <-
                    gTib %>%
                    mutate(GRAD = GRAD * skedEnd$simEnd / TH)
            }
            DFrunsumTH$EndTime <- skedEnd$simEnd
            DFrunsumTH$TimeHorizon <- TH
            ttgDF <- bind_rows(ttgDF, DFrunsumTH)
            if (BIG){
                mstrList[[ttg]] <- sked
            }
            thDF <- tibble(Run = run,
                           SAT = satVal,
                           Ao = AoVal,
                           Experiment = xpVal,
                           Seed = rseed[rs],
                           TimeHorizon = TH)
            thDF <- inner_join(thDF, gTib, by = "Run")
            resilienceDF <- bind_rows(resilienceDF, thDF)
            ## resilienceDF <- add_row(resilienceDF,
            ##                         SAT = satVal,
            ##                         GRAD = gradVal,
            ##                         Ao = AoVal,
            ##                         Run = rVal,
            ##                         Experiment = xpVal,
            ##                          Seed = rs,
            ##                         TimeHorizon = TH)
        }
        print(paste("From Seed", rseed[rs],
                    "added Experiment", exprmnt,
                    "Run", run))
    }
    write_csv(resilienceDF, paste0("allRes", dataDate, rseed[rs],
                                   ".csv"))
    COSATlist <- bind_rows(COSATlist)
    write_csv(COSATlist, paste0("COsatResilience", dataDate, rseed[rs], ".csv"))
    COgradList <- bind_rows(COgradList)
    write_csv(COgradList, paste0("COgradResilience", dataDate, rseed[rs], ".csv"))
    print(paste("Seed", rseed[rs]))
}
write_csv(ttgDF, "ttgDF8SEP.csv")


if(BIG){
    mstrSked <- bind_rows(mstrList)
}

    
    
##countFiles <- function(lst, seedlst){
##    for(1 in 1:length(seedlst)){
##        strp <- str_detect(lst, as.character(seedlst[rs]))
##    }
##}

## Get rid of the "seed" from resilience DF and do a proper count of runs
## On second thought, there does not seem to be a purpose to this at all!

## runCheck <- function(DF){
##     storeDF <- tibble()
##     timehorizon <- unique(DF$TimeHorizon)
##     print(timehorizon)
##     for(TH in 1:length(timehorizon)){
##         wDF <-
##             DF %>%
##             filter(TimeHorizon == timehorizon[TH]) %>%
##             group_by(Experiment) %>%
##             mutate(Run = as.integer(Run)) %>%
##             summarise(MAXRUN = max(Run))
##         k <- wDF$MAXRUN != max(wDF$MAXRUN)
##         if(sum(k) == 0){
##             aDF <-
##                 DF %>%               
##                 filter(TimeHorizon == timehorizon[TH]) %>%
##                 mutate(Run = as.integer(Run),
##                        Run = (Run + 1) + (max(Run + 1)) * (Seed - 1),
##                        Run = as.character(Run)) %>%
##                 select(-Seed)
##             print(aDF)
##             return(aDF)
##         } else {
##             print("not the same number of runs")
##         }
##         storeDF <- bind_rows(storeDF, aDF)
##         print(storeDF)
##     }
##     return(storeDF)
## }

        

#ttgDFtst <- runCheck(ttgDF)

ttgSum <-
    ttgDF %>%
    group_by(Experiment, TimeHorizon) %>%
    summarise(MAX = max(MAX), min = min(MIN), MEAN = mean(MEAN))

#workRDF <- runCheck(resilienceDF)

######################################################################
## troubleshooting dataframes and plots
## acView <-
##     mstrSked %>%
##     select(Time, SLEPlist, boneYard, flightLine, upAircraft, Run, Experiment) %>%
##     gather(Category, Count, -Time, -Run, -Experiment) %>%
##     mutate(Time = Time / (24 * 365))
## 
## studView <- 
##     mstrSked %>%
##     select(Time, SLEPlist, boneYard, flightLine,
##            students, upAircraft, Run, Experiment) %>%
##     gather(Category, Count, -Time, -Run, -Experiment) %>%
##     mutate(Time = Time / (24 * 365))
## 
## acVplot <-
##     ggplot(acView, aes(Time, Count, group = Category, color = Category)) +
##     geom_line() +
##     facet_grid(Experiment ~ Run)
## 
## studVplot <- 
##     ggplot(studView, aes(Time, Count, group = Category, color = Category)) +
##     geom_line() +
##     facet_grid(Experiment ~ Run)
## 
## timeView <-
##     mstrSked %>%
##     group_by(Experiment, Run) %>%
##     summarise(EndTime = max(Time) / (365 * 24))
## 
## timePlot <- ggplot(timeView, aes(Experiment, EndTime, group = Experiment)) +
##     geom_boxplot()
##     
## 
#resDF <- tibble(SAT = satList, GRAD = gradList, Ao = AoList,
#                    Run = rList, Experiment = xpList)
#resDF$Seed <- rseed

endtime <- Sys.time()
print(endtime - starttime)


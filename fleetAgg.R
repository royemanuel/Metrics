#source("fleetRes.R")

#setwd("fleetData/20180630-090621")

starttime <- Sys.time()
timetoGradFilesMstr <- list.files(path = ".", pattern = "aircrew")
skedFilesMstr <- list.files(path = ".", pattern = "sked")

######################################################################
## Define different Chi for each requirement

chiSatPre <- c(0)
chiSatPost <- c(0)

chiAoPre <- c(0)
chiAoPost <- c(0)

chiGradPre <- c(1, .5, .25)
chiGradPost <- c(1, 1, .5)

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
## Big for loop to go through the directory where I put the data
resilienceDF <- tibble(SAT = 0, GRAD = 0, Ao = 0,
                       Run = '0', Experiment = '0', Seed = 0)
resilienceDF <- filter(resilienceDF, SAT == 1)
for(rs in 1:length(rseed)){
    print(rs)
    ttgBin <- str_detect(timetoGradFilesMstr, as.character(rseed[rs]))
    sfBin <- str_detect(skedFilesMstr, as.character(rseed[rs]))
    timetoGradFiles <- timetoGradFilesMstr[ttgBin]
    skedFiles <- skedFilesMstr[sfBin]
    mstrList = list()
    for (ttg in 1:length(timetoGradFiles)){
        DFrun <- read_csv(timetoGradFiles[ttg],
                          col_types = list(col_integer(),
                                           col_character(),
                                           col_character(),
                                           col_character(),
                                           col_character(),
                                           col_character()))
        satVal <- satRes(DFrun, .85, 1200)
        sked <- read_csv(skedFiles[ttg])
        AoVal <- AoRes(sked, 0.85)
        gradVal <- gradRes(sked, 65, chiGradPre, chiGradPost)
        exprmnt <-
            skedFiles[ttg] %>%
            str_extract("(?<=Exp?)\\d+")
        xpVal <- exprmnt
        run <-
            skedFiles[ttg] %>%
            str_extract("(?<=Run?)\\d+")
        rVal <- run
        sked <-
            sked %>%
            mutate(Run = run,
                   Experiment = exprmnt,
                   Seed = rseed[rs])
        mstrList[[ttg]] <- sked
        resilienceDF <- add_row(resilienceDF,
                                SAT = satVal,
                                GRAD = gradVal,
                                Ao = AoVal,
                                Run = rVal,
                                Experiment = xpVal,
                                Seed = rs)
        print(paste("From Seed", rseed[rs],
                    "added Experiment", exprmnt,
                    "Run", run))
    }
    print(paste("Seed", rseed[rs]))
}

mstrSked <- bind_rows(mstrList)


##countFiles <- function(lst, seedlst){
##    for(1 in 1:length(seedlst)){
##        strp <- str_detect(lst, as.character(seedlst[rs]))
##    }
##}

## Get rid of the "seed" from resilience DF and do a proper count of runs

runCheck <- function(DF){
    wDF <-
        DF %>%
        group_by(Experiment) %>%
        mutate(Run = as.integer(Run)) %>%
        summarise(MAXRUN = max(Run))
    k <- wDF$MAXRUN != max(wDF$MAXRUN)
    if(sum(k) == 0){
        DF <-
            DF %>%
            mutate(Run = as.integer(Run),
                   Run = (Run + 1) + (max(Run + 1)) * (Seed - 1),
                   Run = as.character(Run)) %>%
            select(-Seed)
        return(DF)
    } else {
        print("not the same number of runs")
    }
}

workRDF <- runCheck(resilienceDF)


acView <-
    mstrSked %>%
    select(Time, SLEPlist, boneYard, flightLine, upAircraft, Run, Experiment) %>%
    gather(Category, Count, -Time, -Run, -Experiment) %>%
    mutate(Time = Time / (24 * 365))

studView <- 
    mstrSked %>%
    select(Time, SLEPlist, boneYard, flightLine,
           students, upAircraft, Run, Experiment) %>%
    gather(Category, Count, -Time, -Run, -Experiment) %>%
    mutate(Time = Time / (24 * 365))

acVplot <-
    ggplot(acView, aes(Time, Count, group = Category, color = Category)) +
    geom_line() +
    facet_grid(Experiment ~ Run)

studVplot <- 
    ggplot(studView, aes(Time, Count, group = Category, color = Category)) +
    geom_line() +
    facet_grid(Experiment ~ Run)

timeView <-
    mstrSked %>%
    group_by(Experiment, Run) %>%
    summarise(EndTime = max(Time) / (365 * 24))

timePlot <- ggplot(timeView, aes(Experiment, EndTime, group = Experiment)) +
    geom_boxplot()
    

#resDF <- tibble(SAT = satList, GRAD = gradList, Ao = AoList,
#                    Run = rList, Experiment = xpList)
#resDF$Seed <- rseed

endtime <- Sys.time()
print(endtime - starttime)


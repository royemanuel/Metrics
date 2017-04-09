## The data pulls from the big Austin model runs

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
write.csv(bigInfRFR, "bigInfrastructureRunsForRecord.csv")

## Pull out only the resilience metrics, Infrastructure and scenario
bigInfResilience <- select(bigInfRFR, QR, EQR, Rho, extRho,
                           statQuoResilience, extResilience,
                           Infrastructure, Scenario)

write.csv(bigInfResilience, "bigInfrastructureResilience.csv")

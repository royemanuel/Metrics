######################################################################
## Uncertainty in values                                            ##
######################################################################
library("ExtDist")
library("truncdist")
## Build a need that is a normal distribution about 1
cUnk <- data.frame(func = "constantNeed",
                   cLevel = runif(100, min=.8, max=1.1),
                   startTime = NA,
                   slope = NA)

library("dplyr")

bCalculator <- function(inputs){
    bVals <- inputs %>% mutate(b = -(1/T.rec) *
                                   log((1-closeEnough) *
                                           hRecLevel /
                                               (hRecLevel - hFLevel)))
    return(bVals)
}

hurrCat <- c("1", "1" , "2", "2", "3", "3", "4 & 5", "4 & 5")
hurrProb <- c(0.54, 0.26, 0.14, 0.06)
hurrRecLevel <- c(1, 1, 1, 1, .9, .9, .8, .8)
hurrRecTime <- c(3, 5, 7, 21, 14, 35, 28, 100)
hFLevel <- c(.9, .9, .6, .6, 0, 0, 0, 0)
hurricane <- data.frame(Cat = hurrCat,
                       T.rec = hurrRecTime,
                       hRecLevel = hurrRecLevel,
                        hFLevel = hFLevel,
                        closeEnough = .99)

hurricane <- bCalculator(hurricane)

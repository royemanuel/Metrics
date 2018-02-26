library("dplyr")
library('xlsx')
library('triangle')
set.seed(58425)

prob_hurricane_year <- .25

## Lambda using minutes as the basis (1440 / day)
hurr_lambda <- -log(1 - prob_hurricane_year)/(365 * 1440)

a <- rexp(1000, hurr_lambda)
b <- rexp(1000, hurr_lambda)
c <- rexp(1000, hurr_lambda)
d <- rexp(1000, hurr_lambda)
f <- rexp(1000, hurr_lambda)
g <- rexp(1000, hurr_lambda)

## s1 <- runif(1000)
## s2 <- runif(1000)
## s3 <- runif(1000)
## s4 <- runif(1000)
## s5 <- runif(1000)

stormsInYear <- data.frame()
stormsInYear <- data.frame(Storm1 = a,
                           Storm2 = a + b,
                           Storm3 = a + b + c,
                           Storm4 = a + b + c + d,
                           Storm5 = a + b + c + d + f,
                           Storm6 = a + b + c + d + f + g)
mystorms <-
    stormsInYear %>%
    filter(Storm1 < 525600 * 2) %>%
    as.tibble()

######################################################################
## This is all old. I calculate all these things in Anylogic now    ##
######################################################################
##                            S1.Strength = s1,
##                            S2.Strength = s2,
##                            S3.Strength = s3,
##                            S4.Strength = s4,
##                            S5.Strength = s5,
##                            S1.RL= NA,
##                            S2.RL= NA,
##                            S3.RL= NA,
##                            S4.RL= NA,
##                            S5.RL= NA,
##                            S1.RT= NA,
##                            S2.RT= NA,
##                            S3.RT= NA,
##                            S4.RT= NA,
##                            S5.RT= NA,
##                            S1.FL= NA,
##                            S2.FL= NA,
##                            S3.FL= NA,
##                            S4.FL= NA,
##                            S5.FL= NA
##                            )

## stormsInYear[stormsInYear > 2 * 525600] <- NA


#storms2 <- stormsInYear %>% filter(Storm1 < 525600 * 5)

## Assign a storm strength

## eFL <- function(v){
##     if(v == 1){
##         fl <- rtriangle(1, 0, .9, .8)
##     } else if (v ==2) {
##         fl <- rtriangle(1, 0, .8, .6)
##     } else if (v == 4){
##         fl <- rtriangle(1, 0, .5, .2)
##     } else {
##         fl <- 0
##     }
## }



## storms2 <- storms2 %>% mutate(S1.Strength = ifelse(S1.Strength < .54, 1,
##                              ifelse(S1.Strength < .8, 2,
##                                     ifelse(S1.Strength < .94, 3, 4)))
##                             ) %>%
##                      mutate(S2.Strength = ifelse(S2.Strength < .54, 1,
##                              ifelse(S2.Strength < .8, 2,
##                                     ifelse(S2.Strength < .94, 3, 4)))
##                             ) %>%
##                      mutate(S3.Strength = ifelse(S3.Strength < .54, 1,
##                              ifelse(S3.Strength < .8, 2,
##                                     ifelse(S3.Strength < .94, 3, 4)))
##                             ) %>%
##                      mutate(S4.Strength = ifelse(S4.Strength < .54, 1,
##                              ifelse(S4.Strength < .8, 2,
##                                     ifelse(S4.Strength < .94, 3, 4)))
##                             ) %>%
##                      mutate(S5.Strength = ifelse(S5.Strength < .54, 1,
##                              ifelse(S5.Strength < .8, 2,
##                                     ifelse(S5.Strength < .94, 3, 4))))

##     mutate(S1.RT = recode(S1.Strength, `1`= 1440 * rtriangle(1, 3, 5, 4),
##                           `2` = 
##                           `3` = 
##                           `4` = 
## Assign a recovery level


## for (c in 6:10){
##     print(c)
##     for (r in 1:dim(storms)[1]){
##         if (storms[r, c] == 1){
##             storms[r, (c + 5) ] = rtriangle(1, .91, 1, 1)
##             storms[r, (c + 10) ] =  1440 * rtriangle(1, 3, 5, 4)
##             storms[r, (c + 15) ] = rtriangle(1, .5, .9, .8)
##         } else if (storms [r, c] == 2){
##             storms[r, (c + 5) ] =  rtriangle(1, .81, 1, 1)
##             storms[r, (c + 10) ] = 1440 * rtriangle(1, 7, 21, 14)
##             storms[r, (c + 15) ] = rtriangle(1, 0, .8, .4)
##         } else if (storms[r, c] == 3){
##             storms[r, (c + 5) ] =  rtriangle(1, .6, 1, .95)
##             storms[r, (c + 10) ] = 1440 * rtriangle(1, 14, 35, 25)
##             storms[r, (c + 15) ] = rtriangle(1, 0, .4, .1)
##         } else {
##             storms[r, (c + 5) ] =  rtriangle(1, .6, .9, .8)
##             storms[r, (c + 10) ] = 1440 * rtriangle(1, 28, 100, 64)
##             storms[r, (c + 15) ] = 0
##         }
##     }
## }
## 
## storms <- storms %>%
##     mutate(b1 = -(1/S1.RT) * log((0.01 * S1.RL)/(S1.RL - S1.FL))) %>%
##     mutate(b2 = -(1/S2.RT) * log((0.01 * S2.RL)/(S2.RL - S2.FL))) %>%
##     mutate(b3 = -(1/S3.RT) * log((0.01 * S3.RL)/(S3.RL - S3.FL))) %>%
##     mutate(b4 = -(1/S4.RT) * log((0.01 * S4.RL)/(S4.RL - S4.FL))) %>%
##     mutate(b5 = -(1/S5.RT) * log((0.01 * S5.RL)/(S5.RL - S5.FL))) 
## 
#write.xlsx2(storms, file="d:/onedrive/PhD Work/Dissertation/Programming/Metrics/stormsSeed.xlsx")
## Assign a recovery level
## storms <- storms %>%
##     mutate(S1.RL = recode(S1.Strength, `1`= rtriangle(1, .9, 1, 1),
##                           `2` = rtriangle(1, .8, 1, 1),
##                           `3` = rtriangle(1, .6, 1, .95),
##                           `4` = rtriangle(1, .6, .9, .8))) %>%
##     mutate(S2.RL = recode(S2.Strength, `1`= rtriangle(1, .9, 1, 1),
##                           `2` = rtriangle(1, .8, 1, 1),
##                           `3` = rtriangle(1, .6, 1, .95),
##                           `4` = rtriangle(1, .6, .9, .8))) %>%
##     mutate(S3.RL = recode(S3.Strength, `1`= rtriangle(1, .9, 1, 1),
##                           `2` = rtriangle(1, .8, 1, 1),
##                           `3` = rtriangle(1, .6, 1, .95),
##                           `4` = rtriangle(1, .6, .9, .8))) %>%
##     mutate(S4.RL = recode(S4.Strength, `1`= rtriangle(1, .9, 1, 1),
##                           `2` = rtriangle(1, .8, 1, 1),
##                           `3` = rtriangle(1, .6, 1, .95),
##                           `4` = rtriangle(1, .6, .9, .8))) %>%
##     mutate(S5.RL = recode(S5.Strength, `1`= rtriangle(1, .9, 1, 1),
##                           `2` = rtriangle(1, .8, 1, 1),
##                           `3` = rtriangle(1, .6, 1, .95),
##                           `4` = rtriangle(1, .6, .9, .8)))
## 
## ## Assign a recovery level
## storms <- storms %>%
##     mutate(S1.FL = recode(S1.Strength, `1`= rtriangle(1, .5, 1, .9),
##                           `2` = rtriangle(1, 0, .8, .4),
##                           `3` = rtriangle(1, 0, .4, .1),
##                           `4` = 0)) %>%
##     mutate(S2.FL = recode(S2.Strength, `1`= rtriangle(1, .5, 1, .9),
##                           `2` = rtriangle(1, 0, .8, .4),
##                           `3` = rtriangle(1, 0, .4, .1),
##                           `4` = 0)) %>%
##     mutate(S3.FL = recode(S3.Strength, `1`= rtriangle(1, .5, 1, .9),
##                           `2` = rtriangle(1, 0, .8, .4),
##                           `3` = rtriangle(1, 0, .4, .1),
##                           `4` = 0)) %>%
##     mutate(S4.FL = recode(S4.Strength, `1`= rtriangle(1, .5, 1, .9),
##                           `2` = rtriangle(1, 0, .8, .4),
##                           `3` = rtriangle(1, 0, .4, .1),
##                           `4` = 0)) %>%
##     mutate(S5.FL = recode(S5.Strength, `1`= rtriangle(1, .5, 1, .9),
##                           `2` = rtriangle(1, 0, .8, .4),
##                           `3` = rtriangle(1, 0, .4, .1),
##                           `4` = 0))
## storms <- storms %>%
##     mutate(S1.RT = recode(S1.Strength, `1`= 1440 * rtriangle(1, 3, 5, 4),
##                           `2` = 1440 * rtriangle(1, 7, 21, 14),
##                           `3` = 1440 * rtriangle(1, 14, 35, 25),
##                           `4` = 1440 * rtriangle(1, 28, 100, 64))) %>%
##     mutate(S2.RT = recode(S2.Strength, `1`= 1440 * rtriangle(1, 3, 5, 4),
##                           `2` = 1440 * rtriangle(1, 7, 21, 14),
##                           `3` = 1440 * rtriangle(1, 14, 35, 25),
##                           `4` = 1440 * rtriangle(1, 28, 100, 64))) %>%
##     mutate(S3.RT = recode(S3.Strength, `1`= 1440 * rtriangle(1, 3, 5, 4),
##                           `2` = 1440 * rtriangle(1, 7, 21, 14),
##                           `3` = 1440 * rtriangle(1, 14, 35, 25),
##                           `4` = 1440 * rtriangle(1, 28, 100, 64))) %>%
##     mutate(S4.RT = recode(S4.Strength, `1`= 1440 * rtriangle(1, 3, 5, 4),
##                           `2` = 1440 * rtriangle(1, 7, 21, 14),
##                           `3` = 1440 * rtriangle(1, 14, 35, 25),
##                           `4` = 1440 * rtriangle(1, 28, 100, 64))) %>%
##     mutate(S5.RT = recode(S5.Strength, `1`= 1440 * rtriangle(1, 3, 5, 4),
##                           `2` = 1440 * rtriangle(1, 7, 21, 14),
##                           `3` = 1440 * rtriangle(1, 14, 35, 25),
##                           `4` = 1440 * rtriangle(1, 28, 100, 64)))
## 
## 
## storms <- storms %>% mutate(S1.FL = ifelse(S1.Strength == 1,
##                                rtriangle(1, a = 0, b = .9, c = .8),
##                                 ifelse(S1.Strength == 2,
##                                        rtriangle(1, a = 0, b = .8, c = .4),
##                                        ifelse(S1.Strength == 3,
##                                               rtriangle(1, a = 0,
##                                                          b = .4,
##                                                          c = .1),
##                                               0)))
##                             ) %>%
##                      mutate(S2.FL = ifelse(S2.Strength == 1,
##                                rtriangle(1, a = 0, b = .9, c = .8),
##                                 ifelse(S2.Strength == 2,
##                                        rtriangle(1, a = 0, b = .8, c = .4),
##                                        ifelse(S2.Strength == 3,
##                                               rtriangle(1, a = 0,
##                                                          b = .4,
##                                                          c = .1),
##                                               0)))
##                             ) %>%
##                      mutate(S3.FL = ifelse(S3.Strength == 1,
##                                rtriangle(1, a = 0, b = .9, c = .8),
##                                 ifelse(S3.Strength == 2,
##                                        rtriangle(1, a = 0, b = .8, c = .4),
##                                        ifelse(S3.Strength == 3,
##                                               rtriangle(1, a = 0,
##                                                          b = .4,
##                                                          c = .1),
##                                               0)))
##                             ) %>%
##                      mutate(S4.FL = ifelse(S4.Strength == 1,
##                                rtriangle(1, a = 0, b = .9, c = .8),
##                                 ifelse(S4.Strength == 2,
##                                        rtriangle(1, a = 0, b = .8, c = .4),
##                                        ifelse(S4.Strength == 3,
##                                               rtriangle(1, a = 0,
##                                                          b = .4,
##                                                          c = .1),
##                                               0)))
##                             ) %>%
##                      mutate(S5.FL = ifelse(S5.Strength == 1,
##                                rtriangle(1, a = 0, b = .9, c = .8),
##                                 ifelse(S5.Strength == 2,
##                                        rtriangle(1, a = 0, b = .8, c = .4),
##                                        ifelse(S5.Strength == 3,
##                                               rtriangle(1, a = 0,
##                                                          b = .4,
##                                                          c = .1),
##                                               0)))
##                             )

## Assign a recovery time
## storms <- storms %>% mutate(S1.RT = ifelse(S1.Strength == 1,
##                                rl,
##                              ifelse(S1.Strength == 2, 2,
##                                     ifelse(S1.Strength == 3, 3, 4)))
##                             ) %>%
##                      mutate(S2.RT = ifelse(S2.Strength == 1, 1,
##                              ifelse(S2.Strength == 2, 2,
##                                     ifelse(S2.Strength == 3, 3, 4)))
##                             ) %>%
##                      mutate(S3.RT = ifelse(S3.Strength == 1, 1,
##                              ifelse(S3.Strength == 2, 2,
##                                     ifelse(S3.Strength == 3, 3, 4)))
##                             ) %>%
##                      mutate(S4.RT = ifelse(S4.Strength == 1, 1,
##                              ifelse(S4.Strength == 2, 2,
##                                     ifelse(S4.Strength == 3, 3, 4)))
##                             ) %>%
##                      mutate(S5.RT = ifelse(S5.Strength == 1, 1,
##                              ifelse(S5.Strength == 2, 2,
##                                     ifelse(S5.Strength == 3, 3, 4))))
## 
## 
## 
## 
## 

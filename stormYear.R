library("dplyr")
library('xlsx')
library('triangle')
a <- rexp(1000, 0.00000027)

b <- rexp(1000, 0.00000027)
c <- rexp(1000, 0.00000027)
d <- rexp(1000, 0.00000027)
f <- rexp(1000, 0.00000027)

s1 <- runif(1000)
s2 <- runif(1000)
s3 <- runif(1000)
s4 <- runif(1000)
s5 <- runif(1000)

stormsInYear <- data.frame(Storm1 = a,
                           Storm2 = a + b,
                           Storm3 = a + b + c,
                           Storm4 = a + b + c + d,
                           Storm5 = a + b + c + d + f,
                           S1.Strength = s1,
                           S2.Strength = s2,
                           S3.Strength = s3,
                           S4.Strength = s4,
                           S5.Strength = s5
                           )

## stormsInYear[stormsInYear > 2 * 525600] <- NA

storms <- stormsInYear %>% filter(Storm1 < 525600 * 2)

## Assign a storm strength

eFL <- function(v){
    if(v == 1){
        fl <- rtriangle(1, 0, .9, .8)
    } else if (v ==2) {
        fl <- rtriangle(1, 0, .8, .6)
    } else if (v == 4){
        fl <- rtriangle(1, 0, .5, .2)
    } else {
        fl <- 0
    }
}

storms <- storms %>% mutate(S1.FL = recode(S1.Strength, 1, rtriangle(1, .5, 1, .9)
storms <- storms %>% mutate(S1.Strength = ifelse(S1.Strength < .54, 1,
                             ifelse(S1.Strength < .8, 2,
                                    ifelse(S1.Strength < .94, 3, 4)))
                            ) %>%
                     mutate(S2.Strength = ifelse(S2.Strength < .54, 1,
                             ifelse(S2.Strength < .8, 2,
                                    ifelse(S2.Strength < .94, 3, 4)))
                            ) %>%
                     mutate(S3.Strength = ifelse(S3.Strength < .54, 1,
                             ifelse(S3.Strength < .8, 2,
                                    ifelse(S3.Strength < .94, 3, 4)))
                            ) %>%
                     mutate(S4.Strength = ifelse(S4.Strength < .54, 1,
                             ifelse(S4.Strength < .8, 2,
                                    ifelse(S4.Strength < .94, 3, 4)))
                            ) %>%
                     mutate(S5.Strength = ifelse(S5.Strength < .54, 1,
                             ifelse(S5.Strength < .8, 2,
                                    ifelse(S5.Strength < .94, 3, 4))))
## Assign a failure level
storms <- storms %>% mutate(S1.FL = ifelse(S1.Strength == 1,
                               rtriangle(1, a = 0, b = .9, c = .8),
                                ifelse(S1.Strength == 2,
                                       rtriangle(1, a = 0, b = .8, c = .4),
                                       ifelse(S1.Strength == 3,
                                              rtriangle(1, a = 0,
                                                         b = .4,
                                                         c = .1),
                                              0)))
                            ) %>%
                     mutate(S2.FL = ifelse(S2.Strength == 1,
                               rtriangle(1, a = 0, b = .9, c = .8),
                                ifelse(S2.Strength == 2,
                                       rtriangle(1, a = 0, b = .8, c = .4),
                                       ifelse(S2.Strength == 3,
                                              rtriangle(1, a = 0,
                                                         b = .4,
                                                         c = .1),
                                              0)))
                            ) %>%
                     mutate(S3.FL = ifelse(S3.Strength == 1,
                               rtriangle(1, a = 0, b = .9, c = .8),
                                ifelse(S3.Strength == 2,
                                       rtriangle(1, a = 0, b = .8, c = .4),
                                       ifelse(S3.Strength == 3,
                                              rtriangle(1, a = 0,
                                                         b = .4,
                                                         c = .1),
                                              0)))
                            ) %>%
                     mutate(S4.FL = ifelse(S4.Strength == 1,
                               rtriangle(1, a = 0, b = .9, c = .8),
                                ifelse(S4.Strength == 2,
                                       rtriangle(1, a = 0, b = .8, c = .4),
                                       ifelse(S4.Strength == 3,
                                              rtriangle(1, a = 0,
                                                         b = .4,
                                                         c = .1),
                                              0)))
                            ) %>%
                     mutate(S5.FL = ifelse(S5.Strength == 1,
                               rtriangle(1, a = 0, b = .9, c = .8),
                                ifelse(S5.Strength == 2,
                                       rtriangle(1, a = 0, b = .8, c = .4),
                                       ifelse(S5.Strength == 3,
                                              rtriangle(1, a = 0,
                                                         b = .4,
                                                         c = .1),
                                              0)))
                            )

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
## write.xlsx2(storms, file="d:/onedrive/PhD Work/Dissertation/Programming/Metrics/storms.xlsx")
## 
## 

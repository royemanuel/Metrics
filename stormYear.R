library("dplyr")

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





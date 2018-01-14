library("dplyr")

a <- rexp(1000, 0.00000027)

b <- rexp(1000, 0.00000027)
c <- rexp(1000, 0.00000027)
d <- rexp(1000, 0.00000027)
f <- rexp(1000, 0.00000027)

stormsInYear <- data.frame(First.Storm = a,
                           Second.Storm = a + b,
                           Third.Storm = a + b + c,
                           Fourth.Storm = a + b + c + d,
                           Fifth.Storm = a + b + c + d + f)

stormsInYear[stormsInYear > 2 * 525600] <- NA

storms <- stormsInYear %>% filter(!is.na(First.Storm))

set.seed(58425)

prob_hurricane_year <- .25

## Lambda using minutes as the basis (1440 / day)
hurr_lambda <- -log(1 - prob_hurricane_year)/(365 * 1440)

a   <- rexp(100, hurr_lambda)
a1  <- rexp(100, hurr_lambda)
a2  <- rexp(100, hurr_lambda)
a3  <- rexp(100, hurr_lambda)
a4  <- rexp(100, hurr_lambda)
a5  <- rexp(100, hurr_lambda)
a6  <- rexp(100, hurr_lambda)
a7  <- rexp(100, hurr_lambda)
a8  <- rexp(100, hurr_lambda)
a9  <- rexp(100, hurr_lambda)
a10 <- rexp(100, hurr_lambda)
a11 <- rexp(100, hurr_lambda)
a12 <- rexp(100, hurr_lambda)
a13 <- rexp(100, hurr_lambda)
a14 <- rexp(100, hurr_lambda)
a15 <- rexp(100, hurr_lambda)
a16 <- rexp(100, hurr_lambda)
a17 <- rexp(100, hurr_lambda)
b   <- rexp(200, hurr_lambda)
b1  <- rexp(200, hurr_lambda)
b2  <- rexp(200, hurr_lambda)
b3  <- rexp(200, hurr_lambda)
b4  <- rexp(200, hurr_lambda)
b5  <- rexp(200, hurr_lambda)
b6  <- rexp(200, hurr_lambda)
b7  <- rexp(200, hurr_lambda)
b8  <- rexp(200, hurr_lambda)
b9  <- rexp(200, hurr_lambda)
b10 <- rexp(200, hurr_lambda)
b11 <- rexp(200, hurr_lambda)
b12 <- rexp(200, hurr_lambda)
b13 <- rexp(200, hurr_lambda)
b14 <- rexp(200, hurr_lambda)
b15 <- rexp(200, hurr_lambda)
b16 <- rexp(200, hurr_lambda)
b17 <- rexp(200, hurr_lambda)





stormsIn20Year <- tibble(Storm1 = a,
                           Storm2 = a + a1,
                           Storm3 = a + a1 + a2,
                           Storm4 = a + a1 + a2 + a3,
                           Storm5 = a + a1 + a2 + a3 + a4,
                         Storm6 = a + a1 + a2 + a3 + a4 + a5)
bstorms <- tibble(Storm1 = b)

bstorms <-
    bstorms %>%
    mutate(Storm2  = Storm1  + b1,
           Storm3  = Storm2  + b2,
           Storm4  = Storm3  + b3,
           Storm5  = Storm4  + b4,
           Storm6  = Storm5  + b5,
           Storm7  = Storm6  + b6,
           Storm8  = Storm7  + b7,
           Storm9  = Storm8  + b8,
           Storm10 = Storm9  + b9,
           Storm11 = Storm10 + b10,
           Storm12 = Storm11 + b11,
           Storm13 = Storm12 + b12,
           Storm14 = Storm13 + b13,
           Storm15 = Storm14 + b14,
           Storm16 = Storm15 + b15,
           Storm17 = Storm16 + b16
           )

stormsIn20Year <- bind_rows(stormsIn20Year,
                            bstorms)
    

stormsIn20Year <-
    stormsIn20Year %>%
    mutate(Storm7  = Storm6 +  a6,
           Storm8  = Storm7 +  a7,
           Storm9  = Storm8 +  a8,
           Storm10 = Storm9 +  a9,
           Storm11 = Storm10 + a10,
           Storm12 = Storm11 + a11,
           Storm13 = Storm12 + a12,
           Storm14 = Storm13 + a13,
           Storm15 = Storm14 + a14,
           Storm16 = Storm15 + a15,
           Storm17 = Storm16 + a16,
           )
my10storms <-
    stormsIn20Year %>%
    filter(Storm1 < 525600 * 10) 

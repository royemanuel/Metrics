Stochastic Validation
=====================

The goal of this is to build random profiles for input into the resilience models.

The first steps are to set the random seed and to build the time column for the data frame.

``` r
source("metrics.R")
set.seed(123)
beginTime <- 0
lastTime <- 100
res <- 1

baseTime <- data.frame(endTime = lastTime, resolution = res)
```

Random failure times
--------------------

The first example will be a randomized time to failure with a constant time to recover and levels of recovery. This needs to be done carefully. If the TTF is greater than the total time, it should be a constant performance. I made changes to the metrics file to cover for this. For now we are assuming an exponential function.

``` r
TTF <- ceiling(rexp(n=10, rate = 1/30))

TTFdf <- data.frame(failTime = TTF,
                    recTime = TTF + 40,
                    preLevel = 1,
                    failLevel = 0.1,
                    recLevel = 1.2
                    ) %>%
                        mutate(func = "step")
```

TTFdf is a data.frame that my resLoop function uses to define the performance vector. Now, let's run resLoop with some dummy variables for Need and resilience variables.

``` r
r <- data.frame(tDelta = 30,
                decay = 0,
                sigma = 0)
needConstant <- data.frame(func = "constantNeed",
                cLevel = 0.8,
                startTime = NA,
                 slope = NA)

TTFresilience <- resLoop(time = baseTime,
                            need = needConstant,
                            performance = TTFdf,
                            resFactors = r)
```

    ## [1] "NR = 1, PR = 1, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 2, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 3, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 4, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 5, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 6, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 7, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 8, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 9, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 10, RR = 1, TR = 1"

``` r
dim(TTFresilience)
```

    ## [1] 1010   17

``` r
pltData <- randomResilience %>%
    filter(Time == 80) %>%
        select(QR, EQR, TQR, ETQR, Rho, extRho, statQuoResilience,
               extResilience, pRun)
pltData <- melt(pltData, id.vars = "pRun")
tPlot <- ggplot(pltData, aes(variable, value)) + geom_point()
tPlot
```

![](buildRandPerf_files/figure-markdown_github/unnamed-chunk-3-1.png)

Random recovery times and random failure times
----------------------------------------------

Now let's build a data.frame with a random recovery time. This will be a lognormal recovery time with a mean of 20 and vary about 5 from it $ (t) $ test for inline rendering of mathcode

``` r
TTR <- data.frame(nums = ceiling(rlnorm(10, meanlog = log(1600/sqrt(1700)), sdlog = log(1700/1600))))
TTRF <- TTFdf %>% mutate(recTime = TTF + TTR$nums)
print(TTRF)
```

    ##    failTime recTime preLevel failLevel recLevel func
    ## 1        26      64        1       0.1      1.2 step
    ## 2        18      60        1       0.1      1.2 step
    ## 3        40      80        1       0.1      1.2 step
    ## 4         1      41        1       0.1      1.2 step
    ## 5         2      42        1       0.1      1.2 step
    ## 6        10      48        1       0.1      1.2 step
    ## 7        10      54        1       0.1      1.2 step
    ## 8         5      45        1       0.1      1.2 step
    ## 9        82     117        1       0.1      1.2 step
    ## 10        1      42        1       0.1      1.2 step

``` r
logPlot <- ggplot(TTR, aes(x=nums)) + geom_histogram(binwidth = 1)

TTRFresilience <- resLoop(time = baseTime,
                            need = needConstant,
                            performance = TTRF,
                            resFactors = r)
```

    ## [1] "NR = 1, PR = 1, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 2, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 3, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 4, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 5, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 6, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 7, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 8, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 9, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 10, RR = 1, TR = 1"

``` r
dim(TTRFresilience)
```

    ## [1] 1010   17

``` r
pltrData <- TTRFresilience %>%
    filter(Time == 80) %>%
        select(QR, EQR, TQR, ETQR, Rho, extRho, statQuoResilience,
               extResilience, pRun)
pltrData <- melt(pltrData, id.vars = "pRun")
rPlot <- ggplot(pltrData, aes(variable, value)) + geom_point()
rPlot
```

![](buildRandPerf_files/figure-markdown_github/unnamed-chunk-4-1.png)

Vary the failure level.
-----------------------

First we use the Beta distribution skewed to zero. Then we replace the failure levels with this value.

``` r
   FL <- data.frame(nums = rbeta(10, shape1 = .5, shape2 = 2))
   betaPlot <- ggplot(FL, aes(x=nums)) + geom_histogram(binwidth = .05)
   TTRFF <- TTRF %>% mutate(failLevel = FL$nums)
   print(perfLevel)
```

    ##    failTime recTime preLevel   failLevel recLevel func
    ## 1        26      66        1 0.534443664      1.2 step
    ## 2        18      58        1 0.334230223      1.2 step
    ## 3        40      80        1 0.015589112      1.2 step
    ## 4         1      41        1 0.132812721      1.2 step
    ## 5         2      42        1 0.003425807      1.2 step
    ## 6        10      50        1 0.059600454      1.2 step
    ## 7        10      50        1 0.391543496      1.2 step
    ## 8         5      45        1 0.012777777      1.2 step
    ## 9        82     122        1 0.016481510      1.2 step
    ## 10        1      41        1 0.025792444      1.2 step

``` r
   TTRFFresilience <- resLoop(time = baseTime,
                            need = needConstant,
                            performance = perfLevel,
                          resFactors = r)
```

    ## [1] "NR = 1, PR = 1, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 2, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 3, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 4, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 5, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 6, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 7, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 8, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 9, RR = 1, TR = 1"
    ## [1] "NR = 1, PR = 10, RR = 1, TR = 1"

``` r
   dim(TTRFFresilience)
```

    ## [1] 1010   17

``` r
   pltbData <- TTRFFresilience %>%
       filter(Time == 80) %>%
           select(QR, EQR, TQR, ETQR, Rho, extRho, statQuoResilience,
                  extResilience, pRun)
   pltbData
```

    ##          QR EQR       TQR ETQR         Rho      extRho statQuoResilience
    ## 1  1.429594   1 0.5778638  0.5 0.641332397 0.668054581         0.8034718
    ## 2  1.300404   1 0.5844887  0.5 0.401076268 0.417787779         0.7233651
    ## 3  1.203167   1 0.5012698  0.5 0.018706935 0.019486390         0.5090446
    ## 4  1.230631   1 0.6138739  0.5 0.159375266 0.166015902         0.6651564
    ## 5  1.200688   1 0.5965809  0.5 0.004110968 0.004282258         0.5979629
    ## 6  1.212676   1 0.5810826  0.5 0.071520545 0.074500567         0.6060502
    ## 7  1.328701   1 0.6253171  0.5 0.469852195 0.489429370         0.7720217
    ## 8  1.202589   1 0.5898987  0.5 0.015333332 0.015972221         0.5951389
    ## 9  1.000000   1 1.0000000  1.0 1.000000000 1.000000000         1.0000000
    ## 10 1.205295   1 0.6013644  0.5 0.030950932 0.032240555         0.6116462
    ##    extResilience pRun
    ## 1      0.8376644    1
    ## 2      0.7135814    2
    ## 3      0.5144307    3
    ## 4      0.5876955    4
    ## 5      0.5068286    5
    ## 6      0.5419378    6
    ## 7      0.7494022    7
    ## 8      0.5126736    8
    ## 9      1.0000000    9
    ## 10     0.5208078   10

``` r
   pltbData <- melt(pltbData, id.vars = "pRun")
   bPlot <- ggplot(pltbData, aes(variable, value)) + geom_point()
   bPlot
```

![](buildRandPerf_files/figure-markdown_github/unnamed-chunk-5-1.png)

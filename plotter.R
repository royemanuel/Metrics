## File intended to work through building plots for the dissertation
## Plan is to use ggplot2. Reshaping the data will be done here too
library('ggplot2')
library('reshape2')

## 
timeHorPlot <- function(tt){
    ## build a melted data frame with only the resilience results
    ttRes <- tt %>%
        select(-Performance, -Need, -npRatio) %>%
            melt(id = c("Run", "Time"))
    ## build a melted data frame with only the performance and need data
    ttP <- tt %>%
        select(Performance, Time, Run) %>%
            melt(id = c("Run", "Time")) %>%
                mutate(var2 = variable) %>%
                    select(-variable)
    ttN <- tt %>%
        select(Need, Time, Run) %>%
            melt(id = c("Run", "Time")) %>%
                mutate(var2 = variable) %>%
                    select(-variable)
    thPlot <- ggplot(ttRes, aes(Time, value)) + geom_line() +
        geom_line(data=ttP,
                  aes(x=Time, y=value, group=var2), color="green") +
                      geom_line(data=ttN,
                                aes(x=Time, y=value, group=var2),
                                color="red") +
            facet_wrap(~variable, nrow = 3)
}

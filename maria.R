## Build the tibble for Maria and Puerto Rico.

library("readxl")
source("bCalc.R")

maria <- read_excel("Maria.xlsx")

mariaStats <- tibble(Cat = 4, T.rec = 196.3076, hRecLevel = .958, hFLevel = 0, closeEnough = .99)

mariaStats <- bCalculator(mariaStats)

maria <-
    maria %>%
    mutate(Model = 0.958 - (0.958 - 0)*exp(-mariaStats$b * (as.double(Date - Date[11])/(60*1440))))

maria <-
    maria %>%
    gather(Recovery_Measure, Recovery_Quantity, -Date, -CustwithPower)



maria_plot <-
    ggplot(data = maria, aes(x = Date,
                             y = Recovery_Quantity,
                             group = Recovery_Measure,
                             linetype = Recovery_Measure)) +
    geom_line() +
    theme_bw()

## Work with the data generated by HurrSingleStorm.R
library("tidyverse")
## rising_need_data <- read_csv("studyData/singlestormResults/risingNeed.csv") %>%
##     select(-X1) %>%
##     mutate(Strongest_Storm = as.factor(Strongest_Storm))
## sq_data <- read_csv("studyData/singlestormResults/statusQuo.csv") %>%
##     select(-X1)%>%
##     mutate(Strongest_Storm = as.factor(Strongest_Storm))
## all_ss_data <- read_csv("studyData/singlestormResults/allData.csv") %>%
##     select(-X1) %>%
##     mutate(Strongest_Storm = as.factor(Strongest_Storm))
## 
## 
## one_run <- read_csv("studyData/singlestormResults/one_run.csv") %>%
##     select(-X1)%>%
##     mutate(Strongest_Storm = as.factor(Strongest_Storm))
##

ts_data <- sf_data_testneed2yr %>%# read_csv("studyData/singlestormResults/perfNeedtrajectories.csv") %>%
    #select(-X1) %>%
    gather(Profile, Performance, -Run, -Time, -Infrastructure) %>%
    mutate(Days = Time / 1440,
           Infrastructure = fix_infrastructure(Infrastructure))
    
######################################################################
## Maria actual numbers
######################################################################
maria <- read_excel("Maria.xlsx")

mariaStats <- tibble(Cat = 4, T.rec = 196.3076, hRecLevel = .958, hFLevel = 0, closeEnough = .99)

mariaStats <- bCalculator(mariaStats)

maria <-
    maria %>%
    mutate(Model = 0.9 - (0.9 - 0)*exp(-mariaStats$b * (as.double(Date - Date[11])/(60*1440))))


maria <-
    maria %>%
    gather(Recovery_Measure, Recovery_Quantity, -Date, -CustwithPower) %>%
    mutate(Recovery_Quantity = ifelse(Recovery_Quantity < 0, 0, Recovery_Quantity))
######################################################################
## use the Maria peak load data directly
maria <-
    maria %>%
    mutate(lagDate = lag(Date, 1))
maria$lagDate[1] <- maria$Date[1]
maria_peak_power <-
    maria %>%
    mutate(Time = (Date[1] %--% Date)/ dminutes(1)) %>%
    mutate(Infrastructure = Recovery_Measure,
           Performance = Recovery_Quantity,
           Need = 1,
           Run = 1,
           Grp = 1) %>%
    select(Time, Infrastructure, Performance, Need, Grp, Run)

maria_resilience <- calc_EIR(maria_peak_power, 0)

maria_data_point <- tibble(Name = "Hurricane Maria",
                           Infrastructure = "Electricity",
                           Resilience = as.double(maria_resilience[3,3]),
                           Strongest_Storm = as.factor("4+"))
## ts_th <-
##     ts_data %>%
##     group_by(Need_Profile) %>%
##     summarise(Time_Horizon = max(Time))

## plot with columns of status quo and stakeholder input, rows
## of storm strength.

single_storm_resilience <-
    read_csv("studyData/singlestormResults/risingNeedtimeHorIsRecTime.csv") %>%
    select(-X1) %>%
    mutate(Infrastructure = fix_infrastructure(Infrastructure),
           Resilience = ExtendedIntegralResilience,
           Strongest_Storm = as.character(Strongest_Storm),
           Strongest_Storm = ifelse(Strongest_Storm == "4",
                                    "4+",
                                    Strongest_Storm),
           Strongest_Storm = as.factor(Strongest_Storm))

ssr <- single_storm_resilience %>%
    group_by(Infrastructure, Strongest_Storm) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

ssr <- inner_join(single_storm_resilience, ssr)

single_storm_by_strength <-
    ggplot(ssr,
           aes(x = Infrastructure,
               y = Resilience,
               fill = Strongest_Storm)) +
    theme_bw() +
    geom_boxplot(position="dodge",
                 aes(ymin = minY,
                     ymax = maxY,
                     lower = lowerY,
                     middle = middleY,
                     upper = upsinblperY),
                     stat = "identity") +
    scale_colour_manual(c("grey90", "grey70", "grey50", "grey30")) +
    theme(axis.text.x = element_text(angle = -45,
                                        vjust = 1,
                                     hjust = 0),
          legend.position = c(.2, .2)) +
    scale_fill_grey(start = 0, end = .9) +
    ylim(c(0, 1.05)) +
    guides(fill=guide_legend(title = "Hurricane Category")) +
    geom_point(data = maria_data_point,
               aes(x = Infrastructure,
                   y = Resilience,
                   size = 4)) +
    geom_text(data = maria_data_point,
              aes(x=Infrastructure,
                  y=Resilience,
                  label = Name,
                  hjust = -0.1)) +
    guides(size = FALSE,
           alpha = FALSE)


ssi <- single_storm_resilience %>%
    group_by(Infrastructure) %>%
    summarise(minY = min(Resilience),
              maxY = max(Resilience),
              lowerY = quantile(Resilience, 0.25),
              middleY = median(Resilience),
              upperY = quantile(Resilience, 0.75))

ssi <- inner_join(single_storm_resilience, ssi)

single_storm_by_inf <- ggplot(ssi) +
    geom_boxplot(mapping = aes(Infrastructure, Resilience,
                               ymin = minY,
                               ymax = maxY,
                               lower = lowerY,
                               middle = middleY,
                               upper = upperY),
                 stat = "identity") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45,
                                        vjust = 1,
                                     hjust = 0),
          legend.position = "none") +
    ylim(c(0, 1.05)) +
    geom_point(data = maria_data_point,
               aes(x = Infrastructure,
                   y = Resilience,
                   size = 4)) +
    geom_text(data = maria_data_point,
              aes(x=Infrastructure,
                  y=Resilience,
                  label = Name,
                  hjust = -0.1))

SQ_vs_SN_Time <-
    ggplot(one_run, aes(Infrastructure,
                        ExtendedIntegralResilience,
                        fill = Need_Profile)) +
    geom_col(position = position_dodge())


ts_plot <- ggplot(ts_data) +
    geom_line(aes(Days,
                  Performance,
                  group = Profile,
                  linetype = Profile)) +
    scale_linetype_manual(values = c("solid", "dotted")) +
    facet_wrap(~ Infrastructure, ncol = 4) +
    theme_bw() +
    theme(legend.position = c(.1,.9))

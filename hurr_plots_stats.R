######################################################################
## Plots and stats from the hurricane studies
######################################################################

## study_data_res <- read.csv()
## study_data_res <- 

format_plot <- function(plt){
    plt <-
        plt +
        theme_bw(base_size = 12, base_family = "serif") +
        theme(legend.margin=margin(t = 0, unit = 'cm'),
              legend.position = "top",
              legend.title = element_blank()) +
        ylim(0, 1.2)
}


plot_EIR <- function(DF){
    p <- ggplot(DF,
                aes(Infrastructure, Resilience)) +
        geom_boxplot() +
        stat_summary(fun.y = mean, shape = 3, geom="point")
    format_plot(p)
} 

plot_storm_strength <- function(DF){
    DF <-
        DF %>%
        mutate(Infrastructure = fix_infrastructure(Infrastructure),
               Resilience = ExtendedIntegralResilience) %>%
        select(-ExtendedIntegralResilience)
    p <- ggplot(DF,
                aes(Infrastructure, Resilience)) +
        geom_boxplot() +
        facet_grid(Strongest_Storm ~ Number_Storms)
}

example_profiles <- function(DF, run){
    ## This still eneds to have legends put into it
    ## DF needs to be the post-need build, pre-assignment
    working_DF <-
        DF %>%
        filter(Run == run) %>%
        select(Time, Performance, Need, Infrastructure) %>%
        mutate(Infrastructure = fix_infrastructure(Infrastructure)) %>%
        mutate(Time = Time / (365 *1440))
    p <-
        ggplot(working_DF,
               aes(Time,
                   Performance,
                   group = Infrastructure)) +
        geom_line(linetype = "solid") + 
        geom_line(aes(Time, Need), linetype="dashed") +
        facet_wrap(~Infrastructure, ncol = 4)
    p <- p +
        ylim(0,2) +
        theme_bw(base_size = 12,
                 base_family = "serif") +
        theme(legend.position = "right")
}

fix_infrastructure <- function(listinf){
    vec_inf <- listinf %>%
        str_remove("(.)Function") %>%
        str_remove("ality") %>%
        str_replace("ity_", "ity")%>%
        str_remove("Availability") %>%
        str_replace("_", "\n") %>%
        str_replace("IT", "Information\nTechnology")
}

abbrev_inf <- function(listinf){
    vec_inf <- listinf %>%
        str_replace("Communications", "Com") %>%
        str_replace("Information\nTechnology", "IT") %>%
        str_replace("Critical\nManufacturing", "Crit\nMnfg") %>%
        str_replace("Electricity", "Elec") %>%
        str_replace("Emergency\nServices", "Emrg\nSrv") %>%
        str_replace("Healthcare", "H") %>%
        str_replace("Transportation", "T") %>%
        str_replace("Water", "W")
}

## Just an example plot to figure out
## EA <- ggplot(filter(sf_data_SQ,
##                     Infrastructure == "Healthcare_Function" &
##                     Run == 1000),
##              aes(Time, Performance)) + geom_line() + geom_line(aes(Time, Need))
## 
## 
## EA2 <- ggplot(filter(sf_data_testneed2yr,
##                      Infrastructure == "Healthcare_Function" &
##                      Run == 1000),
##               aes(Time, Performance)) + geom_line() + geom_line(aes(Time, Need))

table_summary <- function(tbl){
    tbl <-
        tbl %>%
        mutate(TimeHorizon = as.factor(TimeHorizon)) %>%
        group_by(Infrastructure, TimeHorizon) %>%
        summarise(avg = mean(ExtendedIntegralResilience),
                  min = min(ExtendedIntegralResilience),
                  max = max(ExtendedIntegralResilience),
                  nostorm = sum(is.na(Number_Storms)))
}

fix_inf_table <- function(inf_vec){
    inf_vec <-
        inf_vec %>%
        str_remove("(.)Function") %>%
        str_remove("ality") %>%
        str_remove(" Availability") %>%
        str_replace("_", " ") %>%
        str_replace("IT", "Information Technology")
}

## want to edit the names for something more useful


summary_stats<- function(DF){
    DF <-
        DF %>%
        ungroup() %>%
        mutate(Infrastructure = fix_inf_table(Infrastructure)) %>%
        group_by(Infrastructure, TimeHorizon) %>%
                                        #select(-X) %>%
        summarise(Max    = round(max(ExtendedIntegralResilience), 3),
                  Min    = round(min(ExtendedIntegralResilience), 3),
                  btm    = round(quantile(ExtendedIntegralResilience, probs = 0.25), 3),
                  median = round(median(ExtendedIntegralResilience),3),
                  top    = round(quantile(ExtendedIntegralResilience, probs = 0.75),3),
                  stdev  = round(sd(ExtendedIntegralResilience), 3),
                  Avg    = round(mean(ExtendedIntegralResilience), 3)) %>%
        mutate(Range = Max - Min)
}


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
    DF <-
        DF %>%
        mutate(Infrastructure = fix_infrastructure(Infrastructure),
               Resilience = ExtendedIntegralResilience) %>%
        select(-ExtendedIntegralResilience)
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
        str_remove("Availability") %>%
        str_replace("_", " ") %>%
        str_replace("IT", "Information Technology")
}

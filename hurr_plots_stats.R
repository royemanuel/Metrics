######################################################################
## Plots and stats from the hurricane studies
######################################################################

study_data <- read.csv()

format_plot <- function(plt){
    plt <-
        plt +
        theme_bw(base_size = 12, base_family = "serif") +
        theme(legend.margin=margin(t = 0, unit = 'cm'),
              legend.position = "top",
              legend.title = element_blank()) +
        ylim(0, 1.2)
}

plot_EIR10yr <-
    ggplot(sf_EIR10yr,
           aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() 

plot_storm_strength <- ggplot(sf_EIR, aes(Infrastructure, ExtendedIntegralResilience)) +
    geom_boxplot() +
    facet_grid(Strongest_Storm ~ Number_Storms)

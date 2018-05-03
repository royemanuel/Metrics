## Build the tibble for Maria and Puerto Rico.
library("tidyverse")
library("lubridate")
library("readxl")
source("metrics.R")
source("HurricaneDataPull.R")
source("hurrNeed.R")
source("bCalc.R")

######################################################################
## Maria actual numbers
######################################################################
maria <- read_excel("Maria.xlsx")

mariaStats <- tibble(Cat = 4,
                     T.rec = 196.3076,
                     hRecLevel = .958,
                     hFLevel = 0,
                     closeEnough = .99)

mariaStats <- bCalculator(mariaStats)

maria <-
    maria %>%
    mutate(Model = 0.9 - (0.9 - 0) *
               exp(-mariaStats$b *
                   (as.double(Date - Date[11])/(60*1440))))

maria <-
    maria %>%
    gather(Recovery_Measure, Recovery_Quantity, -Date, -CustwithPower) %>%
    mutate(Recovery_Quantity = ifelse(Recovery_Quantity < 0,
                                      0,
                                      Recovery_Quantity))

maria_fix_datalabels <- function(src_vec){
    src_vec <-
        src_vec %>%
        str_replace("Cust_Percentage",
                    "Percentage of Customers with Electricity") %>%
        str_replace("Peak_Load", "Percentage of Peak Load Restored") %>%
        str_replace("Model", "System Dynamics Model Results")
}


maria2 <-
    maria %>%
    mutate(Recovery_Measure=maria_fix_datalabels(maria$Recovery_Measure),
           Recovery_Quantity = Recovery_Quantity * 100,
           Date = as.Date(Date))

maria_plot <-
    ggplot(data = maria2, #filter(maria, Recovery_Measure != "Model"),
                         aes(x = Date,
                             y = Recovery_Quantity,
                             group = Recovery_Measure,
                             linetype = Recovery_Measure)) +
    geom_line() +
    scale_x_date(labels = date_format("%b %Y"))+
    theme_bw() +
    theme(legend.position = c(0.75, .25)) +
    labs(y = "Percentage", linetype = "Recovery Measure")

## You need to clean this up, but this will do for now.

##ggsave()

######################################################################
## Maria model numbers
######################################################################

data_directory <- "Maria/"

study_files <- c(paste0(data_directory,"MCoutput.xlsx"))
run_profiles <- c(paste0(data_directory,"runProfile.xlsx"))


rising_need2yr <- tibble(Infrastructure = c("Electricity_Availability",
                                       "Communications_Function",
                                       "IT_Function",
                                       "Healthcare_Function",
                                       "Transportation_Function",
                                       "Emergency_Services_Functionality",
                                       "Critical_Manufacturing_Functionality",
                                       "Water_Functionality"),
                    BL = c(1.0, 1.0, .95, 0.9, 1.05, .9, 1.0, 1.0),
                    Y2 = c(1.0, 1.04, .95, 0.96, 1.09, .94, 1.06, 1.2))

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



######################################################################
## Model of a single storm with Maria-like values
DF_EIR_SS <- tibble()
DF_EIR_SS_SQ <- tibble()
for(d in 1:length(study_files)){
    sf_data <- ingestHurrDataSS(study_files[d])
    sf_data <-
        sf_data %>%
        filter(Time > 0) %>%
        mutate(Time = Time + 7200)
    pre_data <-
        tibble(
            Run = rep(801, 31),
            Time = seq(0, 7200, 240),
            Electricity_Availability = rep(100, 31),
            Communications_Function = rep(100, 31),
            IT_Function = rep(100, 31),
            Healthcare_Function = rep(100, 31),
            Transportation_Function = rep(100, 31),
            Emergency_Services_Functionality = rep(100, 31),
            Critical_Manufacturing_Functionality = rep(100, 31),
            Water_Functionality = rep(100, 31)
        )
    sf_data <- bind_rows(pre_data, sf_data)
    mystorms <- ingest_run_profiles(run_profiles[d])
    cat("\r", "Data file ", d, " ingested from Excel")
    R_EIR <- tibble()
    R_EIR_SQ <- tibble()
    storm_summary <-
        mystorms %>%
        group_by(Run) %>%
        summarise(Strongest_Storm = max(HurricaneStrength),
                  Worst_Failure = min(FailLevel),
                  End_Rec_Level = min(RecoveryLevel),
                  RecoveryTime = RecoveryTime)
    sf_data_clean <-
        sf_data %>%
        filter(Time > 0) %>%
        gather(Infrastructure, Performance, -Run, -Time) %>%
        mutate(Performance = round(Performance / 100, 2))  %>%
        group_by(Run, Infrastructure)
    runs <- unique(sf_data_clean$Run)
    number_runs <- length(runs)
    for(r in 1:number_runs){
        sf_data_run <-
            sf_data_clean %>%
            filter(Run == runs[r])
        storm <-
            mystorms %>%
            filter(Run == runs[r])
        TH <- max(sf_data_run$Time)
        rising_need2yr <-
            rising_need2yr %>%
            mutate(Y2 = BL + (Y2 - BL) * (TH /5256000))
        sf_data_testneed2yr <- bld_need_all_q(DF = sf_data_run,
                                           time_h = TH,
                                           stormlist = storm,
                                           need_inf = rising_need2yr)
        storm_run_TN2yr <-
            sf_data_testneed2yr %>%
            group_by(Run)
        sf_data_groups <- assignGroup_q(sf_data_testneed2yr)
        sf_EIR <- calc_EIR(sf_data_groups, 0)
        sf_EIR <- left_join(sf_EIR, storm_summary, by = "Run")
        R_EIR <- bind_rows(R_EIR, sf_EIR)
        ## Status quo need portion
        sf_data_SQ <- sf_data_run %>%
            mutate(Need = 1)
        storm_run_SQ <-
            sf_data_SQ %>%
            group_by(Run)
        sf_data_groups_SQ <- assignGroup_q(sf_data_SQ)
        sf_EIR_SQ <- calc_EIR(sf_data_groups_SQ, 0)
        sf_EIR_SQ<- left_join(sf_EIR_SQ, storm_summary, by = "Run")
        cat("\r", "Run ", runs[r], " complete")
        R_EIR_SQ <- bind_rows(R_EIR_SQ, sf_EIR_SQ)
        both_EIR <- list(R_EIR, R_EIR_SQ)
    }
    DF_EIR_SS <- bind_rows(DF_EIR_SS, both_EIR[[1]])
    DF_EIR_SS_SQ <- bind_rows(DF_EIR_SS_SQ, both_EIR[[2]])
    all_results <- list(rising_need = DF_EIR_SS, status_quo = DF_EIR_SS_SQ)
}

write.csv(all_results[[1]], "studyData/maria/risingNeed.csv")
write.csv(all_results[[2]], "studyData/maria/statusQuo.csv")


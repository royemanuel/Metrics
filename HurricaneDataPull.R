library("tidyverse")
library("readxl")

## Pull the 15 example sheets
xlfiles <- c("HurricaneDataFixed/MCoutputseed1.xlsx",
             "HurricaneDataFixed/MCoutput15seed2.xlsx")#,
##              "HurricaneDataFixed/MCoutput20seed3.xlsx",
##              "HurricaneDataFixed/MCoutput20seed4.xlsx",
##              "HurricaneDataFixed/MCoutput20seed5.xlsx",
##              "HurricaneDataFixed/MCoutput20seed6.xlsx",
##              "HurricaneDataFixed/MCoutput20seed7.xlsx",
##              "HurricaneDataFixed/MCoutput20seed8.xlsx",
##              "HurricaneDataFixed/MCoutput20seed9.xlsx",
##              "HurricaneDataFixed/MCoutput20seed10.xlsx",
##              "HurricaneDataFixed/MCoutput20seed11.xlsx",
##              "HurricaneDataFixed/MCoutput18seed12.xlsx"
##              )

## test_tibble <-
##     xlfiles %>%
##     excel_sheets() %>%
##     set_names() %>%
##     map(read_excel, path = xlfiles)


cleanHurrData <- function(tbl){
    tbl <- tbl %>% select(-Time)
    column_names <- colnames(tbl)
    column_names[2] <- "Time"
    colnames(tbl) <- column_names
    run <- tbl$Run[1]
    tbl <- mutate(tbl, Run = Run[1])
    column_names <- c("Run", "Time", "Electricity_Availability",
                      "Communications_Function", "IT_Function",
                      "Healthcare_Function", "Transportation_Function",
                      "Emergency_Services_Functionality",
                      "Critical_Manufacturing_Functionality",
                      "Water_Functionality")
    colnames(tbl) <- column_names
    return(tbl)
}

ingestHurrData <- function(file_list){
    all_data <- tibble()
    for(f in 1:length(file_list)){
        print(f)
        sheets <-
            file_list[f] %>%
            excel_sheets() %>%
            set_names() %>%
            map(read_excel, path = file_list[f])
        temp_sheet <- tibble()
        for (s in 1:length(sheets)){
            ## print(s)
            if(dim(sheets[[s]])[1] > 0){
                temp_data <- cleanHurrData(sheets[[s]])
                temp_sheet <- rbind(temp_sheet, temp_data)
            }
        }
        all_data <- rbind(all_data, temp_sheet)
    }
    all_data
}

## calc_resilience <- function(performanceDF, stakeholderDF){
##     working_performance <-
##         performanceDF %>%
##         
## }

## Building demonstration code to be used on the whole shebang
wd <- ingestHurrData(xlfiles)

tidy_working_data <-
    wd %>%
    filter(Time > 0) %>%
    mutate(Need = 1, chi = 0) %>%
    gather(Infrastructure, Performance, -Run, -Time, -Need, - chi) %>%
    mutate(Performance = round(Performance / 100, 2)) %>%
    group_by(Run, Infrastructure)

assignGroup <- function(DF){
    DF <- DF %>% mutate(diff = round(Performance - Need, 2))
    i <- sign(DF$diff[1])
    grp <- 1
    DF$Grp <- 1
    DF_output <- tibble()
    for (run in 1:max(DF$Run)){
        DF_by_run <-
            DF %>%
            filter(Run == run)
        print(dim(DF_by_run))
        for (r in 2:nrow(DF_by_run)){
            if(DF_by_run$Infrastructure[r] != DF_by_run$Infrastructure[r-1]){
                grp <- 1
            }
            if (sign(i) == sign(DF_by_run$diff[r])){
                DF_by_run$Grp[r] <- grp
            } else {
                grp <- grp + 1
                i <- sign(DF_by_run$diff[r])
                print(grp)
                DF_by_run$Grp[r] <- grp
            }
            DF_by_run
        }
        DF_output <- bind_rows(DF_output, DF_by_run)
    }
    DF_output 
}
    
calc_EIR <- function(DF, chi){
    DFg <- DF %>%
        group_by(Run, Infrastructure, Grp) %>%
        summarize(grpInt = (trapz(Time, Performance) /
                            trapz(Time, Need)) *
                      (max(Time) - min(Time) + 1),
                  grpTime = max(Time) - min(Time))
    print(DFg)
    DFg <- DFg %>% summarize(ExtendedIntegralResilience =
                                 ifelse(sum(grpInt) / sum(grpTime) < 1,
                                        sum(grpInt) / sum(grpTime),
                                        1 + chi *
                                        (sum(grpInt) / sum(grpTime) - 1)))
    return(DFg)
}
calc_stakeholder_resilience <- function(performanceDF){
    ## Data.Frame or Tibble should be grouped by Infrastructure and
    ## Run. Need and Chishould be a columns. We are using the last
    ## time as the time horizon since that is the purpose of the hurricane
    ## demonstration
    performanceDF %>%
        mutate(excess = Performance - Need) %>%
        
        }

library("tidyverse")
library("readxl")
source("metrics.R")

## Pull the 15 example sheets
## xlfiles <- c("HurricaneDataFixed/MCoutputseed1.xlsx",
##              "HurricaneDataFixed/MCoutput15seed2.xlsx")#,
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
    ## pb <- txtProgressBar(min = 0, max = length(file_list), style = 3)
    for(f in 1:length(file_list)){
        #print(f)
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
        ## setTxtProgressBar(pb, f)
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
## wd <- ingestHurrData(xlfiles)
## 
## tidy_working_data <-
##     wd %>%
##     filter(Time > 0) %>%
##     mutate(Need = 1, chi = 0) %>%
##     gather(Infrastructure, Performance, -Run, -Time, -Need, -chi) %>%
##     mutate(Performance = round(Performance / 100, 2)) %>%
##     group_by(Run, Infrastructure) 

assignGroup <- function(DF){
    DF <- DF %>% mutate(diff = round(Performance - Need, 2))
    DF$Grp <- 1
    DF_output <- tibble()
    DF$Infrastructure <- as.factor(DF$Infrastructure)
    inf_values <- unique(DF$Infrastructure)
    inf_number <- length(inf_values)
    ## pb <- txtProgressBar(min = 0, max = length(unique(DF$Run)), style = 3)
    for (run in 1:length(unique(DF$Run))){
        DF_by_run <-
            DF %>%
            filter(Run == run)
        ## print(run)
        DF_inf_grp <- tibble()
        for (i in 1:inf_number){
            grp <- 1
            DF_inf <-
                DF_by_run %>%
                filter(Infrastructure == inf_values[i])
            s <- sign(DF_inf$diff[1])
            for (r in 1:nrow(DF_inf)){
                if (sign(s) == sign(DF_inf$diff[r])){
                    DF_inf$Grp[r] <- grp
                } else {
                    grp <- grp + 1
                    s <- sign(DF_inf$diff[r])
                    ##print(grp)
                    DF_inf$Grp[r] <- grp
                }
                DF_inf
            }
            DF_inf_grp <- bind_rows(DF_inf_grp, DF_inf)
            cat("\r", inf_values[i], run)
        }
        ## setTxtProgressBar(pb, run)
        DF_output <- bind_rows(DF_output, DF_inf_grp)
    }
    print("groups assigned")
    DF_output <- endcap_group(DF_output)
    DF_output
}

assignGroup_q <- function(DF){
    DF <- DF %>% mutate(diff = round(Performance - Need, 2))
    DF$Grp <- 1
    DF_output <- tibble()
    DF$Infrastructure <- as.factor(DF$Infrastructure)
    inf_values <- unique(DF$Infrastructure)
    inf_number <- length(inf_values)
    DF_by_run <-
        DF
    ## print(run)
    DF_inf_grp <- tibble()
    for (i in 1:inf_number){
        grp <- 1
        DF_inf <-
            DF_by_run %>%
            filter(Infrastructure == inf_values[i])
        s <- sign(DF_inf$diff[1])
        for (r in 1:nrow(DF_inf)){
            if (sign(s) == sign(DF_inf$diff[r])){
                DF_inf$Grp[r] <- grp
            } else {
                grp <- grp + 1
                s <- sign(DF_inf$diff[r])
                ##print(grp)
                DF_inf$Grp[r] <- grp
            }
            DF_inf
        }
        DF_inf_grp <- bind_rows(DF_inf_grp, DF_inf)
    }
    DF_output <- bind_rows(DF_output, DF_inf_grp)
    ## print("groups assigned")
    DF_output <- endcap_group_q(DF_output)
    DF_output
}

assignGroupEIR <- function(DF, chi = 0){
    DF <- DF %>% mutate(diff = round(Performance - Need, 2))
    DF$Grp <- 1
    DF_output <- tibble()
    DF$Infrastructure <- as.factor(DF$Infrastructure)
    inf_values <- unique(DF$Infrastructure)
    inf_number <- length(inf_values)
    ## pb <- txtProgressBar(min = 0, max = length(unique(DF$Run)), style = 3)
    for (run in 1:length(unique(DF$Run))){
        DF_by_run <-
            DF %>%
            filter(Run == run)
        ## print(run)
        DF_inf_EIR <- tibble()
        for (i in 1:inf_number){
            grp <- 1
            DF_inf <-
                DF_by_run %>%
                filter(Infrastructure == inf_values[i])
            s <- sign(DF_inf$diff[1])
            for (r in 1:nrow(DF_inf)){
                if (sign(s) == sign(DF_inf$diff[r])){
                    DF_inf$Grp[r] <- grp
                } else {
                    grp <- grp + 1
                    s <- sign(DF_inf$diff[r])
                    ##print(grp)
                    DF_inf$Grp[r] <- grp
                }
                DF_inf <- endcap_group(DF_inf)
                DF_inf <- calc_EIR(DF, chi)
            }
            DF_inf_EIR <- bind_rows(DF_inf_EIR, DF_inf)
            cat("\r", inf_values[i], run)
        }
        ## setTxtProgressBar(pb, run)
        DF_output <- bind_rows(DF_output, DF_inf_EIR)
    }
    print("all_EIR")
    DF_output
}

assignGroupLrg <- function(DF){
    DF <- DF %>% mutate(diff = round(Performance - Need, 2))
    DF$Grp <- 1
    DF_output <- tibble()
    DF$Infrastructure <- as.factor(DF$Infrastructure)
    inf_values <- unique(DF$Infrastructure)
    inf_number <- length(inf_values)
    ## pb <- txtProgressBar(min = 0, max = length(unique(DF$Run)), style = 3)
    for (run in 1:length(unique(DF$Run))){
        DF_by_run <-
            DF %>%
            filter(Run == run)
        ## print(run)
        DF_inf_grp <- tibble()
        for (i in 1:inf_number){
            grp <- 1
            DF_inf <-
                DF_by_run %>%
                filter(Infrastructure == inf_values[i])
            s <- sign(DF_inf$diff[1])
            for (r in 1:nrow(DF_inf)){
                if (sign(s) == sign(DF_inf$diff[r])){
                    DF_inf$Grp[r] <- grp
                } else {
                    grp <- grp + 1
                    s <- sign(DF_inf$diff[r])
                    ##print(grp)
                    DF_inf$Grp[r] <- grp
                }
                DF_inf
            }
            DF_inf_grp <- bind_rows(DF_inf_grp, DF_inf)
        }
        ## setTxtProgressBar(pb, run)
        DF_output <- bind_rows(DF_output, DF_inf_grp)
        DF_output <- endcap(DF_output)
        write.csv(DF_output, paste("tst/", run, DF_inf[r], "grped"))
    }
    print("groups assigned")
    ## DF_output <- endcap_group(DF_output)
    ## DF_output
}


## area calculations need to take into account where it was left off before
## this should handle the problem of when there is only one time tick in
## a group
endcap_group <- function(DF){
    num_runs <- length(unique(DF$Run))
    type_inf <- unique(DF$Infrastructure)
    num_inf <- length(type_inf)
    rDF <- tibble()
    ## pb <- txtProgressBar(min = 0, max = num_runs, style = 3)
    for (r in 1:num_runs){
        iDF <- tibble()
        ## print(paste("Run", r))
        for (i in 1:num_inf){
            inf_DF <-
                DF %>%
                filter(Run == r, Infrastructure == type_inf[i])
            gDF <- tibble()
            num_grp <- length(unique(inf_DF$Grp))
            for (g in 1:num_grp){
                grp_DF <-
                    inf_DF %>%
                    filter(Grp == g)
                if (g != 1){
                    new_start <-
                        inf_DF %>%
                        filter(Grp == g - 1) %>%
                        filter (Time == max(Time))
                    new_start$Grp <- g
                    grp_DF <- bind_rows(new_start, grp_DF)
                }
                ## I am afraid that this is double counting between
                ## the groups. We are double counting the transitions here.
                
                ## if (g != num_grp){
                ##     new_end <-
                ##         inf_DF %>%
                ##         filter(Grp == g + 1) %>%
                ##         filter(Time == min(Time))
                ##     new_end$Grp <- g
                ##     grp_DF <- bind_rows(grp_DF, new_end)
                ## }
                ##print(grp_DF)
                gDF <- bind_rows(gDF, grp_DF)
                gDF
            }
            iDF <- bind_rows(iDF, gDF)
        }
        ## setTxtProgressBar(pb, r)
        rDF <- bind_rows(rDF, iDF)
    }
    print("endcap done")
    rDF
}
endcap_group_q<- function(DF){
    type_inf <- unique(DF$Infrastructure)
    num_inf <- length(type_inf)
    rDF <- tibble()
    iDF <- tibble()
    ## print(paste("Run", r))
    for (i in 1:num_inf){
        inf_DF <-
            DF %>%
            filter(Infrastructure == type_inf[i])
        gDF <- tibble()
        num_grp <- length(unique(inf_DF$Grp))
        for (g in 1:num_grp){
            grp_DF <-
                inf_DF %>%
                filter(Grp == g)
            if (g != 1){
                new_start <-
                    inf_DF %>%
                    filter(Grp == g - 1) %>%
                    filter (Time == max(Time))
                new_start$Grp <- g
                grp_DF <- bind_rows(new_start, grp_DF)
            }
            ## I am afraid that this is double counting between
            ## the groups. We are double counting the transitions here.
            
            ## if (g != num_grp){
            ##     new_end <-
            ##         inf_DF %>%
            ##         filter(Grp == g + 1) %>%
            ##         filter(Time == min(Time))
            ##     new_end$Grp <- g
            ##     grp_DF <- bind_rows(grp_DF, new_end)
            ## }
            ##print(grp_DF)
            gDF <- bind_rows(gDF, grp_DF)
            gDF
        }
        iDF <- bind_rows(iDF, gDF)
    }
    ## print("endcap done")
    iDF
}

calc_EIR <- function(DF, chi){
    DFg <- DF %>%
        group_by(Run, Infrastructure, Grp) %>%
        summarise(grpInt = (trapz(Time, Performance) /
                            trapz(Time, Need)) *
                      (max(Time) - min(Time) + 1),
                  grpTime = max(Time) - min(Time))
    DFg <- DFg %>% summarise(ExtendedIntegralResilience =
                                 ifelse(sum(grpInt) / sum(grpTime) < 1,
                                        sum(grpInt) / sum(grpTime),
                                        1 + chi *
                                        (sum(grpInt) / sum(grpTime) - 1)))
}

add_nostorm_runs <- function(stormRuns, nostormRuns, num_noStorms){
    stormRuns <- ungroup(stormRuns)
    nostormRuns <- ungroup(nostormRuns) %>%
        select(-Run)    
    run_start <- max(stormRuns$Run)
    all_nf_runs <- tibble()
    for (run in 1:num_noStorms){
        nf_run <-
            nostormRuns %>%
            mutate(Run = run + run_start)
        all_nf_runs <- bind_rows(all_nf_runs, nf_run)
    }
    all_DF <- bind_rows(stormRuns, all_nf_runs)
    all_DF <- group_by(all_DF, Run)
}

######################################################################
## Summary statistics and Plotting                                  ##
######################################################################


summ_stats_inf <- function(DF){
    DF <-
        DF %>%
        group_by(Infrastructure) %>%
        summarise(mean = mean(ExtendedIntegralResilience),
                  sd = sd(ExtendedIntegralResilience),
                  median = median(ExtendedIntegralResilience),
                  min = min(ExtendedIntegralResilience),
                  max = max(ExtendedIntegralResilience),
                  bottom10 = quantile(ExtendedIntegralResilience, .10),
                  top10 = quantile(ExtendedIntegralResilience, .9))
}



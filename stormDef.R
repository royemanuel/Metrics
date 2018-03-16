## Pull the data from the anylogic run outputs

run_profiles <- c("hurrFullRec2yr/01runProfile.xlsx",
                  "hurrFullRec2yr/02runProfile.xlsx",
                  "hurrFullRec2yr/03runProfile.xlsx",
                  "hurrFullRec2yr/04runProfile.xlsx",
                  "hurrFullRec2yr/05runProfile.xlsx",
                  "hurrFullRec2yr/06runProfile.xlsx",
                  "hurrFullRec2yr/07runProfile.xlsx",
                  "hurrFullRec2yr/08runProfile.xlsx",
                  "hurrFullRec2yr/09runProfile.xlsx",
                  "hurrFullRec2yr/10runProfile.xlsx",
                  "hurrFullRec2yr/11runProfile.xlsx",
                  "hurrFullRec2yr/12runProfile.xlsx")

ingest_run_profiles <- function(file_list){
    all_data <- tibble()
    pb <- txtProgressBar(min = 0, max = length(file_list), style = 3)
    for(f in 1:length(file_list)){
        sheets <-
            file_list[f] %>%
            excel_sheets() %>%
            set_names() %>%
            map(read_excel, path = file_list[f])
        temp_sheet <- tibble()
        for (s in 1:length(sheets)){
            ## print(s)
            if(dim(sheets[[s]])[1] > 0){
                temp_data <- clean_run_data(sheets[[s]])
                temp_sheet <- rbind(temp_sheet, temp_data)
            }            
        }
        setTxtProgressBar(pb, f)
        all_data <- rbind(all_data, temp_sheet)
    }
    all_data
}

clean_run_data <- function(wkbk){
    wkbk <-
        wkbk %>%
        mutate(Run = ExperimentNumber[1]) %>%
        select(-ExperimentNumber)
}

    
##my10yrstorms <- ingest_run_profiles(run_profiles)
##write.csv(my10yrstorms, "runyrFR_profiles_seed.csv")

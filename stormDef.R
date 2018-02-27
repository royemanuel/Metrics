## Pull the data from the anylogic run outputs

run_profiles <- c("hurr10year/01runProfile10yr.xlsx",
                  "hurr10year/02runProfile10yr.xlsx",
                  "hurr10year/03runProfile10yr.xlsx",
                  "hurr10year/04runProfile10yr.xlsx",
                  "hurr10year/05runProfile10yr.xlsx",
                  "hurr10year/06runProfile10yr.xlsx",
                  "hurr10year/07runProfile10yr.xlsx",
                  "hurr10year/08runProfile10yr.xlsx",
                  "hurr10year/09runProfile10yr.xlsx",
                  "hurr10year/10runProfile10yr.xlsx",
                  "hurr10year/11runProfile10yr.xlsx")

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

    
my10yrstorms <- ingest_run_profiles(run_profiles)
write.csv(my10yrstorms, "run10yr_profiles_seed.csv")

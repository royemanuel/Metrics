## Pull the data from the anylogic run outputs

run_profiles <- c("HurricaneData1_with_StormSeed/001runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/002runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/003runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/004runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/005runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/006runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/007runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/008runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/009runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/010runProfile.xlsx",
                  "HurricaneData1_with_StormSeed/011runProfile.xlsx"
                  )

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

    
mystorms <- ingest_run_profiles(run_profiles)

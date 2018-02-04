library("tidyverse")
library("readxl")

## Pull the 15 example sheets
xlfiles <- c("HurricaneData/MCoutput15seed2.xlsx",
             "HurricaneData/MCoutput20seed3.xlsx")

## test_tibble <-
##     xlfiles %>%
##     excel_sheets() %>%
##     set_names() %>%
##     map(read_excel, path = xlfiles) %>%
##     filter()


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
        sheets <-
            file_list[f] %>%
            excel_sheets() %>%
            set_names() %>%
            map(read_excel, path = file_list[f])
        temp_sheet <- tibble()
        for (s in 1:length(sheets)){
            print(s)
            if(dim(sheets[[s]])[1] > 0){
                temp_data <- cleanHurrData(sheets[[s]])
                temp_sheet <- rbind(temp_sheet, temp_data)
            }
        }
        all_data <- rbind(all_data, temp_sheet)
    }
    all_data
}















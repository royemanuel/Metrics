library("tidyverse")
library("readxl")

## Pull the 15 example sheets
xlfiles <- c("c:/users/emanurn1/Documents/Resilience/AnyLogic/Austin Sector Model v4/Austin Sector Model/MCoutput15seed2.xlsx")




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
    for(f in 1:length(file_list)){
        sheet <- file_list[f] %>%
            excel_sheets() %>%
            set_names() %>%
            map(read_excel, path = file_list[f])
        for (s in 1:length(sheet){
            sheet[s] <- cleanHurrData(sheet[s])
        }
    }
}

raw_data <- read_excel(xlfiles)

    map(cleanHurrData)

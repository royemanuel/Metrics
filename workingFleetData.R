## This aggregates the final outputs from the combineFleetData to build
## csv's we will be working from. All data is stored with the document
## information so we are not mixing it up with the raw data.
######################################################################
library("lubridate")
setwd("d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience/")

PMResFiles <- list.files(path = ".", pattern = "datallRes")
PMResFiles <- PMResFiles[str_detect(PMResFiles, pattern = "20SEP")]
COgradFiles <- list.files(path = ".", pattern = "COgrad")
COgradFiles <- COgradFiles[str_detect(COgradFiles, pattern = "20SEP")]
COsatFiles <- list.files(path = ".", pattern = "COsat")
COsatFiles <- COsatFiles[str_detect(COsatFiles, pattern = "20SEP")]
PMRes <- list()
COGrad <- list()
COSat <- list()
for(f in 1:length(PMResFiles)){
    PMRes[[f]] <- read_csv(PMResFiles[f])
    COGrad[[f]] <- read_csv(COgradFiles[f])
    COSat[[f]] <- read_csv(COsatFiles[f])
}

dateID <- paste0(as.character(month(now())), "-",
                 as.character(day(now())), "-",
                 as.character(year(now())), "--",
                 as.character(hour(now())), "-",
                 as.character(minute(now())),".csv")

PMRes <- bind_rows(PMRes)
COGrad <- bind_rows(COGrad)
COSat <- bind_rows(COSat)

write_csv(PMRes, paste0("PMRes", dateID))
write_csv(COGrad, paste0("COGrad", dateID))
write_csv(COSat, paste0("COSat",  dateID))

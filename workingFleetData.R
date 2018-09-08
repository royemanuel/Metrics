## This aggregates the final outputs from the combineFleetData to build
## csv's we will be working from. All data is stored with the document
## information so we are not mixing it up with the raw data.
######################################################################

setwd("d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience/")

PMResFiles <- list.files(path = ".", pattern = "datallRes")
COgradFiles <- list.files(path = ".", pattern = "COgrad")
COsatFiles <- list.files(path = ".", pattern = "COsat")
PMRes <- list()
COGrad <- list()
COSat <- list()
for(f in 1:length(PMResFiles)){
    PMRes[[f]] <- read_csv(PMResFiles[f])
    COGrad[[f]] <- read_csv(COgradFiles[f])
    COSat[[f]] <- read_csv(COsatFiles[f])
}

PMRes <- bind_rows(PMRes)
COGrad <- bind_rows(COGrad)
COSat <- bind_rows(COSat)

write_csv(PMRes, paste0("PMRes",
                        as.character(month(now())), "-",
                        as.character(day(now())), "-",
                        as.character(year(now())), "--",
                        as.character(hour(now())), "-",
                        as.character(minute(now())),".csv"))
write_csv(COGrad, paste0("COGrad", 
                        as.character(month(now())), "-",
                        as.character(day(now())), "-",
                        as.character(year(now())), "--",
                        as.character(hour(now())), "-",
                        as.character(minute(now())),".csv"))
write_csv(COSat, paste0("COSat",  
                        as.character(month(now())), "-",
                        as.character(day(now())), "-",
                        as.character(year(now())), "--",
                        as.character(hour(now())), "-",
                        as.character(minute(now())),".csv"))

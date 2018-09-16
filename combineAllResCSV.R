

setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/fleetData/RFR2/8SEPData/")

allResFiles <- list.files(path = ".", pattern = "allRes")

DDate <- "8SEPdata-15SEPbld"

allRes <- tibble()
for(ar in 1:length(allResFiles)){
    a <- read_csv(allResFiles[ar], col_types = list(col_character(),
                                                    col_double(),
                                                    col_double(),
                                                    col_character(),
                                                    col_character(),
                                                    col_double(),
                                                    col_double(),
                                                    col_character()))
    allRes <- bind_rows(allRes, a)
}

allRes2 <-
    allRes %>%
    mutate(TimeHorizon = as.integer(round(TimeHorizon / 8764, 1)))

COsatAll <- list()
COgradAll <- list()

allCOsatFiles <- list.files(path = ".", pattern = "COsat")
allCOgradFiles <- list.files(path = ".", pattern = "COgrad")
for(CO in 1:length(allCOsatFiles)){
    COsatAll[[CO]] <- read_csv(allCOsatFiles[CO])
    COgradAll[[CO]] <- read_csv(allCOgradFiles[CO])
}

COsatAll <- bind_rows(COsatAll)
COgradAll <- bind_rows(COgradAll)


write_csv(allRes2,
          paste0(
              "d:/OneDrive/PhD Work/Dissertation/Word/",
              "Journal Articles/Fleet Resilience/",
              "datallRes",
              DDate,
              ".csv"))
write_csv(COsatAll, 
          paste0(
              "d:/OneDrive/PhD Work/Dissertation/Word/",
              "Journal Articles/Fleet Resilience/",
              "COsatAll",
              DDate,
              ".csv"))
write_csv(COgradAll,
          paste0(
              "d:/OneDrive/PhD Work/Dissertation/Word/",
              "Journal Articles/Fleet Resilience/",
              "COgradAll",
              DDate,
              ".csv"))

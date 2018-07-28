setwd("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/fleetData/RFR2/7JULdata/")

allResFiles <- list.files(path = ".", pattern = "allRes")


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


write_csv(allRes2, "d:/OneDrive/PhD Work/Dissertation/Word/Journal Articles/Fleet Resilience/allRes7JUL.csv")

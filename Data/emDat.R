## Read the csv and build the outputs
library("plyr")
library("dplyr")
emDat <- read.csv("EM-Data.csv", skip = 1)

emSum <- summarize(emDat,
                   mean(occurrence),
                   mean(Total.deaths),
                   mean(Injured),
                   mean(Affected),
                   mean(Homeless),
                   mean(Total.affected),
                   mean(Total.damage),
                   sum(occurrence),
                   sum(Total.deaths),
                   sum(Injured),
                   sum(Affected),
                   sum(Homeless),
                   sum(Total.affected),
                   sum(Total.damage)
                   )
  

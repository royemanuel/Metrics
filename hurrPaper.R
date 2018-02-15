## This file builds the plots and the values to support the
## hurricane infrastructure paper.
source("HurricaneDataPull.R")

base_hurricane_files <-
    c("HurricaneDataFixed/MCoutputseed1.xlsx",
      "HurricaneDataFixed/MCoutput15seed2.xlsx",
      "HurricaneDataFixed/MCoutput20seed3.xlsx",
      "HurricaneDataFixed/MCoutput20seed4.xlsx",
      "HurricaneDataFixed/MCoutput20seed5.xlsx",
      "HurricaneDataFixed/MCoutput20seed6.xlsx",
      "HurricaneDataFixed/MCoutput20seed7.xlsx",
      "HurricaneDataFixed/MCoutput20seed8.xlsx",
      "HurricaneDataFixed/MCoutput20seed9.xlsx",
      "HurricaneDataFixed/MCoutput20seed10.xlsx",
      "HurricaneDataFixed/MCoutput20seed11.xlsx",
      "HurricaneDataFixed/MCoutput18seed12.xlsx"
      )

base_hd <- ingestHurrData(base_hurricane_files)


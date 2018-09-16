source("d:/OneDrive/PhD Work/Dissertation/Programming/Metrics/COandPMplots.R")

ggsave(filename = "PMsatRes.pdf",
       path = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                     "Journal Articles/Fleet Resilience/"),
       plot = PMSatsumPlot,
       device = cairo_pdf,
       width = 3.5,
       height = 3.5)

ggsave(filename = "PMGradRes.pdf",
       path = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                     "Journal Articles/Fleet Resilience/"),
       plot = PMGradsumPlot,
       device = cairo_pdf,
       width = 3.5,
       height = 3.5)

ggsave(filename = "PMAoRes.pdf",
       path = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                     "Journal Articles/Fleet Resilience/"),
       plot = PMAosumPlot,
       device = cairo_pdf,
       width = 3.5,
       height = 3.5)

ggsave(filename = "CO_E_Grad.pdf",
       path = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                     "Journal Articles/Fleet Resilience/"),
       plot = COGradsumPlot_CO_E,
       device = cairo_pdf,
       width = 3.5,
       height = 3.5)

ggsave(filename = "CO_Grad.pdf",
       path = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                     "Journal Articles/Fleet Resilience/"),
       plot = COGradsumPlot_CO,
       device = cairo_pdf,
       width = 7,
       height = 3.5)

ggsave(filename = "CO_Sat.pdf",
       path = paste0("d:/OneDrive/PhD Work/Dissertation/Word/",
                     "Journal Articles/Fleet Resilience/"),
       plot = COSatsumPlot,
       device = cairo_pdf,
       width = 7,
       height = 3.5)
       

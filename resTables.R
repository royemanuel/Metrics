library(ReporteRs)
## Use the loaded data - in this case biDF (big infrastructure data frame)
## filter it down and build a good table for Word

filesLoaded <- readline("Did you load metrics.R, resFuncs.R, and Data.R?")

if (filesLoaded == "n"){
    source("metrics.R")
    source("resFuncs.R")
    source("dataLoad.R")
}


asIsRes <- biDF %>%
    filter(fileName == "bigAsIs.csv") %>%
        select(-fileName, -Scenario)
bigRobRes <- biDF %>%
    filter(fileName == "bigRob.csv") %>%
        select(-fileName, -Scenario)
earlyRec16K <- biDF %>%
    filter(fileName == "big16kRec.csv") %>%
        select(-fileName, -Scenario)
rec100Res <- biDF %>%
    filter(fileName == "big100percentRec.csv") %>%
        select(-fileName, -Scenario)
stepRecRes <- biDF %>%
    filter(fileName == "bigStep.csv") %>%
        select(-fileName, -Scenario)

## Build the Flex Tables
asIsTable <- vanilla.table(asIsRes)
asIsTable <- addHeaderRow(asIsTable, text.properties = textBold(),
                          value = 'As-Is Scenario', colspan = 7)
resTable <- docx()

resTable <- addFlexTable(resTable, vanilla.table(asIsRes))
resTable <- addFlexTable(resTable, vanilla.table(fail20Res))
resTable <- addFlexTable(resTable, vanilla.table(fail5000Res))
resTable <- addFlexTable(resTable, vanilla.table(rec100Res))
resTable <- addFlexTable(resTable, vanilla.table(stepRecRes))
resTable <- addFlexTable(resTable, asIsTable)
writeDoc(resTable, file = "resTable.docx")

allResTable <- docx()
allResTable <- addFlexTable(allResTable, vanilla.table(biDF))
writeDoc(allResTable, file = "allResTable.docx")


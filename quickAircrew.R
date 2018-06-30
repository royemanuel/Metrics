library("readxl")
aircrewTracker <- read_csv("aircrewExp1Run0.csv")

## Using the inverse of the training times to get the training rate.
## That way, resilience is towards 1 when satisfied, and towards 
aircrewData <-
    aircrewTracker %>%
    filter(Disp == "G") %>%
    mutate(Time =  as.integer(exitDate),
           Performance = 1 / TimeInSqdn,
           Need = 1 / 1200,
           indRes = Performance / Need) %>%
    mutate(date = as_datetime(3600 * Time +
                              make_datetime(2005, 1, 1, 8))) %>%
    mutate(qrtr = quarter(date, with_year = TRUE)) %>%
    group_by(qrtr) %>%
    filter(date == max(date)) 


airPlot <- ggplot(aircrewData, aes(Time, indRes)) + geom_point()

airPlot








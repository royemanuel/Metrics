## Load the appropriate data sets to do analysis without worrying about
## all the other stuff.

biDF <- round_df(read.csv("bigInfrastructureResilience2.csv"), 2)
biDF <- select(biDF, -X)

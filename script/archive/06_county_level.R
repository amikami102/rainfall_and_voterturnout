###-------------------------------------
# This script creates the county-level data
# based on the census data we obtained in 
# "05_census_data.R". 
###-------------------------------------


add_countyvar <- function(df){
        df <- df %>% mutate(
                FarmOperations = as.numeric(FarmOperations)) %>%
                mutate(
                        PctBlack = BlackPopulation/TotalPopulation,
                        Rural = FarmOperations/TotalPopulation,
                        PctHSgrad = (HSgrad_Female + HSgrad_Male)/TotalPopulation
        ) 
        return(df)
}

cces2006 <- add_countyvar(cces2006)
cces2008 <- add_countyvar(cces2008)
cces2010 <- add_countyvar(cces2010)
cces2012 <- add_countyvar(cces2012)

source("script/saveRDS.R")

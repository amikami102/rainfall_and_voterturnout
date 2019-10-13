####----------------------------------
# This script adds the rainfall data and CWA 
# (county warning area) code from
# Cooperman (2017)'s dataset. 
####----------------------------------


# load Cooperman's dataset
source("script/cooperman.R")

get_weather <- function(df, year){
        # This function matches the rainfall and CWA data 
        # from cooperman's dataset by FIPS code and year. 
        #-----------------------------------
        # df (dataframe, the cces dataset)
        # fips (string, the column in df for FIPS code)
        # year (numeric, the year of the cces dataset)
        #-----------------------------------
        
        # Cut out the part of cooperman for specified year
        data <- cooperman %>% filter(Year == year) %>%         
                select(FIPS.County, Rainfall12, 
                       Rainfall12.index, Rainfall12.spi, CWA_short) %>%
                rename(FIPScode = FIPS.County)
        
        out <- left_join(x = df, y = data, by = "FIPScode")
        return(out)
}

cces2006 <- get_weather(cces2006, 2006)
cces2008 <- get_weather(cces2008, 2008)
cces2010 <- get_weather(cces2010, 2010)
cces2012 <- get_weather(cces2012, 2012)


# Save data as .rds files 
source("script/saveRDS.R")

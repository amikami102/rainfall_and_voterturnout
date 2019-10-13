#!/usr/bin/env Rscript
###--------------------
# This script obtains the median household
# income per county from the 5-year ACS published
# by US Census Bureau. 
# See README.md for more information on 
# data source. 
###--------------------
# Census table numbers, variable code:
# "B19013" median household income, 
# "C02003" population by race, "C02003_004" for black population
# "B23006" educational attainment 25 to 64 years old, "B23006_009" for hs grad
# "B01003" total population 
###-----------------------------
# Download ACS 2005-2009 and 2010-2015 data for median 
# household income (table.number = "B19013"). For 2016,
# we are using the 2015 data. 
###------------------------------
# We convert $$ to 2010 dollars
###------------------------------
# The outputs of this script are 
# - "data/acs/acs-B01003_001-20([10]|[05]).rds"
# - "data/acs/acs-B19013_001-20([10]|[05]).rds"
# - "data/acs/acs-B23006_009-20([10]|[05]).rds"
# - "data/acs/acs-C02003_004-20([10]|[05]).rds"
###-----------------------------

library(rio)
library(tidyverse)
library(logging)
library(acs)
acs.tables.install()
load("data/counties.Rdata")

# install Census API key
api.key.install(Sys.getenv("CENSUS_API_KEY"))


fetch_acs <- function(endyear, variable){
        # Fetch ACS data for given end year and 
        # variable. 
        #------------------
        # endyear (int, year in YYYY format)
        # variable (str, variable code)
        #-------------------
        acstab <- acs.fetch(endyear = endyear, 
                            span = 5, 
                            geography = geo.make(state = "*", county = "*"),
                            variable = variable,
                            dataset = "acs") %>%
                # convert $$ to 2010 dollars
                currency.convert(newyear = 2015)
        
        # add 'fips_code' column to acstab@geography
        acstab@geography <- acstab@geography %>%
                mutate(fips_code = paste0(str_pad(state, width = 2, pad = "0", side = "left"),
                                          county)) 
        # export data
        filename <- paste("acs", variable, endyear, sep = "-")
        export(acstab, file = file.path("data", "acs", paste0(filename, ".rds")))
        loginfo("Downloaded ACS data for variable %s, end year %d", variable, endyear)
}



main <- {
        # create a list of argument combinations to feed into acs.fetch()
        endyears <- c(2010, 2015)
        vars <- c("B19013_001", "C02003_004", "B23006_009", "B01003_001")
        args <- list("endyears" = endyears, "vars" = vars) %>% cross_df() %>% as.list()
        
        args %>% pmap(~ fetch_acs(.x, .y))
        

}



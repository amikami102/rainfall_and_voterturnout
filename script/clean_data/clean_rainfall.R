#!/usr/bin/Rscript
##------------------
# This script converts the .dbf outputs from 
# "kriging.py" to .csv files while cleaning the 
# data according to the procedure documented in 
# Cooperman (2017). The general steps are as follows:
# 1.) load dbf file;
# 2.) merge the dataframe with the attributes data of 
# 2010 US Census counties cartographic boundary file;
# 3.) convert the units of rainfall level from tenths of mm to 
# inches. 
# Finally, we check the correlation of our rainfall values on 
# election days with Cooperman (2017)'s dataset. 
###-----------------------------
# According to the README.txt of GHCN Daily Ver 3.24
# (https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
# the precipitation data is recorded in tenths of mm. 
# We want to convert the unit to inches. 
# 0.1 mm * (1cm/10mm) * (1 in/ 2.54 cm) 
###-----------------------------
# We will use the cartographic boundary from 2010 US census
# to label the counties as this was the version used in 
# krigoutput.py to draw county boundaries. 
##--------------------
library(rio)
library(tidyverse)
library(logging)
library(argparse)


# set up command line parser
parser <- ArgumentParser(description='Clean the rainfall data.')
parser$add_argument('--zonalstat', type = "character",
                    help = "Directory to storing zonal statistics tables.",
                    default = "./output/rainfall/zonal_stat")
args <- parser$parse_args()


# load counties00, counties10, states stored in "counties.Rdata"
load("output/counties.Rdata")  


clean_rainfall <- function(dbf_file){
        #-----------------------------
        # Loads dbf file and clean the data by
        # 1.) recoding negative rainfall values to 0,
        # 2.) convert the unit to inches, and
        # 3.) add FIPS code label to each county. 
        #-----------------------------
        # dbf_file (str, name of the dbf file)
        #-----------------------------
        
        df <- dbf_file %>% import() %>%
                # adjust the levels of GEO_ID to match that of the counties10
                mutate(GEO_ID = factor(GEO_ID, 
                                levels(counties10$GEO_ID)), 
                       # recode to 0 if rainfall level is negative
                       MEAN = ifelse(MEAN < 0, 0, MEAN),
                       # convert unit to inches 
                       rainfall = MEAN/254) %>%
                select(GEO_ID, MEAN, rainfall) %>%
                left_join(counties10,
                            by = "GEO_ID") %>%
                # create a column called "fips_code"
                mutate(fips_code = paste0(as.character(STATE),
                                         as.character(COUNTY)) %>% 
                               as.character()) %>%
                # select columns
                select(GEO_ID, rainfall, fips_code)
        
        # save to file 
        rds_name <- dbf_file %>% 
                str_replace(pattern = dirname(.), 
                            replacement = "") %>%
                str_replace(pattern = ".dbf", replacement = ".rds")
        export(df, file = file.path(args$zonalstat, rds_name))
        loginfo(paste0("Created ", rds_name))
}

main <- {
        list.files(args$zonalstat, 
                   pattern = "^county-rainfall\\d{4}-\\d{2}-\\d{2}.dbf$",
                   full.names = TRUE) %>% 
                map(~ clean_rainfall(.x))
}



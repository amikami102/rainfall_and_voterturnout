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
# Finally, we check the correlation with Cooperman's dataset. 
###-----------------------------
# According to the README.txt of GHCN Daily Ver 3.24
# (https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
# the precipitation data is recorded in tenths of mm. 
# We want to convert the unit to inches. 
# 0.1 mm * (1cm/10mm) * (1 in/ 2.54 cm) 
##--------------------
library(rio)
library(tidyverse)
library(logging)
library(argparse)
library(rgdal) # for reading shape files

# set up command line parser
parser <- ArgumentParser(description='Process some integers')
parser$add_argument('--output', type = "character",
                    help = "Directory to store output files.",
                    default = "./output/rainfall")
parser$add_argument('--input', type = "character",
                    help = "Directory storing input files.",
                    default = "./output/krigoutput")
args <- parser$parse_args()



# load the US county cartographic boundary shapefile 
shp_name <- "gz_2010_us_050_00_500k"
us_counties <- readOGR(dsn = file.path("data", shp_name), 
                       shp_name, verbose = FALSE)

# convert dbf to csv while merging the dataframe with information 
# about US counties
dbf_files <- list.files(args$input, 
                        pattern = "^county-rainfall\\.dbf$")





clean_rainfall <- function(dbf_file){
        ## Loads dbf file and cleans the data.
        ## Output is a csv file stored in args$outdir. 
        ##------------------
        ## dbf_file (str, name of the dbf file)
        ## -------------------
        
        
        df <- read.dbf(file.path(args$input, dbf_file)) %>%
                # adjust the levels of GEO_ID to match that of us_counties@data
                mutate(GEO_ID = factor(GEO_ID, 
                                levels(us_counties@data$GEO_ID)), 
                       # recode to 0 if rainfall level is negative
                       MEAN = ifelse(MEAN < 0, 0, MEAN),
                       # convert unit to inches 
                       Rainfall = MEAN/254) %>%
                dplyr::select(GEO_ID, MEAN, Rainfall)
                # merge with us_counties@data
        merged <- left_join(df, us_counties@data,
                            by = "GEO_ID") %>%
                # create a column called FIPScode
                mutate(FIPScode = paste0(as.character(STATE),
                                         as.character(COUNTY)))
        
        # save to file 
        csv_name <- gsub(".dbf", ".csv", dbf_file)
        export(merged, file = file.path(args$output, csv_name))
        logger$info(paste0("Created ", csv_name))
}

for (file in dbf_files){
        clean_rainfall(file)
}


# Check correlation with cooperman_rainfall 
cooperman <- read.csv(file.path(args$output, 
                                "cooperman_rainfall.csv")) %>%
                filter(Year >= 2006)

for (yr in c(2006, 2008, 2010, 2012)){
        y <- file.path(args$output, 
                           list.files(args$outdir, 
                                      pattern = as.character(yr))[7]) %>%
                import()
        x <- cooperman %>% filter(Year == yr) 
        
        # Cooperman uses 2000 Census, so make sure that 
        # we only compare the counties that exist in both
        counties <- intersect(x$FIPS.County, y$FIPScode)
        
        # filter x and y
        x_ <- x %>% filter(FIPS.County %in% counties) 
        y_ <- y %>% filter(FIPScode %in% counties) 
        
        # compute correlation
        r <- cor(sort(x_$Rainfall12), sort(y_$Rainfall)) %>% round(digits = 5)
        logger$info(paste0("Correlation with Cooperman's data is ", r))
}




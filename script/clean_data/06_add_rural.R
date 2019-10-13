#!/usr/bin/env Rscript
###--------------------------
# This script adds a column for the number of farms in 
# operation per capita
# per county from the 5-year Census of Agriculture
# published by USDA National Agricultural
# Statistics Service.
###--------------------------
# Accordng to the USDA NASS Census of Agriculture 2017
# FAQ page, the 2017 data will be available in April 2019.
###----------------------

library(rio)
library(tidyverse)
library(logging)
library(argparse)

# set up command line arguments 
parser <- ArgumentParser(description = "Adding 'rural' CCES datasets.")
parser$add_argument("--cces", default="output/cces_processed", 
                    type = "character", nargs = 1,
                    help = "Directory storing output data.")
parser$add_argument("--usca", default = "data/usca",
                    type = "character", nargs = 1,
                    help = "Directory storing US Census of Agriculture data.")
args <- parser$parse_args()



add_rural <- function(file){
        # Adds a column called "rural" indicating the 
        # number of farm in operations per capita per county.
        #---------------
        # file (str, file path to cces*.rds)
        #---------------
        
        if(str_detect(file, "2006")){
                usca <- import(file.path(args$usca, "usca2002.rds"))
        }
        if(str_detect(file, "2008|2010")){
                usca <- import(file.path(args$usca, "usca2007.rds"))
        }
        if(str_detect(file, "2012|2014|2016")){
                usca <- import(file.path(args$usca, "usca2012.rds"))
        }
        farms <- usca %>% filter(str_detect(SHORT_DESC,
                                            "FARM OPERATIONS - NUMBER OF OPERATIONS") & 
                                         str_detect(DOMAIN_DESC, "TOTAL")) %>%
                select(STATE_ALPHA, STATE_NAME, STATE_FIPS_CODE,
                       COUNTY_CODE, COUNTY_NAME, VALUE) %>%
                mutate(fips_code = paste0(str_pad(STATE_FIPS_CODE, width = 2, side = "left", pad = "0"),
                                          str_pad(COUNTY_CODE, width = 3, side = "left", pad = "0")),
                        farms = as.numeric(VALUE)
                ) %>%
                select(farms, fips_code) 
        cces_df <- import(file) %>%
                left_join(farms, by = "fips_code") %>%
                # divide total number of farms in operation ("VALUE") by 
                # total population estimate ("total.est")
                mutate(rural = farms/total.est) 
        export(cces_df, file)
        loginfo("Rural added to %s", 
                str_replace(file, pattern = dirname(file), ""))
}

# main 
main <- {
        rds_list <- list.files(args$cces, pattern = ".rds$",
                               full.names = TRUE)
        
        rds_list %>% map(~ add_rural(.x))
        
        # save output to zip file
        zip(zipfile = file.path(args$cces, "script06_output.zip"), 
            files = rds_list)
}



#!/usr/bin/env Rscript
###-----------------
# This script adds county-level variables
# to the CCES datsets by joining table outputs
# from script/R/census_data.R, namely
# - total population per county
# - % black per county
# - % high school graduate per county.
###-----------------

library(rio)
library(tidyverse)
library(logging)
library(argparse)


# set up command line arguments 
parser <- ArgumentParser()
parser$add_argument("--cces", default="output/cces_processed", 
                    type = "character", nargs = 1,
                    help = "Directory storing processed data.")
parser$add_argument("--acs", default = "data/acs",
                    type = "character", nargs = 1,
                    help = "Directory storing ACS data.")
args <- parser$parse_args()


# create lists for ACS 2010 and ACS 2015
acs2010_list <- list.files(args$acs, pattern = "2010.rds$", full.names = TRUE)
acs2015_list <- list.files(args$acs, pattern = "2015.rds$", full.names = TRUE)


# import data and define objects to be used in join_acs()
import_acs <- function(file_list){
        # Import 'acs' objects from files created by
        # census_data.R, and reduce these objects to 
        # dataframes. 
        #---------------------
        # file_list (list of files storing `acs` objects)
        #---------------------
        total <- import(grep("B01003", file_list, value = TRUE))
        black <- import(grep("C02003", file_list, value = TRUE))
        hsgrad <- import(grep("B23006", file_list, value = TRUE))
        medianincome <- import(grep("B19013", file_list, value = TRUE))
        
        reduce_acs <- function(acs_obj){
                #--------------------
                # Reduce `acs`` object to a dataframe
                # with 3 columns.
                #--------------------
                # acs_obj (acs object)
                #--------------------
                obj_name <- deparse(substitute(acs_obj))
                acs_obj@estimate %>%
                        merge(acs_obj@standard.error, by = 0) %>%
                        merge(acs_obj@geography, 
                                by.x = "Row.names", by.y = "NAME") %>%
                        select(-state, -county, -Row.names)
        }
        # apply reduce_acs to the imported `acs` objects
        out <- list(total = total, black = black, hsgrad = hsgrad, 
                    medianincome = medianincome) %>%
                map(~ reduce_acs(.x)) 
        return(out)    
        
}

# join acs tables to cces dataset
join_acs <- function(file){
        #--------------------
        # Join dataframe outputs from import_acs()
        # to cces datasets.
        #--------------------
        # file (str, file path to cces*.rds)
        #-------------------
        
        year <- str_detect(file, pattern = "20[0-9]{2}") %>% as.numeric()
        if(year < 2010){
                acs_list <- import_acs(acs2010_list)
        }
        else{
                acs_list <- import_acs(acs2015_list)
        }
        cces_df <- import(file) %>%
                        left_join(acs_list$total, by = "fips_code") %>%
                        left_join(acs_list$black, by = "fips_code") %>%
                        left_join(acs_list$hsgrad, by = "fips_code") %>%
                        left_join(acs_list$medianincome, by = "fips_code") %>%
                        rename(total.est = "B01003_001.x", 
                               total.se = "B01003_001.y",
                               black.est = "C02003_004.x", 
                               black.se = "C02003_004.y",
                               hsgrad.est = "B23006_009.x", 
                               hsgrad.se = "B23006_009.y",
                               medianincome.est = "B19013_001.x", 
                               medianincome.se = "B19013_001.y")
        
        export(cces_df, file)
        loginfo("County level variables added to %s", 
                str_replace(file, dirname(file), ""))
        
}

main <- {
        rds_list <- list.files(args$cces, 
                               pattern = "^cces[0-9]{4}.rds$",
                               full.names = TRUE)
        rds_list %>% map(~join_acs(.))
        
        # save outputs to zipfile
        zip(zipfile = file.path(args$cces, "script05_output"),
            files = rds_list)
}

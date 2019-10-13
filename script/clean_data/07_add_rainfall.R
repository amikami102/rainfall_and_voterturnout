#----------------------
# This script adds rainfall values to the 
# CCES datasets. Here are the overall steps:
# 1.) combine the datasets for a year into one dataframe,
# 2.) join the dataframe from step 1 to the corresponding CCES dataset.
# The file outputs of this script are 
# - "output/rainfall/reduced/rainfall_20[0-9]{2}.rds" 
# - "output/cces_processed/cces_20[0-9]{2}.rds" 
#----------------------

library(rio)
library(tidyverse)
library(argparse)
library(logging)

# set up command line parser
parser <- ArgumentParser(description='Add rainfall data to cces datasets.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "output/cces_processed")
parser$add_argument('--rainfall', type = "character",
                    help = "Directory storing rainfall datasets",
                    default = "output/rainfall/zonal_stat")
args <- parser$parse_args() 



add_rainfall <- function(year){
        #----------------------
        # Takes all files for that year and
        # combines the rainfall values for the 
        # 7 days up to the election day. Then joins this
        # dataframe to the corresponding CCES dataset. 
        #----------------------
        # year (int, YYYY format)
        #----------------------
        
        # combine all the rainfall values for this year into one dataframe
        rain_df <- list.files(args$rainfall, 
                               paste0(year, "-[0-9]{2}-[0-9]{2}.rds$"), 
                               full.names = TRUE) %>%
                map(~ import(.x) %>% select(fips_code, rainfall)) %>%
                reduce(left_join, by = "fips_code") %>%
                rename_at(vars(contains("rainfall")),
                          list(~ paste("day", 1:7, sep = "_")))
        rain_file <- file.path(dirname(args$rainfall), 
                               "reduced", 
                               paste0("rainfall_", year, ".rds"))
        export(rain_df, file = rain_file)
        loginfo("Created %s", rain_file)
        
        
        # join to corresponding CCES dataset
        cces_file <- list.files(args$cces, 
                                pattern = paste0("cces", year, ".rds"),
                                full.names = TRUE)
        cces_df <- cces_file %>% 
                import() %>%
                left_join(rain_df, by = "fips_code")
        export(cces_df, cces_file)
        loginfo("Rainfall data added for year %d", year)
                
}

main <- {
        c(2006, 2008, 2010, 2012, 2014, 2016) %>%
                map(~ add_rainfall(.x))
        
        cces_files <- list.files(args$cces, pattern = "^cces[0-9]{4}.rds$", 
                                 full.names = TRUE)
        
        # save outputs to zipfile
        zip(zipfile = file.path(args$cces, "script07_output.zip"), 
            cces_files)
}


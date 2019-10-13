###----------------------
# This script downloads the county warning are
# boundary files from NOAA National Weather Service and
# adds the CWA labels to CCES datasets. See README.md 
# for more information on data source. 
###-----------------------
# We check that the same FIPS codes do not have different
# CWA labels and verify that each FIPS code is under unique CWA.
###-----------------------

library(rio)
library(tidyverse)
library(magrittr) # %<>%
library(argparse)
library(logging)
library(rgdal) # for reading shapefiles


# set up command line parser
parser <- ArgumentParser(description='Add CWA labels to CCES datasets.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
parser$add_argument('--cwa', type = "character",
                    help = "Directory storing CWA datasets",
                    default = "./data/cwa")
args <- parser$parse_args() 



# read cwa shapefile
cwa <- readOGR(dsn = args$cwa, layer = "c_02oc18")@data

# check that each FIPS code has unique CWA by 
# 1.) filtering all rows whose FIPS appear more than once in the dataset,
# 2.) group these rows by FIPS * CWA, and
# 3.) check if the tibble produced by step 2 have duplicated FIPS.
# If there are duplicated FIPS appearing in the tibble produced by 
# step 2, then there must be more than one CWA associated with that FIPS code.
duplicated <- cwa %>% 
        filter(duplicated(FIPS)) %>% 
        group_keys(FIPS, CWA) 
loginfo("Number of FIPS code associated with more than one CWA: %d",
        duplicated %>% filter(duplicated(FIPS)) %>% nrow)

cwa %<>% select(FIPS, CWA) %>% mutate(FIPS = as.character(FIPS))

main <- {
        list.files(args$cces, pattern = ".rds$", 
                   full.names = TRUE) %>% 
                set_names(., nm = .) %>%
                imap(~ {
                        import(.x) %>% 
                                left_join(cwa, by = c("fips_code" = "FIPS")) %>%
                                export(file = .y)
                        loginfo("Added CWA to %s", 
                                str_replace(string = .y, 
                                            dirname(.y), replacement = ""))
                })
        # save outputs to zipfile
        zip(zipfile = file.path(args$cces, "script08_output.zip"),
            files = list.files(args$cces, pattern = ".rds$", full.names = TRUE))
}

        



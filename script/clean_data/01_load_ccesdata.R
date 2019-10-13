#!/usr/bin/Rscript
#---------------------------------------
# This Rscript imports the CCES data files that were downloaded 
# manually from Harvard Dataverse and exports as rds to the output 
# directory. Although the R package `rio` can handle different formats, 
# we use `foreign` package to import the raw datasets to ensure that
# the files are read correctly. Once we convert these files into 
# rds format, we will use `rio` for reading and rewriting them. 
#---------------------------------------

library(rio)
library(magrittr)
library(tidyverse)
library(logging)
library(argparse)
library(haven)


# set up command line arguments
parser <- ArgumentParser(description='Import CCES datasets.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
parser$add_argument('--raw', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./data/cces/raw")
args <- parser$parse_args()


convert_cces <- function(){
        # Import raw CCES datasets as .rds datasets. 
        # Export the .rds files. 
        #--------------------
        # year (int, year of the CCES dataset in YYYY format)
        #--------------------
        
        # create a list of files to be read
        cceslist <- list.files(args$raw, full.names = TRUE) %>%
                set_names(seq(2006, 2018, by = 2))
        
        # convert the raw datasets to "rds" files 
        cceslist %>% imap(~ {
                if(.y == "2008"){
                        cces_df <- foreign::read.spss(.x, to.data.frame = TRUE)
                }
                if(.y == "2018"){
                        cces_df <- read_dta(.x) %>% 
                                mutate_if(is.labelled, list(~ as_factor(.)))
                }
                if(.y != "2008" & .y != "2018"){
                        cces_df <- foreign::read.dta(.x)
                }
                output <- paste0("cces", .y, ".rds")
                export(cces_df, file.path(args$cces, output))
                loginfo("%s created", output)
        })
}

main <- {
        convert_cces()
        
        # save output of this script to zip
        zip(zipfile = "output/cces_processed/script01_output",
            files = list.files(args$cces, full.names = TRUE,
                       pattern = "cces[0-9]{4}.rds$"))
}


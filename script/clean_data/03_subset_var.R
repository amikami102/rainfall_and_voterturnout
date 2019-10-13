#!/usr/bin/Rscript
####----------------------------------
# This script subsets the following variables
# from the CCES datasets. 
#-------------------------
# - "fips_code" the column created in "02_FIPScode.R"
# - "state" respondent's state of residence
# - "race" respondent's race
# - "employment" respondent's employment status
# - "partyID_7" respondent's PID on 7-point scale
# - "education" respondent's highest education achieved
# - "birth_year" respondent's birth year
# - "family_income" respondent's self-reported family income
# - "home_ownership" respondent's home ownership status
# - "validated" indicator for whether respondent's vote was validated on record
# - "intend_to_vote" responden'ts self-reported intention of voting, pre-election
# - "voted" respondent's self-reported voting status post-election
# - "registered" respondent's self-reported voter registration status pre-election
####----------------------------------
library(rio)
library(tidyverse)
library(logging)
library(argparse)

# set up command line arguments
parser <- ArgumentParser(description='Subset columns from CCES datasets.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
args <- parser$parse_args()

# get the variable names from cces code guides in "data/cces/guide"
var.names <- c("fips_code", "state", 
                   "race", "employment", "partyID_7", 
                   "education", "birth_year", 
                   "family_income", "home_ownership",
                   "validated", "intend_to_vote", "voted", "registered")
var06 <- c("fips_code", "v1002", 
               "v2005", "v2030", "v3007", 
               "v2018", "v2020", 
               "v2032", "v2033", 
               "vote_gen06", "vote06turn", "v4004", "v3004")
var08 <- c("fips_code", "V206",
               "V211", "V209", "CC307a",
               "V213", "V207",
               "V246", "CC333",
               "vote_gen08", "CC326", "CC403", "V203")
var10 <- c("fips_code", "V206", 
               "V211", "V209", "V212d",
               "V213", "V207",
               "V246", "V250",
               "vote_gen10", "CC354", "CC401", "V203")
var12 <- c("fips_code", "inputstate",
               "race", "employ", "pid7", 
               "educ", "birthyr", 
               "faminc", "ownhome",
               NA, "CC354", "CC401", "votereg")
var14 <- var12
var16 <- c("fips_code", "inputstate",
           "race", "employ", "pid7", 
           "educ", "birthyr", 
           "faminc", "ownhome",
           NA, "CC16_364", "CC16_401", "votereg")
var18 <- c("fips_code", "inputstate",
           "race", "employ", "pid7", 
           "educ", "birthyr", 
           "faminc_new", "ownhome",
           NA, "CC18_350", "CC18_401", "votereg")

varlist <- list("2006" = var06, "2008" = var08, 
                "2010" = var10, "2012" = var12, 
                "2014" = var14, "2016" = var16, "2018" = var18) %>%
        map(~ setNames(.x, nm = var.names))



## Subset the variables 
subset_var <- function(rds_file, year){
        #----------------------------
        # Select variables from CCES dataset. 
        # ---------------------------
        # rds_file (str, file path to a CCES dataset)
        # year (int, year of the CCES dataset)
        # ----------------------------
        
        vars <- varlist[[as.character(year)]]
        if (NA %in% vars){vars <- vars[!is.na(vars)]}
        
        if(year == "2010"){
                vars <- c(vars, "voting_mode" = "CC403")
        }
        
        cces_df <- rds_file %>% 
                import() %>%
                dplyr::select(!!vars)
        export(cces_df, rds_file)
        loginfo("Columns subsetted for %s", rds_file)
}

# main 
main <- {
        # create a list of rds
        rds_list <- list.files(args$cces, 
                               full.names = TRUE,
                               pattern = "^cces[0-9]{4}.rds$")
        
        # subset variables
        rds_list %>% 
                set_names(seq(2006, 2018, by = 2)) %>% 
                imap(~ subset_var(.x, .y))
        
        # save output of this script to zip
        zip(zipfile = file.path(args$cces, "script03_output"),
            files = rds_list)
}




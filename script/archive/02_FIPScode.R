####---------------------------
# This script creates a new column called
# "fips_code" for all cces data sets which records
# the 5-digit FIPS code of respondent's state and 
# county of residence. 
#--------------------------------
# We use 2010 Census to match county
# name to their FIPS code. 
#------------------------------
# cces2006: v1004 gives the respondent's residence in 
# <County> - <State>" format
#------------------------------
# cces2008: V269 and V251 give pre-election county
# and state FIPS code respectively.
#-------------------------------
# cces2010: V277 gives pre-election 5-digit FIPS code
#-------------------------------
# cces2012, 2014, 2016: countyfips gives the pre-election
# 5-digit FIPS code
####-----------------------------
library(rio)
library(tidyverse)
library(logging)
library(argparse)

# load `counties00`, `counties10`, `states` 
load("output/counties.Rdata")


# set up command line arguments
parser <- ArgumentParser(description = "Add 'fips_code' to CCES datasets.")
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
args <- parser$parse_args()

# define functions to clean cces 2006
get_fips <- function(county, state){
        # Returns a 5-digit FIPS code of the 
        # named state and county for CCES 2006. 
        #------------------
        # county (str, name of the county)
        # state (str, name of the state)
        #------------------
        
        # list of county names to be corrected 
        orig <- c("La Porte", "Prince of Wales-Outer Ketchikan Census",
                  "District of", "Wrangell-Petersburg Census",
                  "Lagrange", "Denali Borough",
                  "Southeast Fairbanks Census", "Aleutians West Census",
                  "Mc Kean", "Valdez-Cordova Census", "De Kalb")
        new <- c("LaPorte", "Prince of Wales-Outer Ketchikan",
                 "District of Columbia", "Wrangell-Petersburg", 
                 "LaGrange", "Denali",
                 "Southeast Fairbanks", "Aleutians West",
                 "McKean", "Valdez-Cordova", "DeKalb")
        rename_counties <- map(new, ~.x) %>% set_names(orig)
        if(county %in% names(rename_counties)){
                county <- rename_counties[[county]]
        }
        
        # get the FIPS code 
        state_fips <- states$STATE[states$NAME == state]
        county_fips <- counties10$COUNTY[counties10$NAME == county & 
                                        counties10$STATE == state_fips]
        if(is.null(county_fips) | length(county_fips) != 1){
                return(NA)
                loginfo("FIPS code not found for %s, %s", county, state)
        }
        
        # return the 5-digit FIPS code
        return(paste0(state_fips, county_fips))
}

clean_06 <- function(cces_df = cces2006){
        cces_df %<>% drop_na(v1004) %>% 
                filter(str_detect(v1004, " - ")) %>%
                mutate(county = str_split(v1004, " - ") %>% 
                               map_chr(1)) %>%
                mutate(state = str_split(v1004, " - ") %>% 
                               map_chr(2)) %>%
                mutate(fips_code = map2_chr(county, state, 
                                            ~ get_fips(.x, .y)))
}


# define functions to clean cces 2008
clean_08 <- function(cces_df = cces2008){
        cces_df %>% 
                mutate(fips_code = paste0(V251, 
                                        str_pad(V269, 
                                                width = 3, 
                                                side = "left", 
                                                pad = "0")))
}
       

# define functions to clean cces 2010
clean_10 <- function(cces_df = cces2010){
        cces_df %>% mutate(fips_code = as.character(V277))
}


# define functions to clean cces 2012, 2014, 2016
## `countyfips` has an 'AsIs' attribute that we don't need. 
.unAsIs <- function(X) {
        # This function reverses the effect of I() and
        # removes the 'AsIs' attribute. 
        #----------------------
        # X (variable name)
        #----------------------
        if(class(X)=="AsIs") {
                class(X) <- class(X)[-match("AsIs", class(X))]
                attr(X, "comment") <- NULL
        }
        return(X)
}

clean_12 <- function(cces_df){
        cces_df %>% mutate(fips_code = .unAsIs(countyfips))
}



add_fips_code <- function(file, year){
        # Add "fips_code" column to the cces dataset
        #-----------------
        # file (str, file path to cces*.rds)
        #-----------------
        
        # select the appropriate function for cleaning the dataset
        if(year == "2006"){
                cces_df <- import(file) %>% clean_06
        }
        if(year == "2008"){
                cces_df <- import(file) %>% clean_08
        }
        if(year == "2010"){
                cces_df <- import(file) %>% clean_10
        }
        if(year %in% seq(2012, 2018, by = 2)){
                cces_df <- import(file) %>% clean_12
        }
        
        
        export(cces_df, file)
        loginfo("'fips_code' added to %s", file)
}

# main
main <- {
        
        rds_list <- list.files(args$cces, 
                               full.names = TRUE,
                               pattern = "cces[0-9]{4}.rds$")
        
        rds_list %>% set_names(seq(2006, 2018, by = 2)) %>%
                imap(~ add_fips_code(.x, .y))
        
        # save output of this script to zip file
        zip(zipfile = file.path(args$cces, "script02_output"),
            files = rds_list)
}




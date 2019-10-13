###--------------------
# This script joins all the CCES datasets into one dataframe. 
# The output is saved as "dataset.rds" in "output/cces_processed"
# subdirectory. 
###--------------------
# We want to process the dataset to make it ready for passing it
# into our model. Specifically, we do the following:
# - create a 'year' column indicating the year of observation;
# - create a factor variable called 'county-year', which is a combination of
# 'fips_code' and 'year',
# - create "pctblack", "pcthsgrad" to get percentage of black population and 
# high school graduates,
# - create "log_medianincome" by taking log of median income.
###--------------------


library(rio)
library(tidyverse, quietly = TRUE)
library(magrittr)
library(logging)
library(argparse)


# set up command line arguments
parser <- ArgumentParser(description='Join all CCES datasets.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
args <- parser$parse_args() 

# import all CCES datasets 
all <- list.files(args$cces, pattern = "^cces[0-9]{4}.rds$", 
                  full.names = TRUE) %>% 
        set_names(c('2006', '2008', '2010', '2012', '2014', '2016')) %>%
        imap(~ import(.x) %>% mutate(year = .y)) %>%
        reduce(full_join, by = c("fips_code", "state", 
                                 "race", "employment", 
                                 "partyID_7", "education", 
                                 "birth_year", "family_income", 
                                 "home_ownership", 
                                 "intend_to_vote", "voted",
                                 "registered", "total.est",
                                 "total.se", "black.est",
                                 "black.se", "hsgrad.est",
                                 "hsgrad.se", "medianincome.est",
                                 "medianincome.se", "farms",
                                 "rural", "day_1", "day_2", "day_3",
                                 "day_4", "day_5", "day_6", "day_7", 
                                 "CWA", "year")) %>%
        mutate(year = factor(year, 
                             levels = as.character(seq(2006, 2016, by =2))),
               county_year = interaction(fips_code, year),
               pctblack = black.est/total.est,
               pcthsgrad = hsgrad.est/total.est,
               log_medianincome = log(medianincome.est)) %>%
        select_at(vars(-contains("validated"), -contains("voting_mode"))) 
export(all, file = file.path(args$cces, "dataset.rds"))
loginfo("Combined dataset has %d rows and %d columns", nrow(all), ncol(all))


# 
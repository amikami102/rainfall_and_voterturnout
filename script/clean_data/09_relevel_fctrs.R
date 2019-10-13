###--------------------
# This script cleans the factor variables that
# we subsetted from the original CCES datasets, namely
# "state", "employment", "partyID_7", "birth_year",
# "home_ownership", "intend_to_vote", "voted", and "registered".
###--------------------
# CCES 2014 and 2016 do not display the birth years except 
# as `<NA>`. Pending response from CCES people. 
###--------------------

library(rio)
library(tidyverse, quietly = TRUE)
library(magrittr)
library(logging)
library(argparse)
load("output/counties.Rdata")


# set up command line arguments
parser <- ArgumentParser(description='Relevel factor variables.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
args <- parser$parse_args() 


# import all CCES datasets 
all <- list.files(args$cces, pattern = "^cces[0-9]{4}.rds$", 
                  full.names = TRUE) %>% 
        set_names(c('2006', '2008', '2010', '2012', '2014', '2016')) %>%
        map(import)

# define the levels for each factor variable
employ.levels <- c("Full-time", "Party-time", "Homemaker", "Retired",
                   "Unemployed", "Temporarily laid off", 
                   "Permanently disabeled", "Student", "Other", "Not Asked")
state.levels <- all %>% pluck('2014', 'state') %>% 
        fct_collapse("Not Asked" = c("Skipped", "Not Asked")) %>% levels
partyID_7.levels <- all %>% pluck('2014', 'partyID_7') %>% 
        fct_collapse("Not Asked" = c("Skipped", "Not Asked")) %>% levels
registered.levels <- c("Yes", "No", "Don't know", "Not Asked")
home.levels <- c("Own", "Rent", "Other", "Not Asked")
intend.levels <- c("Yes", "No", "I already voted", "Undecided", "Not Asked")
voted.levels <- c("Yes", "No", "Not Asked")


# set up factor levels for 'fips_code' by combining counties 
# from 2000 and 2010 census
county.levels <- fct_c(with(counties00, paste0(STATE, COUNTY)) %>% factor,
                      with(counties10, paste0(STATE, COUNTY)) %>% factor
                      ) %>% levels


expand_NotAsked <- function(fct){
        #--------------------
        # Relabels "Not asked" and "Skipped" as "Not Asked". 
        # Assigns NA's to "Not Asked". 
        #--------------------
        # fct (factor)
        #--------------------
        
        fct %<>% fct_relabel(~ str_replace(.x, "asked", "Asked")) %>%
                fct_collapse("Not Asked" = c("Skipped", "Not Asked")) %>%
                fct_explicit_na(na_level = "Not Asked")
        return(fct)
        
}


relabel_home <- function(home_ownership){
        #-----------------
        # Relabels 'home_ownership' variables so that 
        # any response stating "I rent" becomes "Rent",
        # "I own" becomes "Own", 
        # and other statements of the form "I live ..." becomes "Other".
        #-----------------
        # home_ownership (fct)
        #-----------------
        home_ownership %<>% fct_relabel(~ str_replace(.x,
                                                      "I rent", "Rent") %>%
                                str_replace(., "^(I own)[\\W\\w]+", "Own") %>%
                                str_replace(., "^(I live)[\\W\\w]+", "Other")) %>%
                                expand_NotAsked %>%
                factor(levels = home.levels, ordered = FALSE)
        return(home_ownership)
}


relabel_intend <- function(intend_to_vote){
        #-------------------
        # Relabels 'intend_to_vote' variable so that
        # any statement of uncertainty becomes "Undecided",
        # "Yes, definitely" becomes "Yes", and other statements
        # expressing early voting becomes "I already voted". 
        #--------------------
        # intend_to_vote (fct)
        #--------------------
        intend_to_vote %<>% fct_relabel(~ str_replace(.x,
                                                    "(Not sure)|(Probably)", "Undecided") %>%
                                         str_replace(., "Yes, definitely", "Yes") %>%
                                         str_replace(., "^I[\\W\\w]+", "I already voted")) %>%
                        expand_NotAsked() %>%
                        factor(levels = intend.levels, ordered = FALSE)
        return(intend_to_vote)
}



relabel_voted <- function(voted){
        #------------------
        # Relabel 'voted' variable so that 
        # the levels are "Yes", "No", "Not Asked". The last
        # category may include early voters.
        #------------------
        # voted (fct)
        #------------------
        voted %<>% fct_relabel(~ str_replace(.x,
                                            "(I definitely voted)[\\W\\w]+", 
                                            "Yes") %>% 
                str_replace(., "^I[\\W\\w]+", "No") %>%
                str_replace(., "Don't Know", "Not Asked")) %>%
                expand_NotAsked() %>%
                factor(levels = voted.levels, ordered = FALSE)
        return(voted)
}



 
main <- {
        # Need to get information on early voters for CCES 2010
        all[['2010']] %<>% mutate(intend_to_vote = as.character(intend_to_vote)) %>%
                mutate(intend_to_vote = ifelse(voting_mode == "Voted by mail",
                                               "I already voted",
                                               ifelse(voting_mode == "In person before election day (early)",
                                                      "I already voted",
                                                      intend_to_vote)) %>% 
                               factor())
        loginfo("Added early-voter category to CCES 2010")
        
        all %<>% map( ~ {
                # Relevel employment
                .x %>% mutate(employment = employment %>% expand_NotAsked %>%
                                      factor(levels = employ.levels,
                                             ordered = FALSE))
        })
        loginfo("Releveled employment")
        
        all %<>% map(~{
                # Convert 'fips_code' into factor variable
                .x %>% mutate(fips_code = fips_code %>% 
                                      factor(levels = county.levels,
                                             ordered = FALSE))
        })
        loginfo("Converted 'fips_code' into factor variable")
        
        all %<>% map(~ {
                # Relevel state 
                .x %>% mutate(state = fct_relabel(state, ~ str_replace(.x, 
                                                      "District Of Colu",
                                                      "District of Columbia")) %>%
                                      expand_NotAsked %>%
                                      factor(levels = state.levels, 
                                             ordered = FALSE))
        })
        loginfo("Releveled 'states'")
        
        all %<>% map(~ {
                # Relevel partyID_7
                .x %>% mutate(partyID_7 = fct_relabel(partyID_7,
                                                      ~ str_replace(.x,"Weak",
                                                                    "Not very strong") %>%
                                                              str_replace(., "Don't know", "Not sure")) %>%
                                      expand_NotAsked %>%
                                      factor(levels = partyID_7.levels,
                                             ordered = FALSE))
        })
        loginfo("Releveled 'partyID_7'")
        
        all %<>% map(~ {
                # Convert birth_year to integer
                .x %>% mutate(birth_year = as.character(birth_year) %>% 
                                      as.integer)
        })
        loginfo("Converted 'birth_year' to integer")
        
        all %<>% map(~ {
                # Relevel 'home_ownership'
                .x %>% mutate(home_ownership = relabel_home(home_ownership))
        })
        loginfo("Releveled 'home_ownership'")
        
        all %<>% map(~ {
                # Relevel 'registered'
                .x %>% mutate(registered = registered %>% 
                                      expand_NotAsked %>%
                                      factor(levels = registered.levels,
                                             ordered = FALSE))
        })
        loginfo("Releveled 'registered'")
        
        
        all %<>% map(~ {
                # Relevel 'intend_to_vote'
                .x %>% mutate(intend_to_vote = relabel_intend(intend_to_vote))
        })
        loginfo("Releveled 'intend_to_vote'")
        
        all %<>% map(~ {
                # Relevel 'voted'
                .x %>% mutate(voted = relabel_voted(voted))
        })
        loginfo("Releveld 'voted'")
        
        # save as individual rds
        all %>% imap(~ {
                export(.x, file = file.path(args$cces, 
                                            paste0("cces", .y, ".rds")))
                loginfo("Saved %s", paste0("cces", .y, ".rds"))
        })
        # save outputs to zipfile
        zip(zipfile = file.path(args$cces, "script09_output"),
                files = list.files(args$cces, "^cces[0-9]{4}.rds", 
                                   full.names = TRUE))
}
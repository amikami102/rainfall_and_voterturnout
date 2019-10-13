####----------------------------------
# This script recodes `family_income`, `education`,
# `race` and creates a variable called `age`.
####----------------------------------
library(rio)
library(magrittr)
library(tidyverse)
library(logging)
library(argparse)

# set up command line arguments
parser <- ArgumentParser(description='Recode indiviudal level variables.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
args <- parser$parse_args()

# Recode `family_income`
recode_income <- function(file){
        #------------------
        # file (str, file path to cces*.rds)
        #------------------
        income_lvls <- c("Less than $10,000", "$10,000 - $19,999",
                         "$20,000 - $29,999", "$30,000 - $39,999",
                         "$40,000 - $49,999", "$50,000 - $59,999",
                         "$60,000 - $69,999", "$70,000 - $79,999",
                         "$80,000 - $99,999", "$100,000 - $119,999",
                         "$120,000 - $149,999", "$150,000 or more")
        income1 <- c("$10,000 - $14,999", "$15,000 - $19,999")
        income2 <- c("$20,000 - $24,999", "$25,000 - $29,999")
        income3 <- c("$150,000 - $199,999",  "$200,000 - $249,999",
                     "$250,000 - $349,999",  "$350,000 - $499,999",
                     "$350,000 - $499,999", 
                     "$150,000 or more", "$250,000 or more", "$500,000 or more")
        na_string <- c("Prefer not to say", "Skipped", "Not asked")
        
        cces_df <- import(file) %>% 
                mutate(family_income = as.character(family_income)) %>%
                mutate(family_income = ifelse(family_income %in% income1, "$10,000 - $19,999",
                    ifelse(family_income %in% income2, "$20,000 - $29,999",
                           ifelse(family_income %in% income3, "$150,000 or more",
                                  ifelse(family_income %in% na_string, NA, family_income))))) %>%
                mutate(family_income = factor(family_income, levels = income_lvls, ordered = TRUE))
        export(cces_df, file)
        loginfo("Recoded family income for %s", file)
}


# Recode 'race'
recode_race <- function(file){
        #-----------------
        # file (str, file path to cces*.rds)
        #-----------------
        other_string <- c("Native American", "Mixed", 
                          "Middle Eastern", "Other")
        na_string <- c("Prefer not to say", "Skipped", "Not asked")
        
        cces_df <- import(file) %>%
                mutate(race = as.character(race)) %>%
                mutate(race = ifelse(race %in% other_string, "Other", 
                    ifelse(race %in% na_string, NA, race))) %>%
                mutate(race = factor(race, levels = c("White", "Black", 
                                  "Hispanic", "Asian", "Other"),
                    ordered = FALSE))
        export(cces_df, file)
        loginfo("Recoded race for %s", file)
}



# Recode 'education' 
recode_educ <- function(file){
        #-----------------
        # file (str, file path to cces*.rds)
        #-----------------
        college_ <- c("Some college", "2-year",
                      "4-year", "Post-grad")
        na_string <- c("Prefer not to say", "Skipped", "Not asked")
        
        cces_df <- import(file) %>%
                mutate(education = ifelse(education %in% college_, "Some college and above", 
                    ifelse(education %in% na_string, NA, education))) %>%
                mutate(education = factor(education, 
                                          levels = c("No HS", 
                                                     "High school graduate", 
                                                "Some college and above"),
                                          ordered = TRUE))
        export(cces_df, file)
        loginfo("Recoded education for %s", file)
}

# Create variable 'age'
create_age <- function(file, year){
        #-------------------
        # file(str, file path to cces*.rds)
        #-------------------
        cces_df <- import(file) %>%
                mutate(birth_year = birth_year %>% as.character() %>% 
                               as.integer()) %>%
                mutate(birth_year = ifelse(birth_year %in% c(9998, 9999),
                                           NA, birth_year)) %>%
                mutate(age = year - birth_year)
}

# main
main <- {
        rds_list <- list.files(args$cces, full.names = TRUE,
                               pattern = "cces[0-9]{4}.rds$")
        rds_list %>% set_names(seq(2006, 2018, by = 2)) %T>% 
                map(~recode_income(.x))%T>% 
                map(~ recode_race(.x)) %T>% 
                map(~ recode_educ(.x)) %T>%
                imap(~ create_age(.x, .y))
        
        # create zip file of output 
        zip(zipfile = file.path(args$cces, "script04_output"), 
            files = rds_list)
}





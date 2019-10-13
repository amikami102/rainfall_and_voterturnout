###-------------------
# This script fits the hierarchical model
# defined in 'hmodel.stan' to our dataset stored in 
# 'dataset.Rds'. 
###-------------------

library(rio)
library(tidyverse, quietly = TRUE)
library(argparse)
library(rstan)

# set up command line arguments
parser <- ArgumentParser(description='Subset columns from CCES datasets.')
parser$add_argument('--cces', type = "character",
                    help = "Directory storing processed CCES datasets.",
                    default = "./output/cces_processed")
parser$add_argument('--analysis', type = "character",
                    help = "Directory storing this analysis's results.",
                    default = "./script/analysis/2019-03-22")
args <- parser$parse_args()


# import our dataset and drop any observations whose fips_code is NA
dataset <- import(file.path(args$cces, "dataset.rds")) %>%
        mutate(y = ifelse(voted == "Yes", 1, 
                          ifelse(voted == "No", 0, NA))) %>%
        drop_na() 

# Define data list 
individual_covar <- c("race", "partyID_7", "education", "family_income")
county_year_covar <- c("pctblack", "pcthsgrad", "log_medianincome", "rural")
n_county_year <- dataset$county_year %>% unique() %>% length
#X <- dataset %>% select(!!individual_covar)
U <- dataset %>% select(!!county_year_covar, county_year) %>%
        group_by(county_year) %>% 
        summarize_all(unique)
rain <- dataset %>% select(starts_with("day"), county_year) %>%
        group_by(county_year) %>% summarize_all(unique)


X <- model.matrix(y ~ county_year + race + partyID_7 + 
                          education + family_income, data = dataset)

dataList <- list(N = nrow(dataset),
                    K = length(individual_covar),
                    n_county_year = n_county_year,
                    R = length(county_year_covar),
                    X = X,
                    U = U,
                    day7 = rain$day_7,
                    day6 = rain$day_6,
                    day3 = rain$day_3,
                    day1 = rain$day_1,
                    county_year_id = dataset$county_year,
                    y = dataset$y,
                    alpha0_mean = 0,
                    alpha0_var = 1.6,
                    beta_mean = 0,
                    beta_var = 1.6, 
                    a = 3, b = 0.5)

model_debug <- stan(file = file.path(args$analysis, "hmodel.stan"), 
                    data = dataList, 
                    iter=10, chains=1)
modResults <- stan(file = file.path(args$analysis, "hmodel.stan"),
                   model_name = "hmodel",
                   data = dataList)




#!/usr/bin/Rscript
#---------------------------------------
# This script import and cleans Cooperman (2017)'s dataset
# downloaded from https://doi.org/10.7910/DVN/RJF61A. 
#---------------------------------------
library(rio)
library(tidyverse)

cooperman <- import("data/cooperman_rep/cooperman_dataset.Rdata")

# Some of the FIPS code in the column, FIPS.County, are 
# less than 5 digits.
# This is due to some of the state FIPS codes missing the 
# prepending zero, namely the FIPS codes for 
# Alabama = 01, Alaska = 02, Arizona = 04, Arkansas = 05, California = 06, 
# Colorado = 08, Connecticut = 09. 
# Fortunately, the 20,878 entries FIPS.County that are
# 4 digits long all belong to these states, so we only need to pad
# the string with zero in the first index. 


cooperman %<>% 
        mutate(FIPS.County = str_pad(FIPS.County, 
                                     width = 5, side = "left", 
                                     pad = "0")) %>%
        select(FIPS.County, Year, starts_with("Rainfall"),
               County, State, CWA_short, REG)


# check if there are still problematic FIPS code
which(str_length(as.character(cooperman$FIPS.County)) < 5)







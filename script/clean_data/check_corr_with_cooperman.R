##-------------------------
# We will check correlation with Cooperman's dataset for 
# the years 2006, 2008, 2010, 2012, which is where her data 
# ends. The correlation values are stored in the log file. 
##-------------------------
library(rio)
library(tidyverse)
library(logging)
library(argparse)
load("output/counties.Rdata")

# set up command line parser
parser <- ArgumentParser(description='Process some integers')
parser$add_argument('--zonal_stat', type = "character",
                    help = "Directory to store output files.",
                    default = "./output/rainfall/zonal_stat")
args <- parser$parse_args()

# set up logger to write to file
addHandler(writeToFile, file = ".log/check_corr_cooperman.log", 
           level = "DEBUG")


# import cooperman's data
source("script/clean_data/cooperman.R")



c(2006, 2008, 2010, 2012) %>% map_dbl(~{
        
        my_df <- list.files(args$zonal_stat, full.names = TRUE,
                            pattern = paste0("county-rainfall", 
                                             .x, "-[0-9]{2}-[0-9]{2}.rds"))[7] %>%
                import() %>% 
                arrange(fips_code)
        
        cooperman <- cooperman %>% 
                filter(Year == .x) %>% 
                arrange(FIPS.County)
        
        
        shared_fips <- intersect(my_df$fips_code, cooperman$FIPS.County)
        x_ <- my_df %>% 
                filter(fips_code %in% shared_fips) 
        y_ <- cooperman %>% 
                filter(FIPS.County %in% shared_fips)
        
        loginfo("Correlation with Cooperman's: %f", 
                cor(x_$rainfall, y_$Rainfall12) %>% round(digits = 3))
})
        
        
        
        
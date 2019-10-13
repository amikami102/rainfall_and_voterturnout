#!/usr/bin/env Rscript
###----------------------------
# This script grabs data on county-level variables for 
# the years 2006, 2008, 2010, 2012, 2014, and 2016
# from the US Census API. See Gomez et al. (2007) Table 1
# for description of the variables. 
###-----------------------------

#library(censusapi)
#library(lubridate)


#apis <- listCensusApis()
#varlist <- listCensusMetadata(name = "acs5", vintage = 2010)
#geos <- listCensusMetadata(name = "acs5", vintage = 2009, type = "g")
#x <- subset(varlist, grepl("high school", varlist$label, ignore.case = TRUE))

# vintage_yr = 2012 (2012 adj. dollars)
# vintage_yr = 2015 (2015 adj. dollars)
# vintage_yr = 2010 (2010 adj. dollars)

# Annual CPI in 2010 dollars for years 2006, 2008, 2010, 2012, 2014, 2016
monthly_cpi <- read.csv("data/CPIAUCSL.csv", header = TRUE) # downloaded from St.Louis FRED
monthly_cpi$cpi_year <- year(monthly_cpi$DATE)
yearly_cpi <- monthly_cpi %>% group_by(cpi_year) %>% summarize(cpi = mean(CPIAUCSL))
yearly_cpi$adj_factor <- yearly_cpi$cpi/yearly_cpi$cpi[yearly_cpi$cpi_year == 2010]

# Attach Census data 
get_CensusData <- function(vintage_yr, cces_yr){
        #------------------------------
        # This function gets median household income data
        # from 5-year ACS ("acs5") from US Census. We will also
        # adjust the values to 2010 dollars. 
        #------------------------------
        # vintage_yr = (numeric) the vintage year of "acs5"
        # cces_yr  = (numeric), the CCES dataset year
        #-------------------------------
        key <- Sys.getenv("CENSUS_API_KEY")
        adj_factor <- yearly_cpi$adj_factor[yearly_cpi$cpi_year==cces_yr]
        tbl <- getCensus(name = "acs5", vintage = vintage_yr, 
                         key = key,
                         vars = c("B19013_001E", "B19013_001M", 
                                  "C02003_004E", "C02003_004M",
                                  "B15002_028E", "B15002_028M",
                                  "B15002_011E", "B15002_011M",
                                  "B01003_001E", "B01003_001M"),
                         region="county:*")
         tbl <- tbl %>% rename(MedianHouseIncome = B19013_001E,
                       MedianHouseIncome_M = B19013_001M,
                       BlackPopulation = C02003_004E,
                       BlackPopulation_M = C02003_004M,
                       HSgrad_Female = B15002_028E,
                       HSgrad_Female_M = B15002_028M,
                       HSgrad_Male = B15002_011E,
                       HSgrad_Male_M = B15002_011M, 
                       TotalPopulation = B01003_001E,
                       Total_population_M = B01003_001M) %>%
                mutate(FIPScode = paste0(state, county),
                       MedianHouseIncome = MedianHouseIncome * adj_factor,
                       MedianHouseIncome_M = MedianHouseIncome_M * adj_factor) %>%
                select(- state, - county)
        cces <- paste0("data/cces_data/cces", cces_yr, ".rds") %>% readRDS()
        out <- left_join(cces, tbl, by = "FIPScode")
        return(out)
}



# Get number of farms per capita for each county
# rootURL <- "ftp://ftp.nass.usda.gov/quickstats/"
# download.file(paste0(rootURL, "qs.census", 2002, ".txt.gz"), 
#                "usca2002.txt.gz")
# download.file(paste0(rootURL, "qs.census", 2007, ".txt.gz"), 
#                "usca2007.txt.gz")
# download.file(paste0(rootURL, "qs.census", 2012, ".txt.gz"), 
#                "usca2012.txt.gz")
get_FarmNumber <- function(census_yr, cces_yr){
        filename <- paste0("data/usca", census_yr, ".txt.gz")
        tbl <- read.table(gzfile(filename), 
                            sep = "\t", header = TRUE) 
        col <- tbl %>%
                subset(grepl("FARM OPERATIONS - NUMBER OF OPERATIONS", 
                            tbl$SHORT_DESC) &
                                grepl("TOTAL", tbl$DOMAIN_DESC),
                        select = c(STATE_ALPHA, STATE_FIPS_CODE, COUNTY_CODE, COUNTY_NAME,
                                   VALUE)
                )  %>%
                mutate(FIPScode = paste0(sprintf("%02d", STATE_FIPS_CODE),
                                         sprintf("%03d", COUNTY_CODE))) %>%
                rename(FarmOperations = VALUE) %>% select(FIPScode, FarmOperations)
        cces <- paste0("data/cces_data/cces", cces_yr, ".rds") %>% readRDS()
        out <- left_join(cces, col, by = "FIPScode")
        return(out)
        }








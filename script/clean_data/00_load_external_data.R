###------------------------
# This script downloads zipfiles for US cartographic boundary
# shapefiles from US Census Bureau, 
# US Census of Agriculture zip files from USDA,
# and US County Warning Area from National Weather Service. 
###------------------------------
# See README.md for more information on the sources. 
# The datasets are cleaned separately in their own scripts.
###------------------------
# Zip files are stored in a temporary file when downloaded. 
# The extracted files are stored in the subdirectory "data". 
###------------------------

# US cartographic boundary file for counties 

## download and extract zip file from 2010 Census
base_url <- "http://www2.census.gov/geo/tiger/GENZ2010/"
zipnames <- c("gz_2010_us_050_00_500k", # county
              "gz_2010_us_040_00_500k") # state

zipnames %>% map(~ {
        zipfile_url <- paste0(base_url, zip, ".zip")
        temp <- tempfile()
        download.file(zipfile_url, temp)
        unzip(temp, exdir = file.path("data", zip))
        unlink(temp)
})

## download and extract zip file from 2000 Census
base00_url <- "https://www2.census.gov/geo/tiger/PREVGENZ/co/co00shp/"
zipname <- "co99_d00_shp"
zipfile_url <- paste0(base00_url, zipname, ".zip")
temp <- tempfile()
download.file(zipfile_url, temp)
unzip(temp, exdir = file.path("data", zipname))
unlink(temp)


# US Census of Agriculture 2002, 2007, 2012
## download US Census of Agriculture for available years
ca_years <- c(2002, 2007, 2012)
get_usca_files <- function(yr){
        #------------------
        # Download US Census of Agriculture for given year
        #------------------
        # yr (int, year in YYYY fromat)
        #------------------
        base_url <- "ftp://ftp.nass.usda.gov/quickstats/"
        filename <- paste0("usca", yr, ".txt.gz")
        download.file(paste0(base_url, "qs.census", yr, ".txt.gz"),
                      dest = file.path("data", "usca", filename), 
                      method = "curl")
        table <- read.table(gzfile(file.path("data", "usca", filename)),
                            sep = "\t", header = TRUE)
        export(table, file.path("data/usca", 
                                str_replace(filename, pattern = ".txt.gz", replace = ".rds")))
}

ca_years %>% map(~get_usca_files(.x))


# US County Warning Area 
url <- "https://www.weather.gov/source/gis/Shapefiles/County/c_02oc18.zip"
temp <- tempfile()
download.file(url = url, destfile = temp)
unzip(temp, exdir = "data/cwa")

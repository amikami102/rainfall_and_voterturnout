#-----------------------------------------
# This script downloads US county and state 
# cartographic boundary shape files from the 
# 2010 and 2000 censuses. We will add concactenated
# FIPS code to the attributes dataframe and then
# save the output as R object. 
#-----------------------------------------
# Only 2010 version for state cartographic boundary
# contains "GEO_ID". 
#-----------------------------------------
library(tidyverse)
library(rgdal, quietly = TRUE)


# read shapefiles and subset the attribute dataframe  
counties00 <- readOGR(file.path("data", "co99_d00_shp"), 
                      "co99_d00")@data %>%
        select(NAME, STATE, COUNTY, LSAD, LSAD_TRANS) 
counties10 <- readOGR(file.path("data", "gz_2010_us_050_00_500k"), 
                      "gz_2010_us_050_00_500k")@data %>%
        select(GEO_ID, NAME, STATE, COUNTY)
states <- readOGR(file.path("data", "gz_2010_us_040_00_500k"), 
                  "gz_2010_us_040_00_500k")@data 
save(counties00, counties10, states, file = "output/counties.Rdata")

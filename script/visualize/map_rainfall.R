###-------------------
# This script creates a gif animation of chloreoploth maps 
# plotting precipitation levels at county level for each year
# in the sample. Outputs files are 
# "rainfall2006.gif", "rainfall2008.gif", "rainfall2010.gif",
# "rainfall2012.fig", "rainfall2014.gif", "rainfall2016.gif". 
###--------------------

library(rio)
library(tmap)
library(magrittr)
library(tidyverse)
library(rgdal)
library(sp)
library(sf)

us_counties <- st_read("data/gz_2010_us_050_00_500k", "gz_2010_us_050_00_500k")


full <- list.files('output/rainfall/zonal_stat', 
           full.names = TRUE,
           pattern = "^county-rainfall2006-\\d{2}-\\d{2}.rds$") %>%
        map_df(~{
                .x %>% import() %>% 
                        mutate(date = str_extract(.x, "2006-[0-9]{2}-[0-9]{2}")) 
                }) 
us_counties %<>% left_join(full, by = "GEO_ID")


usa_main <- matrix(c(-125, 25, -66, 50), nrow = 2, byrow = FALSE)

tmap_mode('plot')
facet_anim <- tm_basemap(server = "OpenStreetMap")  + 
        tm_shape(us_counties, bbox = usa_main) + 
        tm_fill("rainfall", colorNA = NULL) +
        tm_facets(along = "date", ncol = 1, nrow = 1, drop.NA.facets = TRUE,
                  drop.empty.facets = TRUE, showNA = FALSE) + 
        tm_layout(legend.position = c("right", "bottom"))
tmap_animation(tm = facet_anim, 
               filename = "output/fig/rainfall2006.gif", 
               delay = 50)
magick::image_read("output/fig/rainfall2006.gif")

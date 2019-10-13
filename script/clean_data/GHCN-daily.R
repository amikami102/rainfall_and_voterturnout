###------------------------------------
# This script retrieves the precipitation data for the six
# days prior to the Election Day for the years
# 2006, 2008, 2010, 2012, 2014, and 2016. 
# Here are the overall steps:
# 1) obtain GHCN-Daily dataset via `rnoaa` package;
# 2) obtain geo. coordinates of weather stations via `rnoaa` package;
# 3) convert datasets to shapefiles and save to `data` folder. 
###------------------------------------
# ! Step 1 takes about 35 minutes to run. 
###------------------------------------
# Election dates: 
# 2006-11-07
# 2008-11-04
# 2010-11-02
# 2012-11-06
# 2014-11-04
# 2016-11-08
###------------------------------------
# The projection used is NAD 1983 as according to 
# the procedure in Cooperman (2017)'s supplementary material.
###--------------------------------------
# This script produces the following outputs:
# - "data/GHCND-daily/prcp_list.rds"
# - "data/GHCND-daily/stations_list.rds"
# - "data/GHCND-daily/prcp_data.Rdata"
###----------------------------------

library(rio)
library(tidyverse)
library(magrittr)
library(logging)
library(argparse)
library(rnoaa)
Sys.getenv("NOAA_KEY") #NOAA API key

# set up command line arguments 
parser <- ArgumentParser()
parser$add_argument("--output", default="data/GHCND-daily", 
                    type = "character", nargs = 1,
                    help = "Directory storing output data.")
args <- parser$parse_args()


# list the start and end date of the 7-day interval prior to the election
dates <- list("2006" = list(start = "2006-11-01", end = "2006-11-07"),
              "2008" = list(start = "2008-10-29", end = "2008-11-04"),
              "2010" = list(start = "2010-10-27", end = "2010-11-02"),
              "2012" = list(start = "2012-10-31", end = "2012-11-06"),
              "2014" = list(start = "2014-10-29", end = "2014-11-04"),
              "2016" = list(start = "2016-11-02", end = "2016-11-08")
)

get_prcp <- function(day){
        #------------------
        # Retrieves the precipitation data for 
        # a specific day.
        #-------------------
        # day (date, YYYY-MM-DD format)
        #--------------------
        loginfo(day)
        # create an empty dataframe
        df <- data.frame(station = character(), 
                         value = integer(), 
                         stringsAsFactors = FALSE) 
        # The maximum number of results displayed for each 
        # request is 1000. We want to count how many requests
        # are necessary to get all the results. 
        counter <- ncdc(datasetid = "GHCND", 
                        datatypeid =  "PRCP",
                        startdate = as.character(day),
                        enddate = as.character(day))$meta$totalCount %/% 1000
        for(i in 1:counter){
                safe_fun <- safely(ncdc) 
                response <- safe_fun(datasetid = "GHCND",
                                     datatypeid = "PRCP",
                                     startdate = as.character(day),
                                     enddate = as.character(day),
                                     limit = 10,
                                     offset = 10*i)
                df <- response$result$data %>%
                        select(station, value) %>% rbind(df)
                if(!is.null(response$error)){loginfo(response$error)}
        }
        return(df)
}


# `prcp_list` is a list of 6 elements, each element holding 7 dataframes
prcp_list <- dates %>% map(~ {
        days <- seq(from = as.Date(.x$start), 
                    to = as.Date(.x$end), by = "day")
        days %>% set_names(.) %>% map(~ get_prcp(.x))
}) 
export(prcp_list, file = file.path(args$output, "prcp_list.rds"))

###------------------------------------
get_stations <- function(start, end){
        #--------------------------------
        # Retrieve the geographical coordinates of
        # weather stations in the US whose data is available
        # for the observation period. 
        # Output is a dataframe with three columns: 
        # the station id, longitude, and latitude. 
        #-------------------------------
        # state (chr, start date in YYYY-MM-DD format)
        # end (chr, end date in YYYY-MM-DD format)
        #--------------------------------
        
        # count how many API requests are necessary to get all the results
        counter <- ncdc_stations(datasetid = "GHCND", 
                                 dattatypeid = "PRCP",
                                 startdate = start, 
                                 enddate = end,
                                 limit = 1000)$meta$totalCount %/% 1000
        # create empty dataframe to store output
        df <- data.frame(station = character(),
                         longitude = numeric(),
                         latitude = numeric())
        for (i in 1:counter){
                safe_fun <- safely(ncdc_stations)
                response <- safe_fun(datasetid = "GHCND",
                                     datatypid = "PRCP",
                                     startdate = start,
                                     enddate = end, 
                                     limit = 1000,
                                     offset = 1000*i)
                df <- response$result$data %>%
                        select(id, longitude, latitude) %>%
                        filter(grepl("GHCND:US", id)) %>%
                        rbind(df)
                if(!is.null(response$error)){loginfo(response$error)}
        }
        df %<>% filter(!duplicated(df)) # remove duplicates
        return(df)
}

# `stations_list` is a list of 6 elements, each element containing 
# the dataframe output from get_stations()
stations_list <- dates %>% map(~ get_stations(.x$start, .x$end))
export(stations_list, file.path(args$output, "stations_list.rds"))



get_joined <- function(prcp = prcp_list[['2006']], 
                       station_df = stations_list[['2006']]){
        #--------------------------------
        # This function joins the `stations` dataframe obtained
        # via `get_stations` function to 
        # each dataframe in the list created by `get_weekly`
        # function.
        #-------------------------------
        # prcp (list, a list containing the dataframes to be joined to)
        # station_df (dataframe, the dataframe containing station info)
        #-------------------------------- 
        
        names(prcp) %>% set_names(.) %>%
                map(~ left_join(prcp[[.x]], station_df, 
                                          by = c("station" = "id")) %>%
                    drop_na(longitude, latitude))
}

# `prcp_data` is identical to prcp_list except that the dataframes 
# have geocoordinate information for each station
prcp_data <- c('2006', '2008', '2010', 
               '2012', '2014', '2016') %>% set_names(.) %>%
                    map(~ get_joined(prcp = prcp_list[[.x]],
                                     station_df = stations_list[[.x]]))
export(prcp_data, file = file.path(args$output, "prcp_data.Rdata"))

###------------------------------------
library(sp) 
library(rgdal)

# get the NAD 1983 projection string 
nad1983 <- "http://spatialreference.org/ref/epsg/nad83/proj4/" %>%
        readLines(warn = FALSE)

prcp_data %>% map(~ {.x %>% imap(~ {
        
        # create spatial points dataframe object
        spdf <- SpatialPointsDataFrame(.x[, c("longitude", "latitude")],
                                       data = .x, 
                                       proj4string = CRS(nad1983))
        # save spdf as a shape file
        writeOGR(obj = spdf, 
                 dsn = file.path(args$output, "shp"), 
                 layer = paste0("rainfall", .y), 
                 driver = "ESRI Shapefile") 
                
        })
})






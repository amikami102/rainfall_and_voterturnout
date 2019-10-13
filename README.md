## Directory organization

```
|- data                         (read only)
|- man                          manuscript documents
|- literature                   (read only)
|- output
    |- cces_processed           processed CCES datasets 
    |- fig                      plots 
    |- rainfall
        |- points               rainfall*.{dbf,prj,shp,shx}
        |- points_reproj        rainfall*_reproj.{dbf,prj,shp,shx}
        |- krigoutput           krigout*.{tfw,tif,tif.aux.xml}
        |- raster_reproj        krigout*_reproj.{ovr,tfw,tif,xml}
        |- zonal_stat           county-rainfall*.{cpg,dbf,rds,xml} 
        |- reduced              rainfall_*.rds
|- script                       
    |- python
    |- R
|- shiny                        shiny app 
```

## Data sources and corresponding `R` scripts

The following table names the script that downloads and/or cleans the dataset and describes where the dataset comes from and/or what version was downloaded. All `R` scripts are stored in "script/R" while the raw datasets are stored in "data" directory. 

Script file | Dataset 
 ---- | ---------------------------------------------------------------
 `01_load_cces.R` | *CCES* (*Cooperative Congressional Electoral Survey*) for years 2006, 2008, 2010, 2012, 2014, 2016 ([home](https://cces.gov.harvard.edu/))[^cces] 
 `GHCND-daily.R` | GHCN (Global Historic Climate Network)-Daily for years 2006, 2008, 2010, 2012, 2014, 2016 downloaded via R package `rnoaa` version 0.8.4 ([CRAN](https://cran.r-project.org/package=rnoaa)); for more information on GHCN-Daily, see NOAA's [description](https://www.ncdc.noaa.gov/ghcn-daily-description) and [readme.txt](https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt)
 `counties.R` | US county cartographic boundary data from US Census Bureau ([page](https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html),  [ftp server](https://www2.census.gov/geo/tiger/GENZ2010/)); 2000 Census and 2010 Census
 `00_load_external_data.R` | US Census of Agriculture from US Department of Agriculture for 2002, 2007, 2012 ([ftp server](ftp://ftp.nass.usda.gov/quickstats/)); the 2017 Census is scheduled to be published in April 2019 ([2017 Census of Agriculture FAQ](https://www.nass.usda.gov/AgCensus/FAQ/2017/index.php)) 
 `00_load_external_data.R` | US County Warning Area counties ([page](https://www.weather.gov/gis/Counties), [2 April 2019 version](https://www.weather.gov/source/gis/Shapefiles/County/c_02ap19.zip), [2 October 2018 version](https://www.weather.gov/source/gis/Shapefiles/County/c_02oc18.zip))
 `acs_census_data.R` | US Census 5-year American Community Survey for years 2005 and 2010 via R package `acs` version 2.1.4 ([CRAN](https://CRAN.R-project.org/package=acs))
 

[^cces]: CCES data should be downloaded as ".dta" or ".tab". The option to download as ".Rdata" is available but should be avoided, for some variables will not be read correctly. For example, `birthyr` variable for CCES 2014 and 2016 appear as `<NA>` for all observations when the data is imported in ".Rdata" format. 


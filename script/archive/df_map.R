#--------------------------
# This script creates a customized map dataframe based on 
# the US county map data from tidyverse and then joins 
# together this map dataframe to CCES dataset.  
#-----------------------

source("counties.R")
df_map <- map_data("county") #from tidyverse package

# Capitalize the first letter of the state and county names,
# and rename variables. 
.simpleCap <- function(x) {
        # Function to capitalize the first letter of character string
        #----------------------
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
              sep = "", collapse = " ")
}
df_map <- df_map %>% mutate(region = sapply(region, .simpleCap),
                            subregion = sapply(subregion, .simpleCap)) %>%
                        rename(state = region, 
                                county = subregion)
df_map$name <- paste(df_map$county, df_map$state, sep = " - ")

### Add "Parish" to Louisiana's counties
.addParish <- function(df = df_map) {
        # Function to add "Parish" to Louisiana's counties
        #----------------------
        internal.fxn <- function(x){
                s <- strsplit(x, " - ")[[1]]
                paste(paste(s[1], "Parish"), s[2], sep = " - ")
        }
        La <- which(grepl("Louisiana", df$name))
        df[La, "name"] <- sapply(df$name[La], internal.fxn)
        return(df)
        
}
df_map <- .addParish()
# Give FIPS code to each county
df_map$FIPScode <- counties$fips[match(df_map$name,
                                       counties$name)]


# Join the map data to CCES datasts
map_cces2006 <- left_join(df_map, cces2006, by = 'FIPScode')
map_cces2008 <- left_join(df_map, cces2008, by = 'FIPScode')
map_cces2010 <- left_join(df_map, cces2010, by = 'FIPScode')
map_cces2012 <- left_join(df_map, cces2012, by = 'FIPScode')


# Save datasets as .rds files 
saveRDS(map_cces2006, file = "data/cces_data/map_cces2006.rds")
saveRDS(map_cces2008, file = "data/cces_data/map_cces2008.rds")
saveRDS(map_cces2010, file = "data/cces_data/map_cces2010.rds")
saveRDS(map_cces2012, file = "data/cces_data/map_cces2012.rds")

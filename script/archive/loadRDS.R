##---------------------
# The script defines functions to
# import and export CCES datasets. 
# These functions are used before and
# after each step of data cleaning.
##----------------------


save_rds <- function(cces_df, yr){
        #--------------------
        # cces_df (dataframe, the cces dataset to save)
        # yr (int, the year of the cces dataset)
        #--------------------
        require(rio)
        file_name <- sprintf("cces%d.rds", yr)
        outputdir <- file.path("output", "cces_processed")
        export(df, file = file.path(outputdir, file_name))
}

load_rds <- function(yr){
        #--------------------
        # yr (int, the year of the cces dataset)
        #--------------------
        require(rio)
        file_name <- sprintf("cces%d.rds", yr)
        inputdir <- file.path("output", "cces_processed")
        df <- import(file = file.path(inputdir, file_name))
        return(df)
}

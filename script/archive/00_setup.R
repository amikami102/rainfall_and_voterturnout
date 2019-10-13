#!/usr/bin/Rscript
###-------------------------
# Load favorite data and set up logging.
###-------------------------
library(argparse, quietly = TRUE)
library(logging, quietly = TRUE)


# set up argument parser 
parser <- ArgumentParser(description='weather voting replication shared')
parser$add_argument('--outdir', type = "character",
                    nargs='?',
                    help = "Directory to store output files.",
                    default = "./output")
parser$add_argument('--indir', type = "character",
                    nargs = '?',
                    help = "Directory storing data files to read.",
                    default = "./data")




# set up logging
logReset()
basicConfig()

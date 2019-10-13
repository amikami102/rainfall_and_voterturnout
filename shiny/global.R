library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)



source("theme_map.R")

map_cces2006 <- readRDS("data/map_cces2006.rds")
map_cces2008 <- readRDS("data/map_cces2008.rds")
map_cces2010 <- readRDS("data/map_cces2010.rds")
map_cces2012 <- readRDS("data/map_cces2012.rds")

race <- c("% White", "% Black", "% Hispanic", "% Asian")
income <-  c("% No HS", "% HS graduate", 
             "% Some college and above")
educ <- c("% less than $30k", "% less than 60k", 
          "% less than $80k",
          "% less than 120k", "% $120k and above")

vars <- c("Rainfall (inches)", "Rainfall (index)", "Rainfall (SPI)",
          race,
          income,
          educ,
          "Sample count", 
          "Did you vote this November?",
          "Do you intend to vote this November?")
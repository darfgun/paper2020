# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "tictoc","tigris", "sf","acs")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
  
}

#TODO l?schen
year <- 2011
agr_by <- "county"

#tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
#tracDir <- "/Users/default/Desktop/own_code2/data/02_tracts"
#exp_tracDir <- "/Users/default/Desktop/own_code2/data/03_exp_tracts"
#censDir <- "/Users/default/Desktop/own_code2/data/06_demog"
#cens_agrDir <- "/Users/default/Desktop/own_code2/data/07_dem.agr"

#tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"
#tracDir <-  "C:/Users/Daniel/Desktop/paper2020/data/02_tracts"
#exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"
#censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
#cens_agrDir <- "C:/Users/Daniel/Desktop/paper2020/data/07_dem.agr"

library(tidycensus)
library(tidyverse)
library(viridis)
library(acs)
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef" 
census_api_key(key)

tracts <- get_acs(geography = "tract", 
                  variables = "B19013_001", #dummy variable
                state = "CA",  
                year = year,
                geometry = TRUE,
                keep_geo_vars = TRUE)

sapply(tracts$geometry, function(tract) {
  # get enclosing box, make sure in range of exposure data
  bbox <- st_bbox(tract)
  long_min <- bbox$xmin 
  })

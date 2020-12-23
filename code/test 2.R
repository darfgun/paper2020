# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "tictoc","tigris", "sf")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
  
}

#TODO l?schen
year <- 2000
agr_by <- "county"

#tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
#tracDir <- "/Users/default/Desktop/own_code2/data/02_tracts"
#exp_tracDir <- "/Users/default/Desktop/own_code2/data/03_exp_tracts"
#censDir <- "/Users/default/Desktop/own_code2/data/06_demog"
#cens_agrDir <- "/Users/default/Desktop/own_code2/data/07_dem.agr"

tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"
tracDir <-  "C:/Users/Daniel/Desktop/paper2020/data/02_tracts"
exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"
censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
cens_agrDir <- "C:/Users/Daniel/Desktop/paper2020/data/07_dem.agr"

reshape(trac_cens_expData, 
        direction = "long",
        varying = list(names(d)[3:7]),
        v.names = "Value",
        idvar = c("Code", "Country"),
        timevar = "Year",
        times = 1950:1954)

long <- melt(setDT(trac_cens_expData), 
             id.vars = c("state", "county", "tract", "AFFGEOID", "pm"),
             variable.name = "pop_size")

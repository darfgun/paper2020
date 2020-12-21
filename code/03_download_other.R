#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download required data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "RCurl", "magrittr", "tigris", "stringr", "data.table", "tidyverse", "tictoc","rhdf5") #

options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
expDir <- args[4]
tracDir <- args[5]


#----------------------download exposure data-----------------
filenameExp <- paste0(toString(year), ".h5")
filepathExp <- file.path(expDir, filenameExp)

if (!file.exists(filepathExp)) {
  url <- "ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"
  tic(paste("Successfully downloaded PM exposure data for", year))
  download.file(paste0(url, filenameExp), filepathExp)
  toc()
}

# save useful variable for estimations later on
filepathM <- paste0("m_exp_", toString(year), ".RData") %>%
  file.path(tmpDir, .)

if (!file.exists(filepathM)) {
  exp_data <- H5Fopen(filepathExp)

  long_vec <- c(as.matrix(exp_data$longitude)) # TODO optimize?
  lat_vec <- c(as.matrix(exp_data$latitude))

  n_long <- length(long_vec) - 1
  long_dif <- long_vec[2:(1 + n_long)] - long_vec[1:n_long]
  m_min_long <- min(long_dif)
  m_max_long <- max(long_dif)

  n_lat <- length(lat_vec) - 1
  lat_dif <- lat_vec[2:(1 + n_lat)] - lat_vec[1:n_lat]
  m_min_lat <- min(lat_dif)
  m_max_lat <- max(lat_dif)

  save(m_min_long, m_max_long, m_min_lat, m_max_lat, file = filepathM)
}

rm(filenameExp, filepathExp, filepathM)



### ------------------download tract shape files--------------------
filepathTr <- file.path(tracDir, toString(year))
dir.create(filepathTr, recursive = T, showWarnings = F)

glimpse(states)
print("test")
apply(states, 1, function(state) {
  glimpse(state)
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  filepathTrX <- paste0("tracts_", toString(year), "_", STUSPS, ".rds") %>%
    file.path(filepathTr, .)

  if (!file.exists(filepathTrX)) {
    tic(paste("Downloaded census tracts shape files for", year, name))
    # TODO fallunterscheidung

    tracts <- tracts(state = STUSPS, cb = TRUE, year = year)
    saveRDS(tracts, filepathTrX)
    toc()
  }
})

rm(filepathTr)

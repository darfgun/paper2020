
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c(
  "dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse",
  "tigris", "tictoc", "cdcfluview", "testthat", "rlang"
)

options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

# TODO l?schen
#year <- 2010

# censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
# tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"

#tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
#censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

# quits, if not downloadable year
if (!year %in% c(2000, 2010:2016)) {
  print(paste("can not download census data for", year))
  quit()
}

## ----------download useful data to tmp-------------------------------------------------------------------------------

states <- read.csv(file.path(tmpDir, "states.csv"))


### ------------------------download demographic data-----------------------------------------------------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef"
Sys.setenv(CENSUS_KEY = key)

census_meta <- read.csv(file.path(censDir, "meta", paste0("cens_meta_", toString(year), ".csv")))
# identify relevant variables
relevant_variables <- census_meta$variable %>% unique()

## ---------------- download sex by age for each race----------------------
censDir <- file.path(censDir, year)
dir.create(censDir, recursive = T, showWarnings = F)

# relevant groups for each year and table names
if (year == 2000) {
  # decennical census, sex by age for races
  groups <- c("P012A", "P012B", "P012C", "P012D", "P012E", "P012I", "PCT012J", "PCT012K", "PCT012L", "PCT012M")
  tablename <- "dec/sf1"
} else if (year == 2010) {
  # decennical census, sex by age for races
  groups <- c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I", "PCT12J", "PCT12K", "PCT12L", "PCT12M")
  tablename <- "dec/sf1"
} else if (year %in% 2011:2016) {
  # american community survey, sex by age for races
  groups <- c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E", "B01001H")
  # "not hispanic or latino" only available for white
  tablename <- "acs/acs5"
}

tic(paste("Downloaded census data in year", toString(year)))
# loop over all states
apply(states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric()
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  dem.state.dir <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(censDir, .)

  # download if does not exist yet
  if (!file.exists(dem.state.dir)) {
    tic(paste("Downloaded census data in year", toString(year), "in", name))

    # loop over all groups, download data
    dem.state.data <- lapply(groups, function(group) {
      tic(paste("Downloaded census data in year", toString(year), "in", name, "for group", group))
      data <- getCensus(
        name = tablename,
        vintage = year,
        vars = paste0("group(", group, ")"),
        region = "tract:*",
        regionin = sprintf("state:%02d", STATEFP)
      )

      # subset relevant part of GEO_ID
      data$GEO_ID <- data$GEO_ID %>%
        str_sub(., -11, -1)

      data <- data %>%
        select(
          any_of(c(relevant_variables, "state", "county", "tract", "GEO_ID"))
        ) %>%
        pivot_longer(
          cols = !c("state", "county", "tract", "GEO_ID"),
          names_to = "variable",
          values_to = "pop_size"
        )
      toc()
      return(data)
    }) %>%
      do.call(rbind, .) %>%
      as.data.frame()

    # save data
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)

    # make wider
    tic(paste("Made additional calculations with census data in year", toString(year), "in", name))
    census_meta_sub <- census_meta %>% filter(downloaded == FALSE)

    # show progress
    pb <- txtProgressBar(1, nrow(census_meta_sub), style = 3)
    for (i in 1:nrow(census_meta_sub)) {
      var <- census_meta_sub[i, "variable"]

      # parse String "A|B|C..." to vector c(A,B,C,...)
      tot_var <- census_meta_sub[i, "tot_var"] %>%
        strsplit(., "|", fixed = TRUE) %>%
        unlist()

      ntot_var <- census_meta_sub[i, "ntot_var"] %>%
        strsplit(., "|", fixed = TRUE) %>%
        unlist()

      # calculate difference
      dem.state.data.tot <- dem.state.data %>%
        filter(variable %in% tot_var) %>%
        group_by(state, county, tract, GEO_ID) %>%
        summarise(pop_size_tot = sum(pop_size))

      dem.state.data.ntot <- dem.state.data %>%
        filter(variable %in% ntot_var) %>%
        group_by(state, county, tract, GEO_ID) %>%
        summarise(pop_size_ntot = sum(pop_size))
      
      dem.state.data.dif <- full_join(dem.state.data.tot,dem.state.data.ntot,
                                      by = c("state", "county", "tract", "GEO_ID")) 

      dem.state.data.dif <- dem.state.data.dif %>%
        mutate(
          variable = var,
          pop_size = max(0, pop_size_tot - pop_size_ntot),
          pop_size_tot = NULL,
          pop_size_ntot = NULL,
        )

      dem.state.data <- rbind(dem.state.data, dem.state.data.dif)
      setTxtProgressBar(pb, i)
    }

    # filter relevant variables
    relevant_variables <- census_meta %>%
      filter(relevant == TRUE) %>%
      select(variable) %>%
      unlist()

    dem.state.data <- dem.state.data %>%
      filter( # !is.na(pop_size), #TODO
        variable %in% relevant_variables
      )

    # test basic properties
    test_that("02_download end", {
      expect_false(any(is.na(dem.state.data)))
      expect_true(all(dem.state.data$pop_size >= 0))
    })
    toc()

    # save demographic data in seperate file for each state
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)
    toc()
  }
})
toc()
""
# file.path("tests","test_01_download_cens.R")
# test_file("test_01_download_cens.R")
# test_file("C:\Users\Daniel\Desktop\paper2020\code\tests\test_01_download_cens.R")

#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: aggregate 
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_tracDir <- args[7]
censDir <- args[8]
cens_agrDir <- args[9]
agr_by <- args[10]

# TODO l?schen
year <- 2016
agr_by <- "county"

# tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
# exp_tracDir <- "/Users/default/Desktop/own_code2/data/03_exp_tracts"
# censDir <- "/Users/default/Desktop/own_code2/data/06_demog"
# cens_agrDir <- "/Users/default/Desktop/own_code2/data/07_dem.agr"

tmpDir <- "C:/Users/Daniel/Desktop/paper2020/data/tmp"
exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"
censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
cens_agrDir <- "C:/Users/Daniel/Desktop/paper2020/data/07_dem.agr"

if (!agr_by %in% c("county", "Census_Region", "Census_division", "hhs_region_number", "state", "nation")) {
  print(paste(agr_by, "is an invalid agr_by argument"))
  quit()
}

cens_agrDirC <- cens_agrDir %>% file.path(., "county", year)
dir.create(cens_agrDirC, recursive = T, showWarnings = F)

cens_agrDir <- cens_agrDir %>% file.path(., agr_by, year)
dir.create(cens_agrDir, recursive = T, showWarnings = F)

# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>% read.csv()


## ----

apply(states, 1, function(state) {
  STUSPS <- state[["STUSPS"]] 
  name <- state[["NAME"]]

  cens_agrDirCX <- paste0("cens_agr_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(cens_agrDirC, .)

  if (!file.exists(cens_agrDirCX)) {
    tic(paste("Aggregated Census data in", name, "in year", year, "by pm and", agr_by))
    trac_censData <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
      file.path(censDir, year, .)%>%
      read.csv %>%
      
      setnames("GEO_ID", "AFFGEOID") %>%
      #TODO löschen
      #filter out tracts, where no one is living
      #group_by(AFFGEOID) %>%
      #summarise(anyOneLiving = sum(pop_size))%>%
      #filter(anyOneLiving != 0) %>%
      #mutate(anyOneLiving = NULL)%>%
      #ungroup %>%
      #as.data.frame%>%
      
      pivot_wider(
        names_from = variable,
        values_from = pop_size
      ) 
    

    exp_tracData <- paste0("exp_trac_", toString(year), "_", STUSPS, ".csv") %>%
      file.path(exp_tracDir, year, .) %>%
      read.csv()
    
    if(anyNA.data.frame(exp_tracData))
      browser()
    if(anyNA(exp_tracData$pm))
      browser()

    if (nrow(exp_tracData) != nrow(trac_censData)) warning("exp_tracData and trac_censData should have same number of rows in 06_aggregate")

    cens_agr <- left_join(trac_censData,
      exp_tracData,
      by = "AFFGEOID"
    ) %>%
      setDT() %>%
      melt(
        id.vars = c("state", "county", "tract", "AFFGEOID", "pm"),
        variable.name = "variable"
      ) %>%
      group_by(state, county, variable, pm) %>%
      summarise(pop_size = sum(value))
    
    if(anyNA.data.frame(exp_tracData))
      browser()
    if(anyNA(cens_agr$pm))
      browser()
    
    # add proportions
    cens_agr <- cens_agr %>%
      group_by(state, county, variable) %>%
      summarise(totals = sum(pop_size)) %>%
      filter(totals != 0)  %>%
      inner_join(cens_agr) %>%
      mutate(prop = pop_size / totals)
    
    if(anyNA.data.frame(exp_tracData))
      browser()
    if(anyNA(cens_agr$pm))
      browser()
    
    # TODO test, delete
     cens_agr2 <- cens_agr %>%
      group_by(state, county, variable) %>%
      summarise(sum_prop = sum(prop))

    write.csv(cens_agr, cens_agrDirCX)
    toc()
  }
})

##--- different procedure if not county level
if (agr_by != "county") {
  regions <- states[, agr_by] %>% unique
  
  for (region in regions) {
    cens_agrDirX <- paste0("cens_agr_", toString(year), "_", region, ".csv") %>%
      file.path(cens_agrDir, .)

    if (!file.exists(cens_agrDirX)) { 

      tic(paste("Aggregated Census data", region, "in year", year, "by pm and", agr_by))
      statesX <- states[states[, agr_by] == region, "STUSPS"]

      cens_agr <- lapply(statesX, function(STUSPS) {
        paste0("cens_agr_", toString(year), "_", STUSPS, ".csv") %>%
          file.path(cens_agrDirC, .) %>%
          read.csv 
      }) %>%
        do.call(rbind, .) %>%
        as.data.frame() %>%
        group_by(variable, pm) %>%
        summarise(pop_size = sum(pop_size))

      # add proportions
      cens_agr <- cens_agr %>%
        group_by(variable) %>%
        summarise(totals = sum(pop_size)) %>%
        filter(totals != 0)
        inner_join(cens_agr) %>%
        mutate(prop = pop_size / totals)

      #if (any(is.na(cens_agr$prop))) { #TODO löschen
      #  glimpse(cens_agr$prop)
      #}

      # add region
      cens_agr[, agr_by] <- region

      # select relevant
      cens_agr <- cens_agr %>% select(agr_by, variable, pm, prop)
      write.csv(cens_agr, cens_agrDirX)
      toc()
    }
  }
}

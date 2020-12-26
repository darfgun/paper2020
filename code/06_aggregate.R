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
packages <- c("dplyr", "magrittr", "data.table", "testthat","tidyverse", "tictoc")

for (p in packages) {
  suppressWarnings(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_tracDir <- args[7]
censDir <- args[8]
cens_agrDir <- args[9]
agr_by <- args[10]

# TODO l?schen
year <- 2012
agr_by <- "nation"

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
    tic(paste("Aggregated Census data in", name, "in year", year, "by pm and county"))
    trac_censData <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
      file.path(censDir, year, .)%>%
      read.csv %>%
      setnames("GEO_ID", "AFFGEOID") %>%
      pivot_wider(
        names_from = variable,
        values_from = pop_size
      ) 
    
    exp_tracData <- paste0("exp_trac_", toString(year), "_", STUSPS, ".csv") %>%
      file.path(exp_tracDir, year, .) %>%
      read.csv()
    
    if (nrow(exp_tracData) != nrow(trac_censData)) warning("exp_tracData and trac_censData should have same number of rows in 06_aggregate")
    
    cens_agr <- left_join(trac_censData,
                          exp_tracData,
                          by = "AFFGEOID"
    )  %>%
      setDT() %>%
      melt(
        id.vars = c("state", "county", "tract", "AFFGEOID", "pm"),
        variable.name = "variable"
      ) %>%
      group_by(state, county, variable, pm) %>%
      summarise(pop_size = sum(value)) %>%
      filter(pop_size != 0)
    
    cens_agr <- cens_agr  %>%
      group_by(state, county, variable) %>%
      summarise(totals = sum(pop_size)) %>%
      filter(totals != 0)  %>%
      inner_join(cens_agr, by = c("state", "county", "variable")) %>%
      mutate(prop = pop_size / totals)
    
    #test, check 
    test_that("06_aggregate county", {
      cens_agr %>%
        group_by(state, county, variable) %>%
        summarise(sum_prop = sum(prop)) %>%
        apply(1,function(row){
          expect_equal(1,row[["sum_prop"]] %>% as.numeric)
        })
      expect_equal(any(is.na(cens_agr)), FALSE)
    })
    
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
      tic(paste("Aggregated Census data in",agr_by, region, "in year", year, "by pm"))
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
        inner_join(cens_agr, by = "variable") %>%
        mutate(prop = pop_size / totals)
      
      #test, check 
      test_that("06_aggregate agr_by", {
        cens_agr %>%
          group_by(variable) %>%
          summarise(sum_prop = sum(prop)) %>%
          apply(1,function(row){
            expect_equal(1,row[["sum_prop"]] %>% as.numeric)
          })
        expect_equal(any(is.na(cens_agr)), FALSE)
      })
      
      # add region
      cens_agr[, agr_by] <- region
      
      write.csv(cens_agr, cens_agrDirX)
      toc()
    }
    
    if(TRUE){
      census_meta <-  file.path(censDir,"meta", paste0("cens_meta_", toString(year), ".csv")) %>% read.csv
      
      cens_agr_plotDir <- paste0("cens_agr_", toString(year), "_", region) %>%
        file.path(cens_agrDir, .)
      if (!file.exists(cens_agr_plotDir)) { 
        tic(paste("Plotted aggregated Census data in",agr_by, region, "in year", year, "by pm"))
        dir.create(cens_agr_plotDir, recursive = TRUE)
        cens_agr <- cens_agrDirX %>% 
                      read.csv %>%
                      left_join(.,census_meta, by = "variable") %>%
                      group_by(race,hispanic_origin, pm) %>%
                      summarise(pop_size = sum(pop_size))
        
        for(his_or in unique(cens_agr$hispanic_origin)){
          #totals
          
        }
        #TODO
        toc()
      }
      
    }
  }
}


""
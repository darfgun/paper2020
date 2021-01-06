
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "censusapi", "stringr", "data.table", "tidyverse", 
              "tigris", "tictoc", "cdcfluview","testthat","rlang")

options(tigris_use_cache = FALSE)
for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

#TODO l?schen
year <- 2010

#censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
#tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"

tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

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
dir.create( censDir, recursive = T, showWarnings = F)

# relevant groups for each year and table names
if (year == 2000) {
  # decennical census, sex by age for races
  groups <- c("P012A", "P012B", "P012C", "P012D","P012E", "P012I","PCT012J", "PCT012K", "PCT012L", "PCT012M") 
  tablename <- "dec/sf1"
} else if (year == 2010) {
  # decennical census, sex by age for races
  groups <- c("PCT12A", "PCT12B", "PCT12C", "PCT12D", "PCT12E", "PCT12I", "PCT12J", "PCT12K", "PCT12L", "PCT12M") 
  tablename <- "dec/sf1"
} else if (year %in% 2011:2016) {
  # american community survey, sex by age for races
  groups <- c("B01001A", "B01001B", "B01001C", "B01001D", "B01001E", "B01001H")
  #"not hispanic or latino" only available for white
  tablename <- "acs/acs5"
}

tic(paste("Downloaded census data in year", toString(year)))
# loop over all states
apply(states, 1, function(state) {
  STATEFP <- state["STATEFP"] %>% as.numeric
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]

  dem.state.dir <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
    file.path(censDir, .)
  
  #download if does not exist yet
  if (!file.exists(dem.state.dir)) {
    tic(paste("Downloaded census data in year", toString(year), "in", name))
    
    #loop over all groups, download data
    dem.state.data <- lapply(groups, function(group) {
      tic(paste("Downloaded census data in year", toString(year), "in", name, "for group", group))
      data <- getCensus(
        name = tablename,
        vintage = year,
        vars = paste0("group(", group, ")"),
        region = "tract:*",
        regionin = sprintf("state:%02d", STATEFP)
      ) 
      
      #create GEO_ID, if it does not exist yet
      if("GEO_ID" %in% colnames(data)){
        data$GEO_ID<- data$GEO_ID %>% str_sub(.,-11,-1)
      }else{
        data$GEO_ID <-paste0(formatC(data$state, width=2, flag="0"),
                             formatC(data$county, width=3, flag="0"),
                             formatC(data$tract, width=6, flag="0")
        ) 
      }
      #https://stackoverflow.com/questions/8266915/format-number-as-fixed-width-with-leading-zeros
      
      data<- data%>%
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
    
    #save data
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)
    dem.state.data <- dem.state.dir %>% read.csv()
    
    #make wider
    tic(paste("Made additional calculations with census data in year",toString(year), "in", name))
    dem.state.data <- dem.state.data %>%
      group_by(variable) %>%
      mutate(row = row_number()) %>%
      pivot_wider(
        names_from = variable,
        values_from = pop_size,
        values_fill = 0 #TODO legitimate to fill in this way?
      ) %>%
      select(-row)
    
    census_meta_sub <- census_meta %>% filter(downloaded == FALSE)

    for (i in 1:nrow(census_meta_sub)) {
      var <- census_meta_sub[i, "variable"] 
      
      #parse String "A|B|C..." to vector c(A,B,C,...)
      tot_var <- census_meta_sub[i, "tot_var"] %>%
        strsplit(., "|", fixed = TRUE) %>%
        unlist()
      
      ntot_var <- census_meta_sub[i, "ntot_var"] %>%
        strsplit(., "|", fixed = TRUE) %>%
        unlist()
      
      #row sum all columnds
      if(rlang::is_empty(tot_var)){
        data_tot <-0
      }else{
        data_tot <- dem.state.data[, tot_var, drop = FALSE] %>%
          apply(., 2, as.numeric) %>%
          rowSums() %>%
          unlist()
      }
      
      if(rlang::is_empty(ntot_var)){
        data_ntot <-0
      }else{
        data_ntot <- dem.state.data[, ntot_var, drop = FALSE] %>%
          apply(., 2, as.numeric) %>%
          rowSums() %>%
          unlist()
      }

      #calculate "Hispanic or latino" = "all" - "not hispanic or latino"
      dem.state.data[, var] <- data_tot - data_ntot 
    }
    
    #drop Asian Alone, Pacific Islander alone in favor of combined race "ASIAn OR Pacific islander"
    census_meta_sub <- census_meta %>% 
      filter(race %in%c("ASIAN","NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER")) %>%
      select(variable)%>%
      unlist
    
    dem.state.data<-dem.state.data %>%
              select(!census_meta_sub) #TODO
    
    #longer again
    dem.state.data <- dem.state.data %>%
      pivot_longer(
        cols = !c("state", "county", "tract", "GEO_ID"),
        names_to = "variable",
        values_to = "pop_size"
      ) %>% #TODO
      filter(!is.na(pop_size))
    toc()
    
    #save demographic data in seperate file for each state
    fwrite(dem.state.data, dem.state.dir, row.names = FALSE)
    toc()
  }
})
toc()
""
#file.path("tests","test_01_download_cens.R")
#test_file("test_01_download_cens.R")
#test_file("C:\Users\Daniel\Desktop\paper2020\code\tests\test_01_download_cens.R")
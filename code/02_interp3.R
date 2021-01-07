#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr","tidyr", "magrittr","stringr","data.table","tictoc","foreign")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
year <- args[1]
tmpDir <- args[3]
censDir <- args[8]

year<-2001
tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

if (!year %in% 2001:2009) {
  print(paste("can not interpolate census data for", year))
  quit()
}

states <- file.path(tmpDir, "states.csv") %>% read.csv

crosswalk <- read.dta(file.path(tmpDir,"crosswalk_2010_2000.dta"))

crosswalk<-crosswalk[,c("trtid00","trtid10","weight")]

censDir00 <- file.path(censDir, "2000")
censDir10 <- file.path(censDir, "2010")

censDir00_in10<-file.path(censDir, "2000_in_2010")
dir.create(censDir00_in10, recursive = T, showWarnings = F)

##-----calculate-----

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  censDir00_in10_X <- file.path(censDir00_in10, paste0("census_2000_", STUSPS, ".csv"))
  
  if(!file.exists(censDir00_in10_X)){
    tic(paste("calculated 2000 demographic census data in 2010 boundaries in",name))
    #read demographic census data by tract, make data wider
    trac_censData00 <-  file.path(censDir00, paste0("census_2000_", STUSPS, ".csv"))%>%
      read.csv(colClasses=c(pop_size="numeric"))

    trac_censData00 <-trac_censData00 %>%
      left_join(crosswalk,by)
    write.csv(trac_censData00,censDir00_in10_X)
    toc()
  }

  })

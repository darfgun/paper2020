#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr","tidyr", "magrittr","stringr","data.table","tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
censDir <- args[1]
tmpDir <- args[2]
year <- args[3]

year<-2001
tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

if (!year %in% 2001:2009) {
  print(paste("can not interpolate census data for", year))
  quit()
}

states <- file.path(tmpDir, "states.csv") %>% read.csv

crosswalk <- file.path(tmpDir,"crosswalk_2000_2010.csv") %>% 
  read.table(., header=TRUE) %>%
  setnames("trtid00.","trtid00") %>%
  mutate(trtid00= str_sub(trtid00,1,11))

crosswalk<-crosswalk%>%
        select(trtid00,trtid10) %>%
        mutate(trtid00_long =paste0("1400000US",trtid00),
               trtid10_long =paste0("1400000US",trtid10))

censDir00 <- file.path(censDir, "2000")
censDir10 <- file.path(censDir, "2010")

censDir10_in00<-file.path(censDir, "2010_in_2000")
dir.create(censDir10_in00, recursive = T, showWarnings = F)

##-----calculate-----

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  censDir10_in00X <- file.path(censDir10_in00, paste0("census_2010_", STUSPS, ".csv"))
  if(!file.exists(censDir10_in00X)){
    tic(paste("calculated 2010 demographic census data in 2000 boundaries in",name))
    #read demographic census data by tract,
    censData10 <-  file.path(censDir10, paste0("census_2010_", STUSPS, ".csv"))%>%
      read.csv(colClasses=c(pop_size="numeric"))
    
    censData10 <- censData10 %>% 
      left_join(crosswalk, by=c("GEO_ID"="trtid10_long" )) %>%
      group_by(trtid00_long,variable)#%>%
      #summarise()
    toc()
  }
  })

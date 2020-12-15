#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing 
packages <- c("dplyr", "magrittr")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts=FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly=T)
censDir <- args[1]
tmpDir <- args[2]
year <- args[3]

if(!year %in% 2001:2009){
  print(paste("can not interpolate census data for", year))
  quit()
}

states <- file.path(tmpDir, "states.csv") %>% read.csv

censMetaDir <- file.path(censDir,"meta")
censDir00 <- file.path(censDir,"2000") 
censDir10 <- file.path(censDir,"2010")
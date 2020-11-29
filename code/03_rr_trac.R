#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: assign RR by cause, age to each census tract
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing 

packages <- c("magrittr")

options(tidyverse.quiet = TRUE)
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts=FALSE))
}

# Pass in arguments
args <- commandArgs(trailingOnly=T)
year <- args[1]
tmpDir <- args[3]
exp_tracDir <- args[6]
exp_rrDir <- args[7]
trac_rrDir <- args[8]


##---------------load data---------------
states <- file.path(tmpDir, "states.csv") %>% read.csv

exp_tracDir <- exp_tracDir %>% file.path(., toString(year))

#create folder, where calculations will be stored
trac_rrDir <- file.path(trac_rrDir, toString(year))
dir.create(trac_rrDir, recursive = T, showWarnings = F)

##-----------------calculation---------------

#create age sex groups
ages <- c(9:20,30:32,235) #20+
sexes <- c(1,2)    #females & males, change for GBD2017
cause <- 671  #cataract
cause_name <- "sense_cataract"
measure <- 3   #yld




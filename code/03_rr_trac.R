#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: assign RR by cause, age to each census tract
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
#clear memory
rm(list=ls(all=TRUE))

set.seed(0)
# load packages, install if missing
packages <- c("magrittr","tictoc","MALDIquant","dplyr","tidyverse")

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
tic(paste("Assigned RR to each tract for year",toString(year)))
#loop over all states
apply(states, 1, function(state){
  STUSPS<-state[4]
  name<-state[5]
  
  #path, where result should be stored
  trac_rrDirX <- paste0("trac_rr_",toString(year),"_",STUSPS,".csv") %>%
    file.path(trac_rrDir, .)
  
  #quit execution, if already calculated
  if (file.exists(trac_rrDirX)){
    return()
  }
  
  exp_trac <- paste0("exp_trac_",toString(year),"_",STUSPS,".csv") %>%
    file.path(exp_tracDir, .) %>%
    read.csv
  
  trac_rr<-apply(causes_ages,1, function(cause_age)
  { 
      label_cause <- cause_age[1]
      age_group_id <- cause_age[2]
      
      exp_rr <- ifelse(age_group_id == "all ages",
                       paste0(label_cause,".csv"),
                       paste0(label_cause,"_",age_group_id,".csv")) %>%
                    file.path(exp_rrDir,.) %>%
                    read.csv
      
      getRR <- function(pm) match.closest(pm, exp_rr$exposure_spline) %>% exp_rr[.,'rr'] %>% return(.)
    
      trac_rrX<-exp_trac %>%
        mutate(label_cause = label_cause,
               age_group_id = age_group_id,
               rr = getRR(pm))

      return(trac_rrX)
      }) %>% do.call(rbind,.) %>% as.data.frame #TODO
  
  ##-----save as csv--------
  write.csv(trac_rr,trac_rrDirX, row.names = FALSE)
})

toc()

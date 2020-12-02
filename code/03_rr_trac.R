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

#TODO
#tmrelsDir <- file.path(tmpDir, "tmrels.csv")
#if(file.exists(tmrelsDir)){
#  tmrels <- read.csv(tmrelsDir)[[1]]
#}else{
#  tmrels <-runif(100, min= 2.4, max= 5.9) %>%
#                round(digits = 2)
#  write.csv(tmrels,tmrelsDir, row.names = FALSE)
#}

tmrel <- mean(2.4,5.9)

##---either load data or write it----
#write useful overview over causes
causes_agesDir <- file.path(tmpDir, "causes_ages.csv")

if(file.exists(causes_agesDir)){
  causes_ages <- read.csv(causes_agesDir)
}else{
  #Chronic obstructive pulmonary disease, ? ,lower respiratory infections, ?, type 2 diabetes
  causes_all_ages <-c("resp_copd", "lbw", "lri","neo_lung","ptb","t2_dm")
  causes_age_specific<-c("cvd_ihd","cvd_stroke")
  
  age_ids <-  seq.int(25, 95, 5) #TODO assuming folks do not get older
  
  causes_ages <- data.frame(label_cause = rep(causes_age_specific, each = length(age_ids)),
                            age_group_id = rep(age_ids, times=length(causes_age_specific)))
  
  causes_ages<- data.frame(label_cause = causes_all_ages,
                           age_group_id = rep("all ages", each=length(causes_all_ages)) )%>%
    rbind(causes_ages)
  
  write.csv(causes_ages,causes_agesDir, row.names = FALSE)
}

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
      #%>% subset('exposure_spline','mean') #TODO
      
      getMRBRT <- function(pm) match.closest(pm, exp_rr$exposure_spline) %>% exp_rr[.,'mean'] %>% return(.)
      
      tmrelMRBR <- getMRBRT(tmrel)
      
      #get cause and age specific relative risk for pm exposure
      getRR <-function(pm){
        ifelse(pm<=tmrel,
               1,
               getMRBRT(pm)/tmrelMRBR) %>%
          return(.)}
    
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

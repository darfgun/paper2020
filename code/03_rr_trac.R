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
set.seed(0)

packages <- c("magrittr","tictoc","MALDIquant")

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

tmrelsDir <- file.path(tmpDir, "tmrels.csv")
if(file.exists(tmrelsDir)){
  tmrels <- read.csv(tmrelsDir)
}else{
  tmrels <-runif(1000, min= 2.4, max= 5.9) %>%
                round(digits = 2)
  write.csv(tmrels,tmrelsDir, row.names = FALSE)
}
#else tmrel <- mean(2.4,5.9)
##-----------------calculation---------------


#https://stackoverflow.com/questions/12379128/r-switch-statement-on-comparisons #TODO

causes_agesDir <- file.path(tmpDir, "causes_ages.csv")

if(file.exists(causes_agesDir)){
  causes_ages <- read.csv(causes_agesDir)
}else{
  #Chronic obstructive pulmonary disease, ? ,lower respiratory infections, ?, type 2 diabetes
  causes_all_ages <-c("resp_copd","lbw", "lri","neo_lung","ptb","t2_dm")
  causes_age_specific<-c("cvd_ihd","cvd_stroke")
  
  age_ids <-  seq.int(25, 95, 5) #TODO assuming folks do not get older
  
  causes_ages <- data.frame(label_cause = rep(causes_age_specific, each = length(age_ids)), 
                   age_group_id = rep(age_ids, times=length(causes_age_specific))) 
  
  causes_ages<- data.frame(label_cause = causes_all_ages, 
                  age_group_id = rep("all ages", each=length(causes_all_ages)) )%>%
                    rbind(causes_ages)
  
  write.csv(causes_ages,causes_agesDir, row.names = FALSE)
}

state <-states[1,]

STUSPS<-state[4]
name<-state[5]

trac_rrDirX <- paste0("trac_rr_",toString(year),"_",STUSPS,".csv") %>%
                  file.path(trac_rrDir, .)

getRR <- function(exp_rr, exp){
  #exp <- pm #TODO löschen
  
  rr<-match.closest(exp, exp_rr$exposure_spline) %>%
          exp_rr[.,'mean']
  return(rr)
#TODO auch Konfidenzintervall
  #which(abs(x-your.number)==min(abs(x-your.number)))
}

if(!file.exists(trac_rrDirX)){
  tic(paste("Assigned RR to each tract for year",toString(year), "in", name))
  
  exp_trac <- paste0("exp_trac_",toString(year),"_",STUSPS,".csv") %>%
    file.path(exp_tracDir, .) %>%
    read.csv
  
  
  trac_rr<-merge(exp_trac,causes_ages)
  
  
  
  #TODO mutate
  #TODO change order of loops, so it is more efficient
  rrs<-apply(trac_rr,1,function(row){
    #row<-trac_rr[1,] #TODO löschen
    pm<-row[2]
    label_cause <- row[3]
    age_group_id <- row[4]
    
    exp_rr <- ifelse(age_group_id == "all ages",
                     paste0(label_cause,".csv"),
                     paste0(label_cause,"_",age_group_id,".csv")) %>%
                    file.path(exp_rrDir,.) %>%
                    read.csv
    #
    rr<-sapply(tmrels, function(tmrel){
      rr<-ifelse(pm<=tmrel,
                 1,
                 getRR(exp_rr,pm)/getRR(exp_rr,tmrel))
      
    }) %>% mean        #TODO mean oder median
    
    return(rr)
  })
 
  
  trac_rr <- cbind(trac_rr,rrs) #TODO pipe
  toc()
  write.csv(trac_rr,trac_rrDirX, row.names = FALSE)
}


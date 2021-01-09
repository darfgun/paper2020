#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: read total burden data 
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "testthat","tidyverse", "tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenDir <- args[12]

year <- 2000
agr_by <- "nation"

tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
pafDir <- "/Users/default/Desktop/paper2020/data/08_paf"
totalBurdenDir <- "/Users/default/Desktop/paper2020/data/09_total_burden"

#TODO
totalBurdenDir<-file.path(totalBurdenDir,agr_by)
files<-list.files(totalBurdenDir)

for(file in files){
  fileDir<-file.path(totalBurdenDir,file)
  
  #read.delim("~/Desktop/paper2020/data/09_total_burden/nation/cvd_ihd.txt")
  
  total_burden <- read.delim(fileDir) %>%
    filter(Single.Year.Ages !="Not Stated")  %>%
    mutate(
      Single.Year.Ages.Code = as.numeric(Single.Year.Ages.Code),
      YLD =sapply(Single.Year.Ages.Code, function(a) max(0,75-a))
    )
  
  cause_icd<-total_burden$Notes[grepl("UCD - ICD-10 Codes:", total_burden$Notes, fixed = TRUE)]
  if(grepl("I20-I25 (Ischaemic heart diseases)",cause_icd, fixed = TRUE)){
    cause_id = "Cvd_ihd"
  }
  total_burden$cause_id <- rep(cause_id,nrow(total_burden))
  
  total_burden$Notes<-NULL
  #TODO join with paf
}

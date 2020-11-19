#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# clear memory
rm(list=ls(all=TRUE))

# runtime configuration
if (Sys.info()["sysname"] == "Darwin") {
  j#TODO
} else {
    print(paste("no handler for", Sys.info()["sysname"], "implemented yet."))
}

#create data directory, setwd
h_root<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(h_root)

subDir <- "data"
dataDir <-file.path(h_root, subDir)
if (!file.exists(dataDir)){
  dir.create(dataDir)
} 

expDir <-file.path(dataDir, "01_exposure")
if (!file.exists(expDir)){
  dir.create(expDir)
  fileConn<-file(file.path(expDir,"readme.txt"))
  writeLines(c("This directory contains PM exposure data","downloaded from ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"), fileConn)
  close(fileConn)
} 

tmpDir <-file.path(dataDir, "tmp")
if (!file.exists(tmpDir)){
  dir.create(tmpDir)
  fileConn<-file(file.path(tmpDir,"readme.txt"))
  writeLines(c("This directory contains variables used in calculations"), fileConn)
  close(fileConn)
} 

#paths of scripts
download.script <- file.path(h_root, '01_download.R')

# load packages, install if missing

packages <- c()

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

#--------parameters of code-------------------
years <- c(2010)

for(year in years){
  #TODO
}


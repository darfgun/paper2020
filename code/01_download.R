#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download required data
#
#***************************************************************************

#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing #TODO here?

packages <- c("RCurl","magrittr")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

#download rhdf5
if("rhdf5" %in% rownames(installed.packages())==FALSE){
  if (!requireNamespace("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
  }
  BiocManager::install("rhdf5")
}
library(rhdf5)

# Pass in arguments
args <- commandArgs(trailingOnly=T)
dataDir <- args[1]
expDir <- args[2]
tmpDir <- args[3]
year <- args[4]



#----------------------download exposure data-----------------
filenameExp<-paste(toString(year),".h5", sep = "")
filepathExp <- file.path(expDir, filenameExp)

if (!file.exists(filepathExp)){
  url<-"ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"
  print(paste("Downloading PM exposure data for",year))
  download.file(paste(url, filenameExp, sep = ""), filepathExp)
  print(paste("Successfully downloaded PM exposure data for",year))
}

#save useful variable 
filenameM <-paste("m_exp_",toString(year),".RData", sep = "")
filepathM <- file.path(tmpDir, filenameM)
if (!file.exists(filepathM)){
  exp_data <- H5Fopen(filepathExp)
  
  long_vec <-  c(as.matrix(exp_data$longitude)) #TODO optimize? 
  lat_vec <- c(as.matrix(exp_data$latitude))
  
  n_long<-length(long_vec)-1
  long_dif<-long_vec[2:(1+n_long)]-long_vec[1:n_long]
  m_min_long<-min(long_dif) 
  m_max_long<-max(long_dif)
  
  n_lat<-length(lat_vec)-1
  lat_dif<-lat_vec[2:(1+n_lat)]-lat_vec[1:n_lat]
  m_min_lat<-min(lat_dif) 
  m_max_lat<-max(lat_dif)
  
  save(m_min_long, m_max_long, m_min_lat, m_max_lat, file = filepathM)
}
###--------download
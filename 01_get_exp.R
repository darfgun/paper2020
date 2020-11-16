#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download exposure data from ftp server, convenient getter functions
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# load packages, install if missing

packages <- c("RCurl")

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

#create data directory, setwd
mainDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(mainDir)

subDir <- "data"
dataDir <-file.path(mainDir, subDir)
if (!file.exists(dataDir)){
  dir.create(dataDir)
} 

expDir <-file.path(dataDir, "01_exposure")
if (!file.exists(expDir)){
  dir.create(expDir)
} 

#------------------Functions--------------------------------------------------
#download 

downloadYear <-function(year){
  if(!(year %in% 1981:2016)){
    print("The exposure data is only available for 1981-2016!")
  }
  url<-"ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"
  filename<-getFileName(year)
  filepath<-getFilePath(year)
  print(paste("Downloading PM exposure data for",year))
  download.file(paste(url, filename, sep = ""), filepath)
  print(paste("Successfully downloaded PM exposure data for",year))
}

#getter functions
getFileName<-function(year){
  return(paste(toString(year),".h5", sep = ""))
}

getFilePath <-function(year){
  filename<-getFileName(year)
  filepath <- file.path(expDir, filename)
  return(filepath)
}

getExposureH5F <- function(year){
  filepath<- getFilePath(year)
  if (!file.exists(filepath)){
    downloadYear(year)
  }
  h5f = H5Fopen(filepath)
  return(h5f)
  #TODO return
}

getExposure <- function(year, long, lat){
  #TODO
}

#------------------run tests--------------------------------------------------
year<-2016
getExposure(year)

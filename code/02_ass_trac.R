#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: assign PM exposure to each census tract
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing #TODO here?

packages <- c("magrittr", "tigris", "ggplot", "tmap")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  suppressWarnings(library(p, character.only = T))
  
}
options(tigris_use_cache = TRUE) 

#download rhdf5
if("rhdf5" %in% rownames(installed.packages())==FALSE){
  if (!requireNamespace("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
  }
  BiocManager::install("rhdf5")
}
suppressWarnings(library(rhdf5))

 
# Pass in arguments
args <- commandArgs(trailingOnly=T)
year <- args[1]
dataDir <- args[2]
tmpDir <- args[3]
expDir <- args[4]
tracDir <- args[5]
exp_tracDir <- args[6]

#test #TODO löschen
paste(year,
      dataDir,
      tmpDir,
      expDir,
      tracDir,
      exp_tracDir)

year <- 2010 
dataDir <- "C:/Users/Daniel/Desktop/paper2020/data"
tmpDir <- "C:/Users/Daniel/Desktop/paper2020/data/tmp"
expDir <- "C:/Users/Daniel/Desktop/paper2020/data/01_exposure"
tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/02_tracts"
exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"

##-----code-----
#load data
filenameExp<-paste(toString(year),".h5", sep = "")
filepathExp <- file.path(expDir, filenameExp)
exp_data <- H5Fopen(filepathExp)

filenameM <-paste("m_exp_",toString(year),".RData", sep = "")
filepathM <- file.path(tmpDir, filenameM)
load(filepathM)

filenameTr<-paste("tracts_",toString(year),".rds", sep = "")
filepathTr <- file.path(tracDir, filenameTr)
tracts<-readRDS(filepathTr)

#plot
gg <- ggplot()
gg <- gg + geom_sf(data = tracts, color="black",
                   fill="white", size=0.25)
gg

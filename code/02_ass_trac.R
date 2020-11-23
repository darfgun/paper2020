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

packages <- c("magrittr", "tigris", "sf", "tidyverse")#, "tmap"

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  suppressWarnings(library(p, character.only = T, warn.conflicts=FALSE))
  
}
options(tigris_use_cache = FALSE) 

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
long_vec <-  c(as.matrix(exp_data$longitude)) 
lat_vec <- c(as.matrix(exp_data$latitude))

filenameM <-paste("m_exp_",toString(year),".RData", sep = "")
filepathM <- file.path(tmpDir, filenameM)
load(filepathM)

filenameTr<-paste("tracts_",toString(year),".rds", sep = "")
filepathTr <- file.path(tracDir, filenameTr)
tracts<-readRDS(filepathTr)

#calc
#TODO GEO_ID vs tract vs Name

result <- data.frame()      #Doubles=double()

for(i in seq_along(tracts)) { #
  #i<-50 #TODO löschen
  row <- tracts[i,]
  
  #get enclosing box, make sure in range of exposure data
  bbox <- st_bbox(row)
  long_min <- bbox$xmin %>%
                    max(.,long_vec[1])
  lat_min <- bbox$ymin %>%
                    max(.,lat_vec[1])
  long_max <- bbox$xmax %>%
                    min(.,long_vec[length(long_vec)]) 
  lat_max <- bbox$ymax %>%
                    min(.,lat_vec[length(lat_vec)])
  
  
  long_row_min <- -1+((long_min-long_vec[1])/m_max_long) %>%
                                                            floor
  lat_row_min <- -1+((lat_min-lat_vec[1])/m_max_lat) %>%
                                                            floor
  long_row_max<-1+((long_max-long_vec[1])/m_min_long) %>%
                                                            ceiling
  lat_row_max<-1+((lat_max-lat_vec[1])/m_min_lat) %>%
                                                            ceiling
  
  long_subset<-long_vec[long_row_min:long_row_max]
  lat_subset <- lat_vec[lat_row_min:lat_row_max]
  
  #bbox #TODO delete
  #c( min(long_subset),  min(lat_subset),max(long_subset) ,max(lat_subset))
  #asdfs<-c( (bbox$xmin >=min(long_subset)),  bbox$ymin >=min(lat_subset),bbox$xmax<= max(long_subset) ,bbox$ymax<=max(lat_subset))
  #print(asdfs)
  
  m <- outer (long_subset, lat_subset,     
    FUN = function (x, y){
      coord <- c(x, y)
      #print(coord)
      #point<-st_point(x = coord)
      x+2*y
    }   
  )
  
  coord <- c(80, -120)
  point<-st_point(x = coord)
  
  #TODO was, wenn keiner drin?
  for(j in seq_along(long_subset)){
    for(s in seq_along(lat_subset)){
      coord <- c(long_subset[j], lat_subset[s])
      point<-st_point(x = coord)
      
    }
  }
  
  #tree_in_tract <- st_join(tree_sf, tract_sf, join = st_within)
  
  #add_row(result, bbox)

}




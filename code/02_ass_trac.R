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

packages <- c("magrittr", "tigris", "sf", "tidyverse", "ggplot2","plyr", "sp")#, "tmap", TODO delete plyr

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
#args <- commandArgs(trailingOnly=T)
#year <- args[1]
#dataDir <- args[2]
#tmpDir <- args[3]
#expDir <- args[4]
#tracDir <- args[5]
#exp_tracDir <- args[6]

#test #TODO l?schen

year <- 2010 #TODO delete
dataDir <- "/Users/default/Desktop/own_code2/data"
tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
expDir <- "/Users/default/Desktop/own_code2/data/01_exposure"
tracDir <- "/Users/default/Desktop/own_code2/data/02_tracts"
exp_tracDir <- "/Users/default/Desktop/own_code2/data/03_exp_tracts"

##-----code-----
#load data
filenameExp<-paste(toString(year),".h5", sep = "")
filepathExp <- file.path(expDir, filenameExp)
hdf_file <- H5Fopen(filepathExp)
exp_data <- as.matrix(hdf_file$CorrectedPM2.5)
long_vec <-  c(as.matrix(hdf_file$longitude)) 
lat_vec <- c(as.matrix(hdf_file$latitude))

filenameM <-paste("m_exp_",toString(year),".RData", sep = "")
filepathM <- file.path(tmpDir, filenameM)
load(filepathM)

filenameTr<-paste("tracts_",toString(year),".rds", sep = "")
filepathTr <- file.path(tracDir, filenameTr)
tracts<-readRDS(filepathTr)

#calc
#TODO GEO_ID vs tract vs Name

result <- data.frame()      #Doubles=double()


#TODO optimze this by doing the whole process per county and not per tracts
for(i in seq_along(tracts)) { #TODO nicht for Schleife, sondern apply/mutate?
  i<-50 #TODO l?schen #6 gutes Beispiel
  tract <- tracts[i,]
  
  #get enclosing box, make sure in range of exposure data
  bbox <- st_bbox(tract)
  long_min <- bbox$xmin %>%
                    max(.,long_vec[1])
  lat_min <- bbox$ymin %>%
                    max(.,lat_vec[1])
  long_max <- bbox$xmax %>%
                    min(.,long_vec[length(long_vec)]) 
  lat_max <- bbox$ymax %>%
                    min(.,lat_vec[length(lat_vec)])
  
  #estimate corresponding grid in pm exposure data
  long_row_min <- -1+((long_min-long_vec[1])/m_max_long) %>% #TODO optimize here, if this takes too long (smallaer box)
                                                            floor
  lat_row_min <- -1+((lat_min-lat_vec[1])/m_max_lat) %>%
                                                            floor
  long_row_max<-1+((long_max-long_vec[1])/m_min_long) %>%
                                                            ceiling
  lat_row_max<-1+((lat_max-lat_vec[1])/m_min_lat) %>%
                                                            ceiling
  
  long_subset<-long_vec[long_row_min:long_row_max]
  lat_subset <- lat_vec[lat_row_min:lat_row_max]
  pm_subset <- exp_data[long_row_min:long_row_max,lat_row_min:lat_row_max]
  
  points_subset <- data.frame(lat = rep(lat_subset, times = length(long_subset)), 
                       lng = rep(long_subset, each = length(lat_subset)), 
                       pm = as.vector(t(pm_subset))) %>%
                      st_as_sf(., coords = c("lng", "lat"), 
                        crs = 4269,  #TODO same as st_crs(tract)
                        agr = "constant")
  
  suppressWarnings(points_in_tract <- points_subset[tract, , op = st_within]) 
  #use filter/st_filter instead? https://geocompr.robinlovelace.net/spatial-operations.html
  
  
  #if there are points inside of the tract, the tract is assigned the mean of pm of those points
  # if there are none, the pm of the closest point
  pm <- ifelse(nrow(points_in_tract)>0,
               mean(points_in_tract$pm),
               st_centroid(tract) %>%
                 which.min(st_distance(points_subset, .)) %>%
                 points_subset[.,]$pm)/100
 
  #add_row(result, pm)
  
}


#TODO save to csv

gg <- ggplot()+ 
  geom_sf(data = tract, color="black",
          fill="white")+
  geom_sf(data = points_subset, size = 4, shape = 23)+
  geom_sf(data = r, size = 4, shape = 23, color="red")

gg

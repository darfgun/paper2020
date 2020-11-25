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

packages <- c("magrittr", "tigris", "sf", "tidyverse", "ggplot2","plyr")#, "tmap"

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



for(i in seq_along(tracts)) { #
  i<-50 #TODO l?schen
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
  pm_subset <- exp_data[long_row_min:long_row_max,lat_row_min:lat_row_max]
  
  subset <- expand.grid(longX = seq_along(long_subset), latX = seq_along(lat_subset), pm = NA) %>%
    mutate(pm = pm_subset[longX,latX]) #,long = long_subset[long], lat = lat_subset[lat]
  
  
  #subset['pm'] <- NA
  #https://stackoverflow.com/questions/48662248/apply-function-to-every-element-in-data-frame-and-return-data-frame/48662359
  #https://community.rstudio.com/t/apply-function-to-each-row-in-a-df-and-create-a-new-df-with-the-outputs/38946/4
  #subset[]
  subset2 <- apply(subset, 1, FUN = function(long_ind, lat_ind){
    #print(c("long",long_ind,"lat",lat_ind))
    long <- long_subset[long_ind]
    lat <- lat_subset[lat_ind]
    pm <- pm_subset[long_ind,lat_ind]
    return(c(long,lat,pm))
  })
  subset2<-as.data.frame(subset2)
  subset2
  #TODO convert to spatial data frame
  
  #bbox #TODO delete
  #c( min(long_subset),  min(lat_subset),max(long_subset) ,max(lat_subset))
  #asdfs<-c( (bbox$xmin >=min(long_subset)),  bbox$ymin >=min(lat_subset),bbox$xmax<= max(long_subset) ,bbox$ymax<=max(lat_subset))
  #print(asdfs)
  gg <- ggplot()
  gg <- gg + geom_sf(data = row, color="black",
                     fill="white", size=0.25)
  
  #https://stackoverflow.com/questions/4309217/cartesian-product-data-frame
  
  select <- matrix(ncol = 2, byrow = TRUE) 
  
  for(k in seq_along(long_subset)){
    for(j in seq_along(lat_subset)){
      coord <- c(long_subset[k], lat_subset[j])
      point<-st_point(x = coord)
      (sites <- data.frame(longitude = long_subset[k], latitude = lat_subset[j]))
      (sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                         crs = 4326, agr = "constant"))
      
       gg <- gg + geom_sf(data = sites, size = 4, shape = 23, fill = "darkred")
      
      #if(st_within(point, row, sparse=FALSE)){ #TODO richtige Reihenfolge?, 
      #  c <- c(k,j)
      #  rbind(select,c) #TODO optimize?
      #}
    }
  }
  gg
  
  if(nrow(select)>0){
    pm <- exp_data[long_row_min:long_row_max,lat_row_min:lat_row_max][select] 
    #%>% mean
  }else{
    #TODO find closest, take that
  }
  #add_row(result, pm)
  #TODO pm = pm/100
}




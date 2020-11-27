#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: assign PM exposure to each census tract
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing 

packages <- c("plyr","magrittr", "tigris", "sf", "tidyverse", "ggplot2", "sp","tmap","tictoc")

options(tidyverse.quiet = TRUE)
options(tigris.quiet = TRUE)
options(tigris_use_cache = FALSE) 
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts=FALSE))
  
}

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

filenameExpTrac<-paste("exp_trac",toString(year),".csv", sep = "")
filepathExpTrac <- file.path(exp_tracDir, filenameExpTrac)

#quit execution, if already calculated
if (file.exists(filepathExpTrac)) quit() 


##---------------load data---------------

#load exposure data
filenameExp<-paste(toString(year),".h5", sep = "")
filepathExp <- file.path(expDir, filenameExp)
hdf_file <- H5Fopen(filepathExp)
exp_data <- as.matrix(hdf_file$CorrectedPM2.5)
long_vec <-  c(as.matrix(hdf_file$longitude)) 
lat_vec <- c(as.matrix(hdf_file$latitude))

#load some useful estimates to optimize code
filenameM <-paste("m_exp_",toString(year),".RData", sep = "")
filepathM <- file.path(tmpDir, filenameM)
load(filepathM)

#load shape files of tracts
filenameTr<-paste("tracts_",toString(year),".rds", sep = "")
filepathTr <- file.path(tracDir, filenameTr)
tracts<-readRDS(filepathTr)

##-----------------calculation---------------

#TODO optimze this by doing the whole process per county and not per tracts, group tibble

tic(paste("Assigned pm exposure to each tract for year",toString(year)))
#estimate pm exposure for each tract
tracts <-tracts$geometry %>% sapply(., function(tract){
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
  #TODO optimize here, if this takes too long (smaller box)
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
  
  #estimates, where to look for the pm data for this particular tract to improve run time
  points_subset <- data.frame(lat = rep(lat_subset, times = length(long_subset)), 
                       lng = rep(long_subset, each = length(lat_subset)), 
                       pm = as.vector(t(pm_subset))) %>%
                      st_as_sf(., coords = c("lng", "lat"), 
                        crs = st_crs(tract),
                        agr = "constant")
  
  #subset points, which are inside of the tract
  suppressMessages(points_in_tract <- points_subset[tract, , op = st_within]) 
  
  #if there are points inside of the tract, the tract is assigned the mean of pm of those points
  # if there are none, the pm of the closest point
   pm <- 0.01*ifelse(nrow(points_in_tract)>0,
               points_in_tract$pm %>%   
                  mean(., na.rm = TRUE),
               tract %>%
                 suppressWarnings(st_centroid) %>% 
                 st_distance(x=points_subset, y=.) %>% 
                 which.min %>%
                 points_subset[.,] %>%
                 pull(pm))
  }) %>% 
    cbind(tracts,pm= .)
toc()

##--------------plot-----------
#save everything as interactive map via tmap
if(TRUE){
  #tmap_mode("view")
    
  tm<-tm_shape(tracts) +
       tm_polygons("pm", alpha = 0.6) 
      
  
  filenameExpTrac_plot<-paste("exp_trac",toString(year),".html", sep = "") #png/html möglich
  filepathExpTrac_plot <- file.path(exp_tracDir, filenameExpTrac_plot)
  tmap_save(tm, filename = filepathExpTrac_plot) 
}

##-----save as csv--------
tracts <-tracts %>% 
          as.data.frame %>%
          suppressWarnings(within(., rm('geometry', 'LSAD', 'CENSUSAREA', 'AREA')))

write.csv(tracts,filepathExpTrac, row.names = FALSE)

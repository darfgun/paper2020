#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census tract, assign exposure
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# clear memory
rm(list=ls())

# load packages, install if missing
packages <- c("tidyverse", "sf", "rgdal", "tmap","tigris","censusapi","ggplot2")
options("rgdal_show_exportToProj4_warnings"="none")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

#create data directory, setwd
mainDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(mainDir)

subDir <- "data"
dataDir <-file.path(mainDir, subDir)
if (!file.exists(dataDir)){
  dir.create(dataDir)
} 

tracDir <-file.path(dataDir, "02_tracts")
if (!file.exists(tracDir)){
  dir.create(tracDir)
  fileConn<-file(file.path(expDir,"readme.txt"))
  writeLines(c("This directory contains census shape files","downloaded via the R package tigris"), fileConn)
  close(fileConn)
} 

#------------------Functions--------------------------------------------------
#download tracts 

downloadYear_tr <-function(year){
  #only for for Chicago for test purposes
  chi_counties <- c("Cook", "DeKalb", "DuPage", "Grundy", "Lake", 
                    "Kane", "Kendall", "McHenry", "Will County")
  
  print(paste("Downloading tracts for",year))
  tracts <- tracts(state = "IL", county = chi_counties, cb = TRUE, year=year)
  print(paste("Successfully downloaded Ptracts for",year))
  
  filepath<-getFilePath_tr(year)
  saveRDS(tracts, filepath)
}


#get file path for tract file in year
getFilePath_tr <-function(year){
  filename<-paste("tracts_",toString(year),".rds", sep = "")
  filepath <- file.path(tracDir, filename)
  return(filepath)
}

#plot
#https://community.rstudio.com/t/best-packages-for-making-map-leaflet-vs-ggmap-vs-sf-vs/38403/2
#https://geocompr.robinlovelace.net/adv-map.html
#https://bookdown.org/lexcomber/brunsdoncomber2e/Ch7.html#the-pennsylvania-ling-cancer-data
plotTracts <-function(tracts){
  #TODO tmap
  leaflet(tracts) %>%
    addTiles() %>% #addProviderTiles('Esri.WorldShadedRelief')
    addPolygons(popup = ~NAME)
  
  #plot(chi_tracts)
}

getTracts <-function(year){
  filepath<- getFilePath_tr(year)
  if (!file.exists(filepath)){
    tracts <-downloadYear_tr(year)
  }else{
    tracts <- readRDS(filepath)
  }
  
  return(tracts)
}


  


#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download exposure data from ftp server, convenient getter functions
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# load packages, install if missing

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
  
  #save useful variable #TODO
  exp_data <- H5Fopen(filepath)
  
  long_vec <-  c(as.matrix(exp_data$longitude)) 
  lat_vec <- c(as.matrix(exp_data$latitude))
  
  n_long<-length(long_vec)-1
  long_dif<-long_vec[2:(1+n_long)]-long_vec[1:n_long]
  m_min_long<-min(long_dif) 
  m_max_long<-max(long_dif)
  
  n_lat<-length(lat_vec)-1
  lat_dif<-lat_vec[2:(1+n_lat)]-lat_vec[1:n_lat]
  m_min_lat<-min(lat_dif) 
  m_max_lat<-max(lat_dif)
  
  
  filename <-paste("m_exp_",toString(year),".RData", sep = "")
  filepath <- file.path(tmpDir, filename)
  save(m_min_long, m_max_long, m_min_lat, m_max_lat, file = filepath)
  #TODO check
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
  exp_data <- H5Fopen(filepath)
  return(exp_data)
  #TODO return
}


getExposure <- function(year, long, lat){
  
  exp_data <- getExposureH5F(year)
  
  long_vec <-  c(as.matrix(exp_data$longitude)) #TODO more efficient
  lat_vec <- c(as.matrix(exp_data$latitude))
  
  #make sure in grid
  n<-length(long_vec)-1
  a<-long_vec[2:(1+n)]-long_vec[1:n]
  m_min_long<-min(a) #TODO save
  m_max_long<-max(a)
  
  #test todo delete
  year<-2016
  #ru <-runif(1, min=-140,max=-120)
  #for(long in ru){
  long<-120.5056
  long_clos <- sum(long,0.005)%>%
                  round(.,digit=2) %>%
                  sum(-0.005) %>%
                  min(.,long_vec[length(long_vec)]) %>%
                  max(.,long_vec[1]) 
  r1<-floor(1+(long_clos-long_vec[1])/m_max_long)
  r2<-ceiling(1+(long_clos-long_vec[1])/m_min_long)
  subset<-long_vec[r1:r2]
  r_final <- r1+which.min(abs(long_vec[r1:r2]-long_clos))-1 #TODO pipe
  r_final2 <- long_vec[r1:r2]-long_clos #%>%
                  #abs %>%
                  #which.min %>%
                  #sum(r1-1)

  #}
  
  lat_clos <- min(lat,lat_vec[length(lat_vec)]) %>%
                  max(lat_vec[1]) %>%
                  round(digit=2) %>%
                  sum(-0.005)
  

  #TODO divide by 100
}

#------------------run tests--------------------------------------------------

getExposureH5F(2016)



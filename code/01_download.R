#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download required data
#
#***************************************************************************

#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing #TODO here?
packages <- c("RCurl","magrittr", "tigris", "censusapi")

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}
options(tigris_use_cache = TRUE) 

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
year <- args[1]
dataDir <- args[2]
tmpDir <- args[3]
expDir <- args[4]
tracDir <- args[5]
censDir <- args[6]



#----------------------download exposure data-----------------
filenameExp<-paste(toString(year),".h5", sep = "")
filepathExp <- file.path(expDir, filenameExp)

if (!file.exists(filepathExp)){
  url<-"ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"
  print(paste("Downloading PM exposure data for",year))
  download.file(paste(url, filenameExp, sep = ""), filepathExp)
  print(paste("Successfully downloaded PM exposure data for",year))
}

#save useful variable for estimations later on
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

rm(filenameExp, filepathExp) #TODO
rm(filenameM, filepathM)

###------------------download tract shape files--------------------
#only for for Chicago for test purposes
filenameTr<-paste("tracts_",toString(year),".rds", sep = "")
filepathTr <- file.path(tracDir, filenameTr)

if (!file.exists(filepathTr)){
  
  chi_counties <- c("Cook", "DeKalb", "DuPage", "Grundy", "Lake", 
                    "Kane", "Kendall", "McHenry", "Will County")
  
  print(paste("Downloading tracts for",year))
  tracts <- tracts(state = "IL", county = chi_counties, cb = TRUE, year=year)
  print(paste("Successfully downloaded tracts for",year))
  
  saveRDS(tracts, filepathTr) #TODO other format?
}

rm(filenameTr, filepathTr)

###------------------download census data files--------------------

filenameCens<-paste("census_",toString(year),".csv", sep = "") #TODO data frame as csv
filepathCens <- file.path(censDir, filenameCens) 

# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef" #TODO
Sys.setenv(CENSUS_KEY=key)

#https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
#https://www.census.gov/data/developers/data-sets.html
#https://www.census.gov/data/developers/data-sets/decennial-census.html
#https://hrecht.github.io/censusapi/articles/example-masterlist.html


if (!file.exists(filepathCens)){
  #readme path
  filepathReadMe <- file.path(censDir,"readme.txt")
  data_from_api <- data.frame()
  
  if(year == 2000){
    #dec/sf1
    #data_from_api <- getCensus(name = "dec/sf1", vintage = year,
     #                          group = "P5", #TODO vars = "group(P2)",
      #                         region = "tract:*", 
    #regionin = "state:17")
  }else if(year %in% 2001:2009){
    #TODO 
  }else if(year == 2010){
    #https://api.census.gov/data/2010/dec/sf1/variables.html
    
    census_vars <- listCensusMetadata(
      name = "dec/sf1", 
      vintage = 2010,
      type = "variables",
      group = "P5") %>%
      head
    
    write.table(census_vars, file = filepathReadMe, sep = "\t",
                row.names = TRUE, col.names = NA)#TODO append=true, formatting
    
    data_from_api <- getCensus(name = "dec/sf1", vintage = year,
                              vars = "group(P2)",
                             region = "tract:*", 
    regionin = "state:17") #TODO testzwecke
  }else if(year %in% 2011:2013){
    #TODO
  }else if(year %in% 2014:2016){
    #name = acs/acsse
  }else{
    print(paste("No census data for",year,"available"))
  }
  #values <- data_from_api %>%
  # transmute(GEOID = paste0(state, county, tract), 
  #          value = B25077_001E)
  #TODO harmonize, same col names
  
  write.csv(data_from_api,filepathCens, row.names = FALSE) 
}


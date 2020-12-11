#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download required data
#
#***************************************************************************

#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing 
packages <- c("dplyr", "RCurl","magrittr", "tigris", "censusapi","stringr","data.table","tidyverse")#,"tictoc"

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
tmpDir <- args[3]
expDir <- args[4]
tracDir <- args[5]
censDir <- args[9]


#----------------------download exposure data-----------------
filenameExp<-paste0(toString(year),".h5")
filepathExp <- file.path(expDir, filenameExp)

if (!file.exists(filepathExp)){
  url<-"ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"
  print(paste("Downloading PM exposure data for",year))
  download.file(paste0(url, filenameExp), filepathExp)
  print(paste("Successfully downloaded PM exposure data for",year))
}

#save useful variable for estimations later on
filepathM <- paste0("m_exp_",toString(year),".RData") %>%
                    file.path(tmpDir, .)

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

rm(filenameExp, filepathExp,filepathM) 

##------download useful data to tmp-----
filepathStates <- file.path(tmpDir, "states.csv")
if (!file.exists(filepathStates)){
  #excluding Alaska, Hawaii, American Samoa, Guam, Commonwealth of the Northern Mariana Islands, Puerto Rico, United States Virgin Islands
  states<-states() %>%
    as.data.frame %>%
    select(c(1:3,6,7)) %>%
    filter(!(STUSPS %in% c("AK",'HI','AS','GU','MP','PR','VI'))) %>%
    arrange(STATEFP) 
  states
  
  write.csv(states,filepathStates, row.names = FALSE)
}else{
  states <- read.csv(filepathStates)
}
rm(filepathStates)

###------------------download tract shape files--------------------
filepathTr <- file.path(tracDir, toString(year))
dir.create(filepathTr, recursive = T, showWarnings = F)
  
apply(states, 1, function(x){
  STUSPS<-x[4]
  name<-x[5]
  
  filepathTrX <- paste0("tracts_",toString(year),"_",STUSPS,".rds") %>%
                      file.path(filepathTr, .)
  
  if (!file.exists(filepathTrX)){
    print(paste("Downloading census tracts for",year,name))
    #TODO fallunterscheidung
    
    tracts <- tracts(state = STUSPS, cb = TRUE, year=year)
    saveRDS(tracts, filepathTrX)
  }
})

rm(filepathTr)

###------------------download census data files--------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef" #TODO
Sys.setenv(CENSUS_KEY=key)


##----- download metadata------
filepathCensMeta <- file.path(censDir, "dec_cens_meta.csv")

if ( !file.exists(filepathCensMeta)){ 
        groups <- c("PCT12A","PCT12B","PCT12C","PCT12D","PCT12D","PCT12E","PCT12I","PCT12J","PCT12K","PCT12L","PCT12M")
        census_meta<-lapply(groups, function(group){
            listCensusMetadata(
              name = "dec/sf1", 
              vintage = 2010,
              type = "variables",
              group = group #TODO more
            ) %>% 
              select('name','label','concept') %>%
              mutate(
                label = strsplit(label, "!!"),
                label_len = sapply(label, length),
                gender = label %>% sapply(function(l) l[2]),
                gender_label = gender %>% sapply(function(g) ifelse(g == "Female", 'F', 'M')),
                age = label %>% sapply(function(l) l[3]),
                min_age = age %>% sapply(function(a){ 
                  if(grepl("Under 1 year", a))
                    return(0)                 
                  str_extract(a, "[:digit:]+") %>% 
                    as.numeric %>% #TODO
                    return(.)
                  }),
                max_age = age %>% sapply(function(a){ #TODO
                  if(grepl("and over", a))
                    return(150)
                  if(grepl("Under 1 year", a))
                    return(0)
                  str_extract_all(a, "[:digit:]+") %>% 
                    unlist %>% 
                    tail(1) %>% 
                    as.numeric %>% #TODO
                    return(.)
                }),
                #age = NULL, #TODO
                label = NULL,
                race_his = concept %>% sapply(function(conc)
                  regmatches(conc, gregexpr("(?<=\\().*?(?=\\))", conc, perl=T))[[1]]
                ),
                concept = NULL,
                race =race_his %>% sapply(function(race_his){
                  race_his %>% 
                    strsplit(.,",") %>% 
                    unlist %>% 
                    extract2(1)%>% 
                    substr(.,1,nchar(.)-6)
                }),
                hispanic_origin =race_his %>% sapply(function(race_his){
                  a<- race_his %>% 
                        strsplit(.,",") %>% 
                        unlist
                  ifelse(length(a) == 1, 
                           "all", 
                           a[2])
                }),
                race_his = NULL
                ) 
      })  %>%
          do.call(rbind,.) %>% 
          as.data.frame%>%
          filter(label_len==3) %>%
          mutate(label_len = NULL)
  
      setnames(census_meta, "name", "variable")
      
      write.csv(census_meta,filepathCensMeta, row.names = FALSE) 
}else{
  census_meta <- read.csv(filepathCensMeta)
}
##--- download sex by age for each race---
filepathCens<- paste0("census_",toString(year),".csv") %>% #TODO data frame as csv
                        file.path(censDir, .) 



#https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
#https://www.census.gov/data/developers/data-sets.html
#https://www.census.gov/data/developers/data-sets/decennial-census.html
#https://hrecht.github.io/censusapi/articles/example-masterlist.html


if (!file.exists(filepathCens)){
  #readme path
  filepathReadMe <- file.path(censDir,"readme.txt")
  data_from_api <- data.frame()
  
  #https://stackoverflow.com/questions/12379128/r-switch-statement-on-comparisons
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
    #https://www.census.gov/data/tables/2010/demo/age-and-sex/2010-age-sex-composition.html
    year <-2010 #TODO delete
    
    census_vars <- listCensusMetadata(
      name = "dec/sf1", 
      vintage = 2010,
      type = "variables" ,
      #variable = "PCT012A001"
      group = "PCT12A"
      ) %>%
      head
    #P5:P9, P12 SEX BY AGE

    
    #write.table(census_vars, file = filepathReadMe, sep = "\t",row.names = TRUE, col.names = NA)#TODO append=true, formatting
    
    data_from_api <- getCensus(name = "dec/sf1", 
                               vintage = year,
                              vars  = "PCT012A209",
                             region = "tract:*", 
    regionin = "state:17") #TODO testzwecke
    
  }else if(year %in% 2011:2013){
    #TODO
  }else if(year %in% 2014:2016){
    #https://www.census.gov/data/developers/data-sets/ACS-supplemental-data.html
    #https://data.census.gov/cedsci/table?g=0400000US01_1600000US0100820&d=ACS%20Supplemental%20Estimates%20Detailed%20Tables&tid=ACSSE2019.K200104&hidePreview=true
    #https://data.census.gov/cedsci/table?g=0400000US01_1600000US0100820&d=ACS%20Supplemental%20Estimates%20Detailed%20Tables&tid=ACSSE2019.K200101&hidePreview=true
    #https://data.census.gov/cedsci/table?g=0400000US01_1600000US0100820&d=ACS%20Supplemental%20Estimates%20Detailed%20Tables&tid=ACSSE2019.K200201&hidePreview=true
    
    data_from_api <- getCensus(name = "acs/acsse", 
                               vintage = year, #TODO year
                               vars = "K200104",
                               region = "county subdivision", #TODO https://api.census.gov/data/2019/acs/acsse/examples.html
                               regionin = "state:17")
    
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


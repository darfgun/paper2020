#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing 
packages <- c("dplyr", "magrittr",  "censusapi","stringr","data.table","tidyverse")#,"tictoc"

options(tigris_use_cache = FALSE) 
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  
  suppressMessages(library(p, character.only = T, warn.conflicts=FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly=T)
censDir <- args[1]
tmpDir <- args[2]
year <- args[3]

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
      group = group 
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
            as.numeric
        }),
        max_age = age %>% sapply(function(a){ 
          if(grepl("and over", a))
            return(150)
          if(grepl("Under 1 year", a))
            return(0)
          str_extract_all(a, "[:digit:]+") %>% 
            unlist %>% 
            tail(1) %>% 
            as.numeric 
        }),
        age = NULL,
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


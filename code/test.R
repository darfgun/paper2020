rm(list=ls(all=TRUE))

# load packages, install if missing 
packages <- c("dplyr","magrittr", "censusapi","tidyr","tidyverse","stringr","data.table","tictoc")

options(tigris_use_cache = FALSE) 
for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  
  suppressMessages(library(p, character.only = T, warn.conflicts=FALSE))
}

# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef" 
Sys.setenv(CENSUS_KEY=key)

census_meta<-"/Users/default/Desktop/own_code2/data/06_census/meta/cens_meta_2010.csv" %>% read.csv
data_from_api<-"/Users/default/Desktop/own_code2/data/06_census/2010/census_2010_DC.csv" %>% read.csv

data_from_api<-data_from_api %>%
  pivot_wider(names_from = variable,
              values_from = value)

data_from_api2 <-data_from_api

census_meta_sub <- census_meta %>% filter(downloaded ==FALSE) 

for(i in 1:nrow(census_meta_sub)){
  #i<-1
  var <- census_meta_sub[i,"variable"]
  tot_var <- census_meta_sub[i,"tot_var"]
  ntot_var <- census_meta_sub[i,"ntot_var"] %>% strsplit(.,"|",fixed=TRUE) %>% unlist

  data_ntot<-data_from_api[,ntot_var]%>% 
    apply(., 2, as.numeric)%>% 
    rowSums %>% 
    unlist
  

  if(length(data_from_api[,tot_var]%>% unlist) == 358){
    print(data_from_api[,tot_var])
    data_from_api[1,tot_var]
  }

  data_from_api[,var] <- (data_from_api[,tot_var]%>% unlist) - data_ntot 
}


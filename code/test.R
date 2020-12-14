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

filepathCensMeta<-"/Users/default/Desktop/own_code2/data/06_census/meta/cens_meta_2011.csv"
census_meta <- read.csv(filepathCensMeta)
dataDir <-"/Users/default/Desktop/own_code2/data/06_census/2011/census_2011_AL.csv"
data_from_api  <- read.csv(dataDir)

data_from_api<-data_from_api %>%
                  pivot_wider(names_from = variable,
                              values_from = value)

census_meta_sub <- census_meta %>% filter(downloaded ==FALSE) 

for(i in 1:nrow(census_meta_sub)){
  var <- census_meta_sub[i,"variable"]
  tot_var <- census_meta_sub[i,"tot_var"]
  ntot_var <- census_meta_sub[i,"ntot_var"]
  data_from_api[,var] <- data_from_api[,tot_var]- data_from_api[,ntot_var]
}

data_from_api<-data_from_api %>%
                  pivot_longer(
                    cols=!c('state','county','tract','GEO_ID'), 
                    names_to = "variable", 
                    values_to = "value")



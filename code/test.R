rm(list=ls(all=TRUE))

# load packages, install if missing 
packages <- c("dplyr","magrittr", "censusapi","tidyr","tidyverse","stringr","data.table")

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



#census api 
year <-2010 #TODO delete

#TODO age, gender passt nicht
census_vars <- listCensusMetadata(
                    name = "dec/sf1", 
                    vintage = 2010,
                    type = "variables",
                    group = "PCT12A"
                    ) %>% 
                select('name','label','concept') %>%
                mutate(
                    gender = strsplit(label, "!!")[[1]][2],
                    gender_label = ifelse(gender == "Female", 'F', 'M'),
                    min_age = strsplit(label, "!!")[[1]][3] %>%
                        str_extract(., "[:digit:]+")  %>% #Under 1 year, 100 to 104 years 
                        as.numeric,
                    max_age = strsplit(label, "!!")[[1]][3] %>%
                        str_extract(., "[:digit:]+")  
                        %>% as.numeric,
                    #label = NULL,
                    race_his = regmatches(concept, gregexpr("(?<=\\().*?(?=\\))", concept, perl=T))[[1]] %>%
                         strsplit(.,","),
                    concept = NULL,
                    race = race_his[[1]][1] %>%substr(.,1,nchar(.)-6),
                    hispanic_origin = ifelse(length(race_his[[1]]) == 1, "all", race_his[[1]][2]),
                    race_his = NULL) 

setnames(census_vars, "name", "variable")


data_from_api <- getCensus(name = "dec/sf1", 
                           vintage = 2010,
                           vars  = "group(PCT12A)",
                           #vars = c("PCT012A139","PCT012A134"),
                           region = "tract:*", 
                           regionin = "state:17") %>% #TODO testzwecke
                      select(!NAME) %>% 
                      pivot_longer(
                        cols=!c('state','county','tract','GEO_ID'), 
                        names_to = "variable", 
                        values_to = "value") %>%
                      merge(., census_vars, by ="variable")




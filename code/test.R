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
relevant_variables <- census_meta$variable %>% unique

year <-2011
groups <- c("B01001A","B01001B","B01001C")
tablename <- "acs/acs5"
##--- download sex by age for each race---
#censDir <- file.path(censDir, year)
#dir.create(censDir, recursive = T, showWarnings = F)

tic(paste("Downloaded census data in year",toString(year)))

    data_from_api<-lapply(groups, function(group){
      tic(paste("Downloaded census data in year",toString(year), "for group", group))
      data<-getCensus(name = tablename, 
                      vintage = year,
                      vars = paste0("group(",group,")"),
                      region = "tract:*", 
                      regionin = "state:01") %>% 
        select(any_of(c(relevant_variables, "state", "county", "tract", "GEO_ID"))) %>%  
        pivot_longer(
          cols=!c('state','county','tract','GEO_ID'), 
          names_to = "variable", 
          values_to = "value")
      toc()
      return(data)
    }) %>%
      do.call(rbind,.) %>% 
      as.data.frame #%>%
toc()



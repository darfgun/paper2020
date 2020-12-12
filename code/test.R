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


groups <- c("PCT12A","PCT12B","PCT12C","PCT12D","PCT12D","PCT12E","PCT12I","PCT12J","PCT12K","PCT12L","PCT12M")

tic(paste("Downloaded census data"))
census_meta<-lapply(groups, function(group){
  getCensus(name = "dec/sf1", 
            vintage = 2010,
            #vars  = "group(PCT12A)",
            vars = paste0("group(",group,")"),
            region = "tract:*", 
            regionin = "state:17") %>% #TODO testzwecke
    select(!NAME) %>%  #TODO
    pivot_longer(
      cols=!c('state','county','tract'
              ,'GEO_ID' #TODO
      ), 
      names_to = "variable", 
      values_to = "value")
}) %>%
  do.call(rbind,.) %>% 
  as.data.frame 
toc()




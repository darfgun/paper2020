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

filepathCensMeta<-"/Users/default/Desktop/own_code2/data/06_census/meta/cens_meta_2000.csv"
census_meta <- read.csv(filepathCensMeta)
census_meta2 <- census_meta



census_meta_all <- census_meta %>% filter(hispanic_origin == "all")
census_meta_nhis <- census_meta %>% filter(hispanic_origin == "NOT HISPANIC OR LATINO")

for(i in 1:nrow(census_meta_all)){
  row <- census_meta_all[i,]
  census_meta_nhis_sub <- census_meta_nhis %>%
                                filter(
                                  year == row[["year"]],
                                  gender_label == row[["gender_label"]],
                                  race == row[["race"]],
                                  min_age >= row[["min_age"]],
                                  max_age <= row[["max_age"]]
                                )

  if(nrow(census_meta_nhis_sub) >0 &&
    row[["min_age"]] == (census_meta_nhis_sub$min_age %>% min) &&
     row[["max_age"]] == (census_meta_nhis_sub$max_age %>% max)){
    
    row_copy <- row %>% mutate(
     tot_var = variable,
      downloaded = FALSE,
      variable = paste0(variable,"C"),
     hispanic_origin = "HISPANIC OR LATINO" #TODO was noch?
    )
    
    row_copy$ntot_var[1] <- census_meta_nhis_sub$variable %>% list
    
    print(row_copy)
    census_meta<-rbind(census_meta,row_copy)
  }
}

#fwrite(census_meta, "/Users/default/Desktop/cens_meta_2000_test.csv")


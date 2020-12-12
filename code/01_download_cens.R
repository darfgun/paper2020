#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: download census data
#
#***************************************************************************

#clear memory
rm(list=ls(all=TRUE))

# load packages, install if missing 
packages <- c("dplyr", "magrittr",  "censusapi","stringr","data.table","tidyverse", "tigris","tictoc")

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

##------download useful data to tmp-----
filepathStates <- file.path(tmpDir, "states.csv")
if (!file.exists(filepathStates)){
  #excluding Alaska, Hawaii, American Samoa, Guam, Commonwealth of the Northern Mariana Islands, Puerto Rico, United States Virgin Islands
  states<-states() %>%
    as.data.frame %>%
    select(c(1:3,6,7)) %>%
    filter(!(STUSPS %in% c("AK",'HI','AS','GU','MP','PR','VI'))) %>%
    arrange(STATEFP) 
  
  
  write.csv(states,filepathStates, row.names = FALSE)
}else{
  states <- read.csv(filepathStates)
}
rm(filepathStates)

###------------------download census data files--------------------
# Add key to .Renviron
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef" #TODO
Sys.setenv(CENSUS_KEY=key)


##----- download metadata------
censMetaDir <- file.path(censDir,"meta" )
dir.create(censMetaDir, recursive = T, showWarnings = F)

filepathCensMeta <- paste0("cens_meta_",toString(year),".csv") %>%
                        file.path(censMetaDir, .)
if(year == 2010){
  groups <- c("PCT12A","PCT12B","PCT12C","PCT12D","PCT12D","PCT12E","PCT12I","PCT12J","PCT12K","PCT12L","PCT12M")
  tablename <- "dec/sf1"
}else if(year == 2000){
  groups <- c("P012A","P012B","P012C","P012D","PCT012J","PCT012K","PCT012L","PCT012M") 
  tablename <- "dec/sf1"
}else if(year %in% 2011:2016){
  groups <- c("B01001A","B01001B","B01001C","B01001D","B01001E","B01001H" )
  tablename <- "acs/acs5"
}

if ( !file.exists(filepathCensMeta)){ 
  tic(paste("Downloaded census meta data for year", toString(year)))
  census_meta<-lapply(groups, function(group){
    listCensusMetadata(
      name = tablename,  
      vintage = year,
      type = "variables",
      group = group 
    ) %>% 
      select('name','label','concept') %>%
      mutate(
        year = year,
        group = group,
        label = strsplit(label, "!!"),
        
        #datatype = sapply(label, function(l){
        #  if(l[[1]] %in% c("Estimate","Annotation of Estimate","Margin of Error","Annotation of Margin of Error")){
        #    return(l[[1]])
        #  }else{
        #    return("Estimate")
        #  }
        #}),
        
        #label = lapply(label, function(l){
        #  if(l[[1]] %in% c("Estimate","	Annotation of Estimate","Margin of Error","Annotation of Margin of Error")){
        #    return(l[-1])
        #  }else{
        #    return(l)
        #  }
        #}),

        label_len = sapply(label, length),
        gender = label %>% sapply(function(l) l[2]),
        gender_label = gender %>% sapply(function(g) ifelse(g == "Female", 'F', 'M')),
        age = label %>% sapply(function(l) l[3]),
        min_age = age %>% sapply(function(a){ 
          if(grepl("Under", a))
            return(0)                 
          str_extract(a, "[:digit:]+") %>% 
            as.numeric
        }),
        max_age = age %>% sapply(function(a){ 
          if(grepl("and over", a))
            return(150)
          res<-str_extract_all(a, "[:digit:]+") %>% 
                  unlist %>% 
                  tail(1) %>% 
                  as.numeric 
          if(grepl("Under", a)){
            return(res-1)
          }else{
            return(res)
          } 
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
          ifelse(length(a) <= 1, #TODO
                 "all", 
                 a[2])
        }),
        race_his = NULL
      ) 
  })  %>%
    do.call(rbind,.) %>% 
    as.data.frame%>%
    filter(label_len==3) %>% #TODO datatype == "Estimate"
    mutate(label_len = NULL)
  
  setnames(census_meta, "name", "variable")
  
  census_meta <- apply(census_meta,2,as.character)
  
  write.csv(census_meta,filepathCensMeta, row.names = FALSE) 
  toc()
}else{
  census_meta <- read.csv(filepathCensMeta)
}

census_meta <- census_meta %>% as.data.frame
relevant_variables <- census_meta$variable %>% unique

##--- download sex by age for each race---
censDir <- file.path(censDir, year)
dir.create(censDir, recursive = T, showWarnings = F)

tic(paste("Downloaded census data in year",toString(year)))
apply(states, 1, function(state){
  STATEFP <- state[3] %>% as.numeric
  STUSPS<-state[4]
  name<-state[5]
  
  filepathCens<- paste0("census_",toString(year),"_",STUSPS,".csv") %>% 
                    file.path(censDir, .) 
  
  if (!file.exists(filepathCens)){
    tic(paste("Downloaded census data in year",toString(year), "in", name))
    if(year %in% c(2000,2010)){
      data_from_api<-lapply(groups, function(group){
        tic(paste("Downloaded census data in year",toString(year), "in", name, "for group", group))
        data<-getCensus(name = tablename, 
                  vintage = year,
                  vars = paste0("group(",group,")"),
                  region = "tract:*", 
                  regionin = sprintf("state:%02d", STATEFP)) %>% 
          select(!NAME) %>%  #TODO
          pivot_longer(
            cols=!c('state','county','tract','GEO_ID'), 
            names_to = "variable", 
            values_to = "value")
        toc()
        return(data)
            }) %>%
          do.call(rbind,.) %>% 
          as.data.frame %>%
          filter(variable %in% relevant_variables) #TODO filter,relevant_variables
      
      #%>% merge(., census_vars, by ="variable")
      #https://dplyr.tidyverse.org/reference/join.html TODO join, merge oder was?
      
    }else if(year %in% 2011:2016){
      
    }
    
    write.csv(data_from_api,filepathCens, row.names = FALSE) 
    toc()
  }
})
toc()






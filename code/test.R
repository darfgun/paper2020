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

year <-2010

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
        #  ifelse(l[1] %in% c("Estimate","	Annotation of Estimate","Margin of Error"),
        #         l[1],
        #         "Estimate")
        #}),
        
        #label = lapply(label, function(l){
          
        #  ifelse(l[1] %in% c("Estimate","	Annotation of Estimate","Margin of Error"),
       #          l, #l[-1]
      #           l) %>% return()
       # }),
        
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
        #label = NULL, TODO
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
    #filter(label_len==3) %>% #TODO datatype == "Estimate"
    mutate(label_len = NULL)
  
  setnames(census_meta, "name", "variable")
  
  #census_meta <- apply(census_meta,2,as.character)

  toc()

  head(census_meta)
  class(census_meta)
  label<-census_meta$label
  
  label<-label[1:5]
  
  label2<-lapply(label, function(l){
    if(l[[1]] %in% c("Estimate","Annotation of Estimate","Margin of Error")){
      return(l[-1])
    }else{
      return(l)
    }
  })

  label ="Annotation of Estimate!!Total!!Male!!55 to 64 years" %>% 
              strsplit(., "!!") 
  label2 = label[[1]][1]
  label2 %in% c("Annotation of Estimate")  
  
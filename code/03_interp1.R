#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/14/2020
# Purpose: interpolate decennical census for 2001 -2009
#
#***************************************************************************

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr","tidyr", "magrittr","stringr","data.table","tictoc")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}


# Pass in arguments
args <- commandArgs(trailingOnly = T)
censDir <- args[1]
tmpDir <- args[2]
year <- args[3]

year<-2001
tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
censDir <- "/Users/default/Desktop/paper2020/data/06_demog"

if (!year %in% 2001:2009) {
  print(paste("can not interpolate census data for", year))
  quit()
}

states <- file.path(tmpDir, "states.csv") %>% read.csv

crosswalk <- file.path(tmpDir,"crosswalk_2000_2010.csv") %>% 
  read.table(., header=TRUE) %>%
  setnames("trtid00.","trtid00") %>%
  mutate(trtid00= str_sub(trtid00,1,11))

crosswalk<-crosswalk[,c("trtid00","trtid10","weight")]

censDir00 <- file.path(censDir, "2000")
censDir10 <- file.path(censDir, "2010")

censDir00_in10<-file.path(censDir, "2000_in_2010")
dir.create(censDir00_in10, recursive = T, showWarnings = F)

##-----calculate-----

apply(states, 1, function(state) {
  STUSPS <- state["STUSPS"]
  name <- state["NAME"]
  
  censDir00_in10_X <- file.path(censDir00_in10, paste0("census_2000_", STUSPS, ".csv"))
  
  if(!file.exists(censDir00_in10_X)){
    tic(paste("calculated 2000 demographic census data in 2010 boundaries in",name))
    #read demographic census data by tract, make data wider
    trac_censData00 <-  file.path(censDir00, paste0("census_2000_", STUSPS, ".csv"))%>%
      read.csv(#header=TRUE, 
               #stringsAsFactors=FALSE,
               #colClasses=c(pop_size="numeric")
               )%>%
      pivot_wider(
        names_from = variable,
        values_from = pop_size#,
        #values_fill =0 #TODO legitimate?
      ) %>% as.data.frame
    
    #TODO löschen
    #DF<-trac_censData00
    #if(any(is.na(DF))){
    #  new_DF <- DF[rowSums(is.na(DF)) > 0,
    #                            c(rep(TRUE,4),colSums(is.na(DF[,5:ncol(DF)])) > 0)
    #                            ]
    #  missingVar<-colnames(new_DF[,5:ncol(new_DF)])
    #  browser()
    #}
      
    
    cens00_in10X<-apply(trac_censData00, 1,function(row_cens){
      #row_data_in00 <-row_cens[-c("state","county","tract","GEO_ID")] 
      row_data_in00 <-row_cens[-c(1:4)] %>% unlist%>% as.numeric
      short_geo_id<-row_cens[["GEO_ID"]] %>% str_sub(.,-11,-1)
      crosswalk_sub <- crosswalk %>% filter(trtid00 == short_geo_id)
      
      if(nrow(crosswalk_sub) > 0){
        new_rows<-apply(crosswalk_sub,1,function(row_cross){
          GEO_ID_short10<-row_cross[["trtid10"]]
          state10 <- str_sub(GEO_ID_short10,1,2)
          county10 <-str_sub(GEO_ID_short10,3,6)
          tract10 <-str_sub(GEO_ID_short10,7,11)
          GEO_ID_10<-paste0("1400000US",GEO_ID_short10)
          
          weight <-row_cross[["weight"]] %>% as.numeric
          row_data_in10 <- row_data_in00 *weight 
          row_data_in10<-as.data.frame(t(row_data_in10))
          
          if(any(is.na(row_data_in10)))
            browser()
          
          cbind(state10,county10,tract10,GEO_ID_10,row_data_in10) %>% as.data.frame
        })%>% do.call(rbind,.) %>% return(.)
      }else{
        print(paste(short_geo_id, "not available in crosswalk"))
        return(NA)
      }
    })%>% do.call(rbind,.)
    
    colnames(cens00_in10X)<- colnames(trac_censData00)
    
    if(any(is.na(cens00_in10X)))
      browser()
    
    cens00_in10X<-cens00_in10X %>% pivot_longer(
      cols = !c("state", "county", "tract", "GEO_ID"),
      names_to = "variable",
      values_to = "pop_size"
    )
    
    if(any(is.na(cens00_in10X)))
      browser()
    
    write.csv(cens00_in10X,censDir00_in10_X)
    toc()
  }

  })

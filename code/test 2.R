packages <- c("acs","dplyr", "RCurl", "magrittr", "tigris","tidycensus", "stringr", "data.table", "tidyverse", "tictoc","rhdf5") #


for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
}
options(tigris_use_cache = FALSE)

year <- 2011
key <- "d44ca9c0b07372ada0b5243518e89adcc06651ef" 

STUSPS <-"AL" #AL

  tracts <- get_acs(geography = "tract", 
                    variables = "B19013_001", #dummy variable
                    state = STUSPS,  
                    year = as.numeric(year),
                    geometry = TRUE,
                    keep_geo_vars = TRUE,
                    key = key)
  
  if(year %in% 2011:2012)
    tracts$AFFGEOID <-paste0("1400000US",tracts$GEOID)
  
tracts2<- tracts  %>%
    filter(is.na(AFFGEOID))
  
tracts3 <- tracts(state = STUSPS, cb = FALSE, year = year)

#tracts4<- tracts3  %>%
#  filter(is.na(AFFGEOID))
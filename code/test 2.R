# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "tictoc","tigris", "sf")

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE))
  
}

#TODO l?schen
year <- 2000
agr_by <- "county"

#tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
#tracDir <- "/Users/default/Desktop/own_code2/data/02_tracts"
#exp_tracDir <- "/Users/default/Desktop/own_code2/data/03_exp_tracts"
#censDir <- "/Users/default/Desktop/own_code2/data/06_demog"
#cens_agrDir <- "/Users/default/Desktop/own_code2/data/07_dem.agr"

tmpDir <-  "C:/Users/Daniel/Desktop/paper2020/data/tmp"
tracDir <-  "C:/Users/Daniel/Desktop/paper2020/data/02_tracts"
exp_tracDir <- "C:/Users/Daniel/Desktop/paper2020/data/03_exp_tracts"
censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
cens_agrDir <- "C:/Users/Daniel/Desktop/paper2020/data/07_dem.agr"


# load states, so we can loop over them
states <- file.path(tmpDir, "states.csv") %>% read.csv

state <- states[1,] #TODO löschen
STUSPS <- state[["STUSPS"]] # TODO überall
name <- state[["NAME"]]

trac_censData <- paste0("census_", toString(year), "_", STUSPS, ".csv") %>%
  file.path(censDir, year, .) %>%
  read.csv %>%
  pivot_wider(
    names_from = variable,
    values_from = pop_size
  )

# load shape files
tracts <- paste0("tracts_", toString(year), "_", STUSPS, ".rds") %>%
  file.path(tracDir, toString(year), .) %>%
  readRDS(.)

#TODO if ! AFFGEOID %in% colnames()
tracts$AFFGEOID <-paste0("1400000US",tracts$STATE,tracts$COUNTY,tracts$TRACT)

#tracts2 <- tracts(state = STUSPS, cb = TRUE, year = 2016)

#https://www.census.gov/programs-surveys/geography/guidance/geo-identifiers.html
#https://stackoverflow.com/questions/49186893/remove-leading-0s-with-stringr-in-r

exp_tracData <- paste0("exp_trac_", toString(year), "_", STUSPS, ".csv") %>%
  file.path(exp_tracDir, year, .) %>%
  read.csv()

#exp_tracData$tract <-sapply(exp_tracData$tract, function(tractid){
#  while(tractid %% 10 == 0 && tractid >= 999)
#    tractid <- tractid/10
#  return(tractid)
#})

#https://walker-data.com/tigris-webinar/#21

joined <- geo_join(tracts, trac_censData, 
                   by_sp= "AFFGEOID",
                   by_df= "GEO_ID")

#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: read total burden data
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "DataCombine", "testthat", "tidyverse", "tictoc")#"sets","prob"

for (p in packages) {
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(dplyr.summarise.inform = FALSE)
options(dplyr.join.inform = FALSE)

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
agr_by <- args[10]
pafDir <- args[11]
totalBurdenDir <- args[12]
attrBurdenDir <- args[13]

year <- 2010
agr_by <- "nation"

tmpDir <- "/Users/default/Desktop/paper2020/data/tmp"
pafDir <- "/Users/default/Desktop/paper2020/data/08_paf"
totalBurdenDir <- "/Users/default/Desktop/paper2020/data/09_total_burden"
attrBurdenDir <- "/Users/default/Desktop/paper2020/data/10_attr_burd"
# TODO

pafDir <- file.path(pafDir, agr_by, year)
totalBurdenDir <- file.path(totalBurdenDir, agr_by)

attrBurdenDir <- file.path(attrBurdenDir, agr_by)
dir.create(attrBurdenDir, recursive = T, showWarnings = F)
attrBurdenDir <- file.path(attrBurdenDir, paste0("attr_burd_", toString(year), ".csv")) # .csv

if (!file.exists(attrBurdenDir)) {
  ## ----- read paf------
  states <- file.path(tmpDir, "states.csv") %>% read.csv
  regions <- states[, agr_by] %>% unique()
  pafs <- lapply(regions, function(region) {
    file.path(pafDir, paste0("paf_", toString(year), "_", region, ".csv")) %>%
      read.csv()
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()

  # Find and replace so it is compatible with other data
  replaces <- data.frame(
    from = c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"),
    to = c("Not Hispanic or Latino", "Hispanic or Latino")
  )

  pafs <- FindReplace(data = pafs, Var = "hispanic_origin", replaceData = replaces, from = "from", to = "to", exact = FALSE)

  replaces <- data.frame(
    from = c("WHITE", "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN OR PACIFIC ISLANDER", "BLACK OR AFRICAN AMERICAN"),
    to = c("White", "American Indian or Alaska Native", "Asian or Pacific Islander", "Black or African American")
  )

  pafs <- DataCombine::FindReplace(data = pafs, Var = "race", replaceData = replaces, from = "from", to = "to", exact = FALSE)
  ## ----- read total burden ---------
  files <- list.files(totalBurdenDir)
  total_burden <- lapply(files, function(file) {
    fileDir <- file.path(totalBurdenDir, file)

    total_burden <- read.delim(fileDir) %>%
      filter(Single.Year.Ages != "Not Stated") %>%
      mutate(
        Single.Year.Ages.Code = as.numeric(Single.Year.Ages.Code),
        YLD = sapply(Single.Year.Ages.Code, function(a) max(0, 75 - a)) # TODO right formula?
      )

    cause_icd <- total_burden$Notes[grepl("UCD - ICD-10 Codes:", total_burden$Notes, fixed = TRUE)]

    total_burden$Notes <- NULL
    total_burden <- total_burden[!apply(is.na(total_burden) | total_burden == "", 1, all), ]

    if (grepl("I20-I25 (Ischaemic heart diseases)", cause_icd, fixed = TRUE)) {
      total_burden$label_cause <- "cvd_ihd"
    }

    if (!"Hispanic.Origin" %in% colnames(total_burden)) {
      total_burden[, "Hispanic.Origin"] <- "all"
    }

    if (agr_by == "nation") {
      total_burden[, agr_by] <- "us"
    } # TODO
    return(total_burden)
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame

  ## ----- join total_burden and pafs-----
  #TODO
  #missing_label_causes <-prob::setdiff(pafs$label_cause, total_burden$label_cause)
  #if(length(missing_label_causes)){
  #  warning(paste("Causes missing:"))
  #  print(missing_label_causes)
  #} 
  
  burden_paf <- inner_join(total_burden,pafs,
    by = c(
      "Gender" = "gender",
      "Gender.Code" = "gender_label",
      "Race" = "race",
      "Year.Code" = "year",
      "Hispanic.Origin" = "hispanic_origin",
      "label_cause" = "label_cause" # ,
      # agr_by=agr_by #TODO
    )
  )
  
  #filter those, where age in correct interval
  burden_paf <- burden_paf %>%
    filter(
      (min_age <= Single.Year.Ages.Code & Single.Year.Ages.Code <= max_age) |
        (Single.Year.Ages.Code == 100 & 100 <= min_age)
    )
  
  # test_that("09_read burden join", {
  #  total_burden_sub <- total_burden %>%
  #    filter(
  #      Year.Code == year,
  #      Hispanic.Origin == "Hispanic or Latino"
  #    )
  #  pafs_sub <- pafs %>%
  #    filter(
  #      label_cause == "cvd_ihd",
  #      hispanic_origin != "all",
  #      race != "ASIAN",
  #      race != "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER"
  #    )
  # })

  ## ----- calculate attributable burden------

  test_that("09_calc distinct rows",{
    burden_paf_sub <- burden_paf %>%
      select(Single.Year.Ages.Code,Gender.Code,Race,Year.Code,Hispanic.Origin,label_cause)
    
    dub_ind<-duplicated(burden_paf_sub) | duplicated(burden_paf_sub, fromLast = TRUE)
    burden_paf_sub<-burden_paf[dub_ind,]
    
    expect_equal(nrow(burden_paf_sub),0)
  })
  
  attrBurden <- burden_paf %>%
    mutate(
      attrDeaths = Deaths * pafs,
      attrYLD = YLD * pafs
    )

  attrBurden <- attrBurden %>%
    group_by(Year,Gender,Gender.Code,Race,min_age,max_age,Hispanic.Origin) %>%
    #group_by(Year,Gender,Gender.Code,Race,Hispanic.Origin) %>%
    summarize(
      Deaths = sum(Deaths),
      YLD = sum(YLD),
      attrDeaths = sum(attrDeaths),
      attrYLD = sum(attrYLD)
    )

  test_that("09_read burden join2", {
    comp1 <- total_burden %>%
      group_by(Year,Gender,Gender.Code,Race,Hispanic.Origin) %>%
      summarize(
        Deaths = sum(Deaths),
        YLD = sum(YLD)
      ) 
    
    comp2<-attrBurden %>%
      group_by(Year,Gender,Gender.Code,Race,Hispanic.Origin) %>%
      summarize(
        Deaths = sum(Deaths),
        YLD = sum(YLD)
      ) 
      
    comp3<-inner_join(comp1,comp2, by = c("Year","Gender","Gender.Code","Race","Hispanic.Origin"))  
    comp3 %>%
      apply(1,function(row){
        expect_equal(row[["Deaths.x"]],row[["Deaths.y"]])
        expect_equal(row[["YLD.x"]],row[["YLD.y"]])
      })
  })
  fwrite(attrBurden, attrBurdenDir)
}

#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 12/18/2020
# Purpose: calculate PAF
#
#***************************************************************************
#*

# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("dplyr", "magrittr", "data.table", "tidyverse", "tictoc","testthat", "MALDIquant","ggplot2")

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}

# Pass in arguments
args <- commandArgs(trailingOnly = T)

year <- args[1]
tmpDir <- args[3]
exp_rrDir <- args[6]
censDir <- args[8]
cens_agrDir <- args[9]
agr_by <- args[10]
pafDir <- args[11]

# TODO löschen
#year <- 2000
#agr_by <- "nation"

#tmpDir <- "/Users/default/Desktop/own_code2/data/tmp"
#exp_rrDir <- "/Users/default/Desktop/own_code2/data/04_exp_rr"
#censDir <- "/Users/default/Desktop/own_code2/data/06_demog"
#cens_agrDir <- "/Users/default/Desktop/own_code2/data/07_census_agr"
#pafDir <- "/Users/default/Desktop/own_code2/data/08_paf"

#tmpDir <- "C:/Users/Daniel/Desktop/paper2020/data/tmp"
#exp_rrDir <- "C:/Users/Daniel/Desktop/paper2020/data/04_exp_rr"
#censDir <- "C:/Users/Daniel/Desktop/paper2020/data/06_demog"
#cens_agrDir <- "C:/Users/Daniel/Desktop/paper2020/data/07_dem.agr"
#pafDir <- "C:/Users/Daniel/Desktop/paper2020/data/08_paf"

# create directories
cens_agrDir <- cens_agrDir %>% file.path(., agr_by, year)
pafDir <- pafDir %>% file.path(., agr_by, year) # TODO drop year, everything in one file
dir.create(pafDir, recursive = T, showWarnings = F)

# load some data
states <- file.path(tmpDir, "states.csv") %>% read.csv()
causes_ages <- file.path(tmpDir, "causes_ages.csv") %>% read.csv()

### -----calculation
regions <- states[, agr_by] %>% unique()
for (region in regions) {
  pafDirX <- paste0("paf_", toString(year), "_", region, ".csv") %>%
    file.path(pafDir, .)

  if (!file.exists(pafDirX)) {
    tic(paste("Calculated PAF in", agr_by, region, "in year", year, "by pm"))

    cens_agr <- paste0("cens_agr_", toString(year), "_", region, ".csv") %>%
      file.path(cens_agrDir, .) %>%
      read.csv()

    censMetaAll <- paste0("cens_meta_", toString(year), ".csv") %>%
      file.path(censDir, "meta", .) %>%
      read.csv()

    pafs <- apply(causes_ages, 1, function(cause_age) {
      label_cause <- cause_age[["label_cause"]]
      age_group_idX <- cause_age[["age_group_id"]]
      tic(paste("Calculated PAF in", agr_by, region, "in year", year, "by pm", "for", label_cause))

      exp_rr <- ifelse(age_group_idX == "all ages",
        paste0(label_cause, ".csv"),
        paste0(label_cause, "_", age_group_idX, ".csv")
      ) %>%
        file.path(exp_rrDir, .) %>%
        read.csv

      getRR <- function(pm) {
        match.closest(pm, exp_rr$exposure_spline) %>%
          exp_rr[., "rr"] %>%
          return(.)
      }
      
      ifelse(age_group_idX == "all ages",
             censMeta <- censMetaAll,
             censMeta <- censMetaAll %>% filter(age_group_id == as.numeric(age_group_idX))
             )

      #TODO same thing via group by
      pafs <- apply(censMeta, 1, function(row) {
        variableX <- row[["variable"]]
        cens_agr_sub <- cens_agr %>% filter(variable == variableX)

        rr <- sapply(cens_agr_sub$pm, getRR) %>% as.numeric 
        props <- cens_agr_sub$prop
        
        x <- sum(props*(rr-1)) # TODO umbennen
        y<-x / (1 + x)
        #test_that("07_paf sum(props)", {
        #  expect_equal(sum(props), 1)
        #  expect_lt(y, 1) smaller
        #})
        return(y)
      })

      toc()

      result<-data.frame(
        label_cause = rep(label_cause, nrow(censMeta)),
        censMeta$variable,
        pafs
      )
      
      #test_that("07_paf sum(props)", {
      #  expect_equal(any(is.na(result)), FALSE)
      #})
      
      return(result)
    }) %>% do.call(rbind, .)

    pafs[, agr_by] <- region

    write.csv(pafs, pafDirX, row.names = FALSE)
    toc()
  }
}
""
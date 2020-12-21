#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# clear memory
rm(list = ls(all = TRUE))

#install packages if missing
packages <-c("cdcfluview","censusapi","data.table","dplyr", "ggplot2", "magrittr",
             "MALDIquant","plyr","RCurl","sf","sp","stringr","testthat","tidyverse","tigris","tmap")

options(tigris_use_cache = FALSE)
for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

# download rhdf5
if ("rhdf5" %in% rownames(installed.packages()) == FALSE) {
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  BiocManager::install("rhdf5")
}

# runtime configuration
# run cripts from command line depending on OS
if (Sys.info()["sysname"] == "Darwin") {
  runscript <- function(script, args = "") {
    system(paste("Rscript", script, args))
  }
} else if (Sys.info()["sysname"] == "Windows") {
  exec <- paste0("C:/Program Files/R/R-", R.Version()$major, ".", R.Version()$minor, "/bin/Rscript.exe")
  exec <- shQuote(exec)
  runscript <- function(script, args = "") {
    system(paste(exec, "--vanilla", script, args))
  }
}else {
  print(paste("no handler for", Sys.info()["sysname"], "implemented yet."))
}

## ----------------directories--------------------------------------------------------------------------------
# create data directory, setwd
code.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
h_root <- dirname(code.dir)
setwd(h_root)

# create directory, where all downloaded and calculated data is stored
data.dir <- file.path(h_root, "data")
dir.create(data.dir, recursive = T, showWarnings = F)


# directory contains variables used in calculations, which several scripts might need
tmp.dir <- file.path(data.dir, "tmp")
dir.create(tmp.dir, recursive = T, showWarnings = F)

# directory for downloaded PM exposure data
exp.dir <- file.path(data.dir, "01_exposure")
dir.create(exp.dir, recursive = T, showWarnings = F)


# directory for downloaded TIGER/Line tract shape files
trac.dir <- file.path(data.dir, "02_tracts")
dir.create(trac.dir, recursive = T, showWarnings = F)

# this directory contains calculated year - census tract - pm level tuples
trac.exp.dir <- file.path(data.dir, "03_exp_tracts")
dir.create(trac.exp.dir, recursive = T, showWarnings = F)

exp.rr.dir <- file.path(data.dir, "04_exp_rr")
if (!file.exists(exp.rr.dir)) warning("The mrbrt_summary files from Cohen (2019) need to be downloaded")

# directory for downloaded demographic census data
dem.dir <- file.path(data.dir, "06_demog")
dir.create(dem.dir, recursive = T, showWarnings = F)

# directory for demographic data grouped by PM exposure and aggregated by county/hhs region/census region
dem.agr.dir <- file.path(data.dir, "07_dem.agr")
dir.create(dem.agr.dir, recursive = T, showWarnings = F)
agr_by <- "Census_Region" # c("county","Census_Region","Census_division","hhs_region_number","state","nation")

paf.dir <- file.path(data.dir, "08_paf")
dir.create(paf.dir, recursive = T, showWarnings = F)

# paths of scripts
download.cens.script <- file.path(code.dir, "01_download_cens.R")
interp.script <- file.path(code.dir, "02_interp.R")
download.script <- file.path(code.dir, "03_download_other.R")
assignTract.script <- file.path(code.dir, "04_ass_trac.R")
mrbrtRR.script <- file.path(code.dir, "05_mrbrt_rr.R")
cens_agr.script <- file.path(code.dir, "06_aggregate.R")
paf.script <- file.path(code.dir, "07_paf.R")

# load packages, install if missing
packages <- c()

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
  library(p, character.only = T)
}



#--------parameters of code-------------------
years <- c(2000)


#TODO complement ding stimmt noch nicht. erfasst nicht alle
 runscript(script=download.cens.script, args = paste(dem.dir,tmp.dir, 2000))
# runscript(script=download.cens.script, args = paste(censDir,tmpDir, 2010))
# runscript(script=interp.script, args = paste(censDir,tmpDir, 2001))

for (year in years) {
  args <- paste(
    year, # 1
    data.dir, # 2
    tmp.dir, # 3
    exp.dir, # 4
    trac.dir, # 5
    exp.rr.dir, # 6
    exp_rrDir, # 7
    dem.dir, # 8
    dem.agr.dir, # 9 
    agr_by, # 10
    paf.dir # 11
  ) 

   runscript(script=download.script, args = args)
   runscript(script=assignTract.script, args = args)
   runscript(script=mrbrtRR.script, args = args)
  runscript(script = cens_agr.script, args = args)
  runscript(script = paf.script, args = args)
}

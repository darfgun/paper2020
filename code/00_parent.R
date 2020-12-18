#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# clear memory
rm(list=ls(all=TRUE))

# runtime configuration
runscript <-function(script, args=""){}
if (Sys.info()["sysname"] == "Darwin") {
  runscript <-function(script, args=""){system(paste("Rscript", script, args))} 
  
} else if (Sys.info()["sysname"] == "Windows"){
  exec <- paste0("C:/Program Files/R/R-",R.Version()$major,".",R.Version()$minor,"/bin/Rscript.exe") 
  exec <- shQuote(exec)
  runscript <-function(script, args=""){system(paste(exec, "--vanilla", script, args))}
  
}else if (Sys.info()["sysname"] == "Linux"){
  #https://stackoverflow.com/questions/3560641/running-an-rscript-on-mac-os-x
  
}else {
    print(paste("no handler for", Sys.info()["sysname"], "implemented yet."))
}

##----------------directories---------
#create data directory, setwd
code.dir<-dirname(rstudioapi::getSourceEditorContext()$path)
h_root<-dirname(code.dir) 
setwd(h_root)

#create directory, where downloaded and calculated data is stored
dataDir <-file.path(h_root, "data")
dir.create(dataDir, recursive = T, showWarnings = F)


#directory contains variables used in calculations
tmpDir <-file.path(dataDir, "tmp")
dir.create(tmpDir, recursive = T, showWarnings = F)

#directory for downloaded PM exposure data
expDir <-file.path(dataDir, "01_exposure")
dir.create(expDir, recursive = T, showWarnings = F)


#directory for downloaded tract shape files
tracDir <-file.path(dataDir, "02_tracts")
dir.create(tracDir, recursive = T, showWarnings = F)

#this directory contains calculated year - census tract - pm level tuples
exp_tracDir <-file.path(dataDir, "03_exp_tracts")
dir.create(exp_tracDir, recursive = T, showWarnings = F)

exp_rrDir <-file.path(dataDir, "04_exp_rr")
if (!file.exists(exp_rrDir)) warning("The mrbrt_summary files from Cohen (2019) need to be downloaded")


#directory for downloaded census data
trac_rrDir <-file.path(dataDir, "05_tracts_rr")
dir.create(trac_rrDir, recursive = T, showWarnings = F)

#directory for downloaded census data
censDir <-file.path(dataDir, "06_census")
dir.create(censDir, recursive = T, showWarnings = F)

#directory for census data aggregated by PM exposure and county/hhs region/census region
cens_agrDir <-file.path(dataDir, "07_census_agr")
dir.create(cens_agrDir, recursive = T, showWarnings = F)
agr_by <- "Census_Region" #c("county","Census_Region","Census_division","hhs_region_number","state","nation")

pafDir <-file.path(dataDir, "08_paf")
dir.create(pafDir, recursive = T, showWarnings = F)

#paths of scripts
download.cens.script <- file.path(code.dir, '01_download_cens.R')
interp.script <- file.path(code.dir, '01_interp.R')
download.script <- file.path(code.dir, '01_download.R')
assignTract.script <- file.path(code.dir, '02_ass_trac.R')
mrbrtRR.script <- file.path(code.dir, '03_mrbrt_rr.R')
assignRR.script <- file.path(code.dir, '03_rr_trac.R')
cens_agr.script <- file.path(code.dir, '04_aggregate.R')
paf.script <- file.path(code.dir, '05_paf.R')

# load packages, install if missing
packages <- c() 

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}



#--------parameters of code-------------------
years <- c(2000)

# Run code
#complement ding stimmt noch nicht. erfasst nicht alle
#runscript(script=download.cens.script, args = paste(censDir,tmpDir, 2000))
#runscript(script=download.cens.script, args = paste(censDir,tmpDir, 2010))
#runscript(script=interp.script, args = paste(censDir,tmpDir, 2001))

for(year in years){
  args <- paste(year, #1
                dataDir,#2
                tmpDir,#3
                expDir,#4
                tracDir,#5
                exp_tracDir,#6
                exp_rrDir,#7
                trac_rrDir,#8
                censDir, #9
                cens_agrDir, #10
                agr_by, #11
                pafDir) #12
  
  #runscript(script=download.script, args = args)
  #runscript(script=assignTract.script, args = args)
  #runscript(script=mrbrtRR.script, args = args)
  #runscript(script=assignRR.script, args = args)
  #runscript(script=cens_agr.script, args = args)
  runscript(script=paf.script, args = args)
}
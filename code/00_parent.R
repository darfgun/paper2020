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
  exec <- paste("C:/Program Files/R/R-",R.Version()$major,".",R.Version()$minor,"/bin/Rscript.exe",sep="") 
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
if (!file.exists(dataDir)){
  dir.create(dataDir)
} 

#directory contains variables used in calculations
tmpDir <-file.path(dataDir, "tmp")
if (!file.exists(tmpDir)){
  dir.create(tmpDir)
  fileConn<-file(file.path(tmpDir,"readme.txt"))
  writeLines(c("This directory contains variables used in calculations"), fileConn)
  close(fileConn)
} 

#directory for downloaded PM exposure data
expDir <-file.path(dataDir, "01_exposure")
if (!file.exists(expDir)){
  dir.create(expDir)
  fileConn<-file(file.path(expDir,"readme.txt"))
  writeLines(c("This directory contains PM exposure data","downloaded from ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"), fileConn)
  close(fileConn)
} 

#directory for downloaded tract shape files
tracDir <-file.path(dataDir, "02_tracts")
if (!file.exists(tracDir)){
  dir.create(tracDir)
  fileConn<-file(file.path(tracDir,"readme.txt"))
  writeLines(c("This directory contains census shape files","downloaded via the R package tigris"), fileConn)
  close(fileConn)
} 

#this directory contains calculated year - census tract - pm level tuples
exp_tracDir <-file.path(dataDir, "03_exp_tracts")
if (!file.exists(exp_tracDir)){
  dir.create(exp_tracDir)
  fileConn<-file(file.path(exp_tracDir,"readme.txt"))
  writeLines(c("This directory contains calculated year - census tract - pm level tuples"), fileConn)
  close(fileConn)
} 

#directory for downloaded census data
censDir <-file.path(dataDir, "04_census")
if (!file.exists(censDir)){
  dir.create(censDir)
  fileConn<-file(file.path(censDir,"readme.txt"))
  writeLines(c("This directory contains census data","downloaded via the R package censusapi"), fileConn)
  close(fileConn)
} 

#paths of scripts
download.script <- file.path(code.dir, '01_download.R')
assignTract.script <- file.path(code.dir, '02_ass_trac.R')

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
for(year in years){
  args <- paste(year,
                dataDir,
                tmpDir,
                expDir,
                tracDir,
                exp_tracDir,
                censDir)
  
  runscript(script=download.script, args = args)
  runscript(script=assignTract.script, args = args)
}

states()%>% as.data.frame %>% pull(REGION, NAME)
  #pull("REGION","DIVISION","STATEFP","GEOID","NAME")


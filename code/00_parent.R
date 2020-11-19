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
if (Sys.info()["sysname"] == "Darwin") {
  exec <- "/usr/bin/Rscript"
  exec <- shQuote(exec)
  
} else if (Sys.info()["sysname"] == "Windows"){
  exec <- paste("C:/Program Files/R/R-",R.Version()$major,".",R.Version()$minor,"/bin/Rscript.exe",sep="") #TODO
  exec <- shQuote(exec)
  
}else if (Sys.info()["sysname"] == "Linux"){
  #https://stackoverflow.com/questions/3560641/running-an-rscript-on-mac-os-x
  
}else {
    print(paste("no handler for", Sys.info()["sysname"], "implemented yet."))
}

#create data directory, setwd
code.dir<-dirname(rstudioapi::getSourceEditorContext()$path)
h_root<-dirname(code.dir) 
setwd(h_root)

#create directory, where downloaded and calculated data is stored
dataDir <-file.path(h_root, "data")
if (!file.exists(dataDir)){
  dir.create(dataDir)
} 

#directory, where PM expsure data will be downloaded to
expDir <-file.path(dataDir, "01_exposure")
if (!file.exists(expDir)){
  dir.create(expDir)
  fileConn<-file(file.path(expDir,"readme.txt"))
  writeLines(c("This directory contains PM exposure data","downloaded from ftp://stetson.phys.dal.ca/jmeng/HEI2018-HistoricalPM25/historicalPM25/"), fileConn)
  close(fileConn)
} 

#directory contains variables used in calculations
tmpDir <-file.path(dataDir, "tmp")
if (!file.exists(tmpDir)){
  dir.create(tmpDir)
  fileConn<-file(file.path(tmpDir,"readme.txt"))
  writeLines(c("This directory contains variables used in calculations"), fileConn)
  close(fileConn)
} 

#paths of scripts
download.script <- file.path(code.dir, '01_download.R')

# load packages, install if missing
packages <- c() #"RCurl","magrittr" 

for(p in packages){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p)
  }
  library(p, character.only = T)
}

#--------parameters of code-------------------
years <- c(2010)

for(year in years){
  #TODO
  
  #args <- paste("test")
  #command <- "open \'/usr/bin/Rscript\' \'/Users/usr/Documents/Shares.R\'"
  #command <- paste(exec, " ",download.script, sep="")
  
  
  command <- paste(exec, " --vanilla ", download.script, sep="")
  command
  system(command)
  
  
  #args <- paste(output.version,
                #draws.required,
                #year,
                #hap.stgpr.runid)
  
  
  #mem <- "-l m_mem_free=2G"
  #fthread <- "-l fthread=5"
  #runtime <- "-l h_rt=01:00:00"
  #archive <- "" # no j-drive access needed
  #jname <- paste0("-N air_paf_cataract_",year)
  
  #system(paste("qsub",jname,mem,fthread,runtime,archive,project,"-q all.q",sge.output.dir,rshell,cataract.calc.script,args))
}


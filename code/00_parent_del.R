
#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 11/15/2020
# Purpose: parent file for project
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

# clear memory
rm(list=ls())

#create data directory, setwd
mainDir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(mainDir)

subDir <- "data"
dataDir <-file.path(mainDir, subDir)

if (!file.exists(dataDir)){
  dir.create(dataDir)
} 

  




DF<-censData_joined
if(any(is.na(DF))){
  new_DF <- DF[rowSums(is.na(DF)) > 0,]
#  browser()
}

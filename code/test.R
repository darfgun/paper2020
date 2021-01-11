df<-data.frame(a = list(0,1), b=list(2,3))
list<-apply(df,1,function(row){
  return(sum(row))
})

library(stringr)

ex_GEO_ID<-censData10[[1,"GEO_ID"]] 

ex_GEO_ID <- ex_GEO_ID %>%toString

ex_GEO_ID3<- str_pad(ex_GEO_ID, 11, pad = "0")




censData102 <- censData10 %>%
  mutate(GEO_ID = sprintf("%11d", GEO_ID))

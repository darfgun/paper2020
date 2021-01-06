
#library(foreign)
#data <- read.dta("/Users/default/Downloads/interpolate_to_2000/crosswalk_2010_2000.dta")

formatC(5, width=6, flag="0")

a <-paste0(formatC(1, width=2, flag="0"),
                     formatC(23, width=3, flag="0"),
                     formatC(456, width=6, flag="0")
) 

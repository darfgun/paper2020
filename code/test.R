SEQ  <- seq(1,100000)
pb   <- txtProgressBar(1, 100000, style=3)
TIME <- Sys.time()
for(i in SEQ){
     Sys.sleep(0.00002)
     setTxtProgressBar(pb, i)
}
Sys.time() - TIME
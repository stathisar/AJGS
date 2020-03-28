#import of replicated geo-referenced data
Kalamos_tw.dec <- read.csv("/home/stathis/Desktop/Kalamos_r_script/processed/Kalamos_tw.dec.csv")
#import of initially geo-referenced data
Kalamos_tw.int <- read.csv("/home/stathis/Desktop/kalamos_r_script/processed/replicated/Kalamos_tw.int.csv")

#indicatively: deletion of unneeded fields of the dataframe
remove <- c("X.2", "lastmatch", "idd", "iddint", "nofdelet", "textnew", "deleted")

Kalamos_tw.dec.final <- Kalamos_tw_dec_total[ , !(names(Kalamos_tw_dec_total) %in% remove)]

#rbind
Kalamos_total_prerandomize <- rbind(Kalamos_tw.int, Kalamos_tw.dec)
write.csv(Kalamos_total_prerandomize, file = "/home/stathis/Desktop/kalamos_r_script/processed/replicated/Kalamos_total_prerandomize.csv")

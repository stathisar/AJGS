#
require("splitstackshape")
# dataset

Kalamos_tw <- read.csv("/home/stathis/Desktop/kalamos_r_script/processed/Kalamos_tw.xy.csv", encoding = "UTF-8")
Kalamos_tw$reference2 <- Kalamos_tw$reference
Kalamos_tw.2 <- expandRows(Kalamos_tw, "reference2")

#district .dec and .int
id <- as.numeric(row.names(Kalamos_tw.2))
Kalamos_tw.int   <- Kalamos_tw.2[abs(id-round(id))<1e-6, ]
Kalamos_tw.dec   <- Kalamos_tw.2[abs(id-round(id))>1e-6, ]
Kalamos_tw.dec$lastmatch <- NA

#export as csv
write.csv(Kalamos_tw.int, file = "/home/stathis/Desktop/kalamos_r_script/processed/replicated/Kalamos_tw.int.csv")
write.csv(Kalamos_tw.dec, file = "/home/stathis/Desktop/kalamos_r_script/processed/replicated/Kalamos_tw.dec.csv")

#clear dataframes and console
rm(Kalamos_tw, Kalamos_tw.2, Kalamos_tw.int, Kalamos_tw.dec)
cat("\014")

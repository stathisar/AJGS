Kalamos_tw$reference <- 0
Kalamos_tw$x <- 0
Kalamos_tw$y <- 0
Kalamos_tw$precision <- 0
Kalamos_tw$comment <- NA
View(Kalamos_tw[1:10, ])


for(im in 1:NROW(Kalamos_tw){
  for(ig in 1:nrow(geolocations)){
    if(grepl(geolocations$name[ig], Kalamos_tw$Text[im])){
      Kalamos_tw$reference[im] <- Kalamos_tw$reference[im] + 1
      Kalamos_tw$x[im] <- geolocations$Long[ig]
      Kalamos_tw$y[im] <- geolocations$Lat[ig]
      Kalamos_tw$precision[im] <- geolocations$Precision[ig]
      Kalamos_tw$comment[im] <- geolocations$duplicates[ig]
      cat("coordinates added between im:", im, "and ig:", ig)
      ig <- ig + 1
    }else{
      cat("no word match found between im:", im, "and ig:", ig)
      ig <- ig + 1
    }}
  im <- im + 1
  ig <- 1
}

write.csv(Kalamos_tw.1.dec, file="/home/stathis/Desktop/kalamos_r_script/processed/Kalamos_tw.xy.csv")

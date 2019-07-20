#estimating the number of times that the assign xy data loop will be applied
Kalamos_tw.dec$idd <- as.numeric(Kalamos_tw.dec$X.1)
Kalamos_tw.dec$iddint <- Kalamos_tw.dec$idd - round(Kalamos_tw.dec$idd)
Kalamos_tw.dec$nofdelet <- Kalamos_tw.dec$iddint * 10


#preparing the file
Kalamos_tw.dec$textnew <- Kalamos_tw.dec$Text
Kalamos_tw.dec$lastmatch <- NA
Kalamos_tw.dec$lastmatch <- as.character(Kalamos_tw.dec$lastmatch)
Kalamos_tw.dec$textnew <- as.character(Kalamos_tw.dec$Text)
Kalamos_tw.dec$deleted <- 0
#apply loop for differing the last match
#until here everything works perfectly
# 1-400
for(im in 1:NROW(Kalamos_tw){ 
  for(d in 1:Kalamos_tw.dec$nofdelet[im]) {
    { for(g in 1:nrow(geolocations) +1){
      if(g == nrow(geolocations) +1){
        Kalamos_tw.dec$textnew[im] <- gsub(Kalamos_tw.dec$lastmatch[im], "", Kalamos_tw.dec$textnew[im])
        Kalamos_tw.dec$deleted[im] <- Kalamos_tw.dec$deleted[im] + 1 }
      else {
        if(grepl(geolocations$name[g], Kalamos_tw.dec$textnew[im])){
          Kalamos_tw.dec$lastmatch[im] <- paste(geolocations$name[g])
          cat("word match detected in im:", im, "and g:", g)
          g <- g + 1
        } else {
          cat("no word match detected in im:", im, "and g:", g)
          g <- g + 1
        }}
    }
      d <- d + 1
      g <- 1 }    
  }
  g <- 1
  im <- im + 1
  d <- 1
}      
#assign the xy values and export the file as csv
cat("\014")
for(im in 1:NROW(Kalamos_tw){
  for(ig in 1:nrow(geolocations)){
    if(grepl(geolocations$name[ig], Kalamos_tw.dec$textnew[im])){
      Kalamos_tw.dec$x[im] <- geolocations$Long[ig]
      Kalamos_tw.dec$y[im] <- geolocations$Lat[ig]
      Kalamos_tw.dec$precision[im] <- geolocations$Precision[ig]
      Kalamos_tw.dec$comment[im] <- geolocations$duplicates[ig]
      cat("coordinates added between im:", im, "and ig:", ig)
      ig <- ig + 1
    }else{
      cat("no word match found between im:", im, "and ig:", ig)
      ig <- ig + 1
    }}
  im <- im + 1
  ig <- 1
}

write.csv(Kalamos_tw.dec, file="/home/stathis/Desktop/Kalamos_r_script/processed/Kalamos_tw.dec.csv")

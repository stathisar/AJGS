rm(list = ls())
#import of kalamos tweet-corpus
require("xlsx")
#Sys.setlocale(category = "LC_ALL", locale = "Greek")

Kalamos_tw <- read.xlsx("/home/stathis/Desktop/kalamos_r_script/kalamos_tweets.xlsx",1,header=T,colClasses=c("character"), encoding = "UTF-8")

#import of geolocations
geolocations <- read.xlsx("/home/stathis/Desktop/kalamos_r_script/geolocations_kalamos.xlsx",1,header=T,colClasses=c("character"), encoding = "UTF-8")


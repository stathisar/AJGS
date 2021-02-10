rm(list = ls())
require("RTextTools")
require("splitstackshape")
require("spdep")
require("splancs")
require("rgdal")
require("plyr")
require("rgeos")
require("geoR")

start.time <- Sys.time()
setwd("/home/stathis/Desktop/research/ajgs/article/")

#dataset load
alltweets <- read.csv(file = "./all-tweets.csv")
alltweets <- alltweets[ ,c("Text", "X.M.._twitter_id", "X.M..object_posted_time.")]
alltweets$Text <- as.character(alltweets$Text)
alltweets$Text <- tolower(alltweets$Text)
#remove @, urls
#alltweets$Text <- gsub("@[a-z,A-Z,0-9]*", "", alltweets$Text)
#alltweets$Text <- gsub("rt ", "", alltweets$Text)
#alltweets$Text <- gsub("https://t.co/[a-z,A-Z,0-9]*", "", alltweets$Text)
#alltweets$Text <- gsub("http://t.co/[a-z,A-Z,0-9]*", "", alltweets$Text)
#training dataset load
trainingdata <- read.csv(file = "./data/training.dataset.r1.csv")
names(trainingdata)
trainingdata$Text <- tolower(trainingdata$Text)
#trainingdata$Text <- gsub("rt ", "", trainingdata$Text)
#trainingdata$Text <- gsub("https://t.co/[a-z,A-Z,0-9]*", "", trainingdata$Text)
#trainingdata$Text <- gsub("http://t.co/[a-z,A-Z,0-9]*", "", trainingdata$Text)
#trainingdata$Text <- gsub("@[a-z,A-Z,0-9]*", "", trainingdata$Text)

#dtMatrix
dtMatrix <- create_matrix(trainingdata["Text"])

#create container: fire identification 
container.fire.event.ident <- create_container(dtMatrix, 
                                               trainingdata$FIRE.EVENT.IDENTIFICATION, 
                                               trainSize = 1:NROW(trainingdata), virgin = FALSE)
##create container: other emergency
container.other.emerg.event <- create_container(dtMatrix, 
                                                trainingdata$OTHER.EMERGENCY.EVENT, 
                                                trainSize = 1:NROW(trainingdata), 
                                                virgin = FALSE)
##create container: consequences
container.consequences <- create_container(dtMatrix, 
                                           trainingdata$CONSEQUENCES.OF.FIRE..KALAMOS.EVENT., 
                                           trainSize = 1:NROW(trainingdata), 
                                           virgin = FALSE)
#create container: event tracking
container.event.tracking <- create_container(dtMatrix, 
                                             trainingdata$FIRE.EVENT.TRACKING.OF.KALAMOS, 
                                             trainSize = 1:NROW(trainingdata), 
                                             virgin = FALSE)
#create container: emergency management
container.emerg.management <- create_container(dtMatrix, 
                                               trainingdata$FIRE.EMERGENCY.MANAGEMENT.OF.KALAMOS.EVENT, 
                                               trainSize = 1:NROW(trainingdata), 
                                               virgin = FALSE)

#training of SVM Models
model1 <- train_model(container.fire.event.ident, "SVM", kernel="linear", cost=1)
model2 <- train_model(container.other.emerg.event, "SVM", kernel="linear", cost=1)
model3 <- train_model(container.consequences, "SVM", kernel="linear", cost=1)
model4 <- train_model(container.event.tracking, "SVM", kernel="linear", cost=1)
model5 <- train_model(container.emerg.management, "SVM", kernel="linear", cost=1)

#new data
predictionData <- alltweets

#create a prediction document term matrix
predMatrix <- create_matrix(predictionData, originalMatrix = dtMatrix)

#create the corresponding container
predSize = length(predictionData$Text)
#predSize
#NROW(predMatrix)
predictionContainer <- create_container(predMatrix, 
                                        labels = rep(0, NROW(predictionData)), 
                                        testSize = 1:NROW(predictionData), 
                                        virgin = FALSE)

#predict
results1 <- classify_model(predictionContainer, model1)
results2 <- classify_model(predictionContainer, model2)
results3 <- classify_model(predictionContainer, model3)
results4 <- classify_model(predictionContainer, model4)
results5 <- classify_model(predictionContainer, model5)
#write.csv(results1, "/media/stathis/Maxtor/article/results1.csv")
#write.csv(results2, "/media/stathis/Maxtor/article/results2.csv")
#write.csv(results3, "/media/stathis/Maxtor/article/results3.csv")
#write.csv(results4, "/media/stathis/Maxtor/article/results4.csv")
#write.csv(results5, "/media/stathis/Maxtor/article/results5.csv")

#join tables results and predictionData
results.total <- cbind(predictionData, results1, results2, results3, results4, results5)
#names(results.total)
write.csv(results.total, "./data/predicted.r1jan28.csv")

#predicted.total <- read.csv("./data/predicted.r1.csv")
predicted.r1.sample <- results.total[sample(results.total$X.M..id.,  200), ]
write.csv(predicted.r1.sample, "./data/predicted.r1.samplejan28.csv")
#predicted1 <- merge(results1, predictionData, by = "id")

#georeference
kalamos <- results.total
#kalamos <- read.csv("./data/predicted.r1jan28.csv")
names(kalamos)
kalamos$Text <- tolower(kalamos$Text)
kalamos$incl <- NA
#kalamos$Text[1:10]
#georeferencing of classified tweets

#import of geolocations
geolocations <- read.csv("./georeference/geolocations_kalamos.csv")

#geolocations with known areas experimental
#geolocations <- read.csv("/home/stathis/Desktop/research/ajgs/article/data/training.dataset.ambiguities.csv")

geolocations$name <- as.character(tolower(geolocations$name))
kalamos$reference <- 0
kalamos$x <- 0
kalamos$y <- 0
kalamos$precision <- 0
kalamos$comment <- NA
#creation of dataframe in which the georeferencing output will be stored

tweets.georeferenced <- data.frame(matrix(nrow = 0, 
                                          ncol = NCOL(kalamos)))
#loop that adds lat lon information and replicates tweets according to geolocation word detection
for (i in 1:NROW(geolocations)){
  tweets.test <- kalamos[ grepl(geolocations$name[i], 
                                kalamos$Text), ]
  if(NROW(tweets.test) == 0){
  }else{
    tweets.test$x <- geolocations$Long[i]
    tweets.test$y <- geolocations$Lat[i]
    tweets.test$detected <- geolocations$name[i]
    tweets.test$precision <- geolocations$precision[i]
    tweets.test$comment <- geolocations$duplicates[i]
    tweets.georeferenced <- rbind(tweets.georeferenced, tweets.test)
    kalamos$incl[grepl(geolocations$name[i], kalamos$Text)] <- "g"
  }
  cat(i/NROW(geolocations)*100, "% completed", "\r")
}


sample.incl <- kalamos[ sample(kalamos$X.M.._twitter_id, 200), ]
write.csv(sample.incl, "./data/sample.incl.csv")


#export geo-referenced prerandomized dataset
write.csv(tweets.georeferenced, 
          "./data/georeferenced.prerandomizedjan28.csv")


#start of randomization part
final.results.prerandomized <- tweets.georeferenced
#removal of non needed dataframes

rm(tweets.test, tweets.georeferenced)

#keeping only data with coordinates
#final.results.prerandomized <- subset(final.results.prerandomized, y > 0)
#final.results.prerandomized$

#import of area.map shapefile
area.map <- readOGR(dsn = "./georeference/shapefiles/kallikratis-wgs84-utf8.shp", 
                    layer = "kallikratis-wgs84-utf8")
#creation of unique id to area.map
area.map@data$id <- as.numeric(row.names(area.map@data)) + 1

final.results.prerandomized$x <- gsub(",", ".", final.results.prerandomized$x)
final.results.prerandomized$y <- gsub(",", ".", final.results.prerandomized$y)
final.results.prerandomized$x <- as.numeric(final.results.prerandomized$x)
final.results.prerandomized$y <- as.numeric(final.results.prerandomized$y)


#creation of spatial points dataframe
spdf.prerandomize <- SpatialPointsDataFrame(coords = final.results.prerandomized[ ,c("x", "y")], 
                                            data = final.results.prerandomized, 
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#spatial join of municipality name on spdfprerandomize
spdf.prerandomize@data$NAME <- NA
spdf.prerandomize@data$id <- NA
spdf.prerandomize@data[, c("NAME", "id")] = over(spdf.prerandomize, 
                                                 area.map[, c("NAME", "id")])



point.count <- summary(spdf.prerandomize@data$NAME)
point.count <- as.data.frame(point.count)
point.count$NAME <- row.names(point.count)

map.count <- merge(area.map, point.count, by.area.map = NAME, 
                   by.point.count = NAME, 
                   all.area.map = TRUE)
#aligning ids
map.count@data$id <- as.numeric(map.count@data$id)
map.count@data$id <- as.numeric(map.count@data$id + 1)
rownames(map.count@data) <- map.count@data$id
map.count@data$point.count[is.na(map.count@data$point.count)] <- 0 
#create randompoints and spdf in which randompoints will be stored

generated.random.points <- SpatialPoints(data.frame(x = 0, y = 0))[-1,]
map.count@data$point.count  <- as.numeric(map.count@data$point.count)

for (i in 1:nrow(map.count)) {
  if (map.count@data$point.count[i] == 0){
    i = i+1}
  else {
    generated.random.points <- append(generated.random.points, 
                                      spsample(map.count[i, ], 
                                               n=map.count@data$point.count[i], 
                                               "random"))
    i <- i+1
  }
}

#put all random coords in a dataframe

random.points <- data.frame()
random.points$x <- as.numeric()
random.points$y <- as.numeric()

#add Counter by column
#table$Counter <- with(table, ave(seq_along(NAME), NAME, FUN = seq_along))

for (i in 1:length(generated.random.points)){
  random.points <- rbind(random.points, generated.random.points[[i]]@coords)
  i <- i+1
}
write.csv(random.points, file = "./randompointswithoutvalues.csv")
#remove data that are not used any more
rm(generated.random.points, point.count, spdf.prerandomize)
#creating a uniqueid of finalresultsprerandomize
final.results.prerandomized$munname <- NA
final.results.prerandomized$counter <- 0
final.results.prerandomized$uniqueid <- NA
spdf.prerandomized <- SpatialPointsDataFrame(coords = final.results.prerandomized[ , c("x", "y")], 
                                             data = final.results.prerandomized, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

final.results.prerandomized$munname <- over(spdf.prerandomized, area.map[, "NAME"])
final.results.prerandomized$municipality <- paste(as.character(unlist(final.results.prerandomized$munname)))
final.results.prerandomized$counter <-  with(final.results.prerandomized, 
                                             ave(seq_along(municipality), 
                                                 municipality, 
                                                 FUN=seq_along))
final.results.prerandomized$uniqueid <- paste(final.results.prerandomized$municipality, 
                                              final.results.prerandomized$counter, 
                                              sep = "")
rm(spdf.prerandomized)

#creating unique id of randompoints
names(random.points)
NROW(random.points)
random.points$munname <- NA

random.points$counter <- 0
random.points$uniqueid <- NA
random.points$distance.from.epic <- NA
spdf.random.points <- SpatialPointsDataFrame(coords = random.points[ ,c("x", "y")], 
                                             data = random.points, 
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
random.points$munname <- over(spdf.random.points, area.map[ , "NAME"])
#      random.points$distance.from.epic <- over(spdf.random.points, area.map[ , "distance.from.epic"])
random.points$municipality <- paste(as.character(unlist(random.points$munname)))
random.points$counter <- with(random.points, 
                              ave(seq_along(municipality), 
                                  municipality, 
                                  FUN = seq_along))
random.points$uniqueid <- paste(random.points$municipality, random.points$counter, sep = "")
rm(spdf.random.points)
#merge finalresultsprerandomized and randompoints
final.results.prerandomized.2 <- final.results.prerandomized
final.results.prerandomized <- merge(final.results.prerandomized, 
                                     random.points, 
                                     by = "uniqueid", 
                                     all.final.results.prerandomized = TRUE)


#unlist results
for (i in 1:length(final.results.prerandomized)){
  final.results.prerandomized[ , i] <- paste(unlist(final.results.prerandomized[ ,i]))
  i <- i + 1
}
write.csv(final.results.prerandomized, file = "./tweets.randomized.final.jan.28.csv")

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

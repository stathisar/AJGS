rm(list = ls())

require("splitstackshape")
require("spdep")
require("splancs")
require("rgdal")
require("plyr")
require("rgeos")
require("rgdal")

#Sys.setlocale(category = "LC_ALL", locale = "Greek")
#import of kalamos tweet-corpus
output <- read.csv("./kalamos.csv")

#import of geolocations
    geolocations <- read.csv("./geolocations.csv")
              geolocations$name <- as.character(tolower(geolocations$name))
              output$reference <- 0
              output$x <- 0
              output$y <- 0
              output$precision <- 0
              output$comment <- NA
              
              #creation of dataframe in which the georeferencing output will be stored
              tweets.georeferenced <- data.frame(matrix(nrow = 0, 
                                                        ncol = NCOL(output)))
          #    require("rgdal")
              #loop that adds lat lon information and replicates tweets according to geolocation word detection
              for (i in 1:NROW(geolocations)){
                tweets.test <- output[ grepl(geolocations$name[i], 
                                                             output$Text), ]
                if(NROW(tweets.test) == 0){
                }else{
                  tweets.test$x <- geolocations$Long[i]
                  tweets.test$y <- geolocations$Lat[i]
                  tweets.test$detected <- geolocations$name[i]
                  tweets.test$precision <- geolocations$Precision[i]
                  tweets.test$comment <- geolocations$duplicates[i]
                  tweets.georeferenced <- rbind(tweets.georeferenced, tweets.test)
                }
              }
              
              
              #export geo-referenced prerandomized dataset
              write.csv(tweets.georeferenced, 
                        "./georeferenced.prerandomized.mar.24.csv")
             # tweets.georeferenced <-read.csv("./georeferenced.prerandomized.mar.24.csv")
             
 #start of randomization part
              final.results.prerandomized <- tweets.georeferenced
              #removal of non needed dataframes
              
              rm(tweets.test, tweets.georeferenced)
            
            #keeping only data with coordinates
            final.results.prerandomized <- subset(final.results.prerandomized, y > 0)
        #    require("rgdal")
         #   install.packages("rgdal")

           # .rs.restartR()
            #import of area.map shapefile
            area.map <- readOGR(dsn = "./kallikratis.shp", 
                                layer = "kallikratis")
            #creation of unique id to area.map
            area.map@data$id <- as.numeric(row.names(area.map@data)) + 1
            
            

            
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
            random.points$munname <- NA
            random.points$counter <- 0
            random.points$uniqueid <- NA
            random.points$distance.from.epic <- NA
            spdf.random.points <- SpatialPointsDataFrame(coords = random.points[ ,c("x", "y")], 
                                                         data = random.points, 
                                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
            random.points$munname <- over(spdf.random.points, area.map[ , "NAME"])

            random.points$municipality <- paste(as.character(unlist(random.points$munname)))
            random.points$counter <- with(random.points, 
                                          ave(seq_along(municipality), 
                                              municipality, 
                                              FUN = seq_along))
            random.points$uniqueid <- paste(random.points$municipality, random.points$counter, sep = "")
            rm(spdf.random.points)

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
            write.csv(final.results.prerandomized, file = "./tweets.randomized.final.mar.26.csv")

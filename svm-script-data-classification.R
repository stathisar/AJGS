require("RTextTools")
require("readODS")
rm(list = ls())

#dataset load
alltweets <- read.csv(file = "/media/stathis/Maxtor/article/all-tweets.csv")

#training dataset load
trainingdata <- read.csv(file = "/media/stathis/Maxtor/article/data/training_dataset.csv")
View(trainingdata[1:10, ])

#dtMatrix
dtMatrix <- create_matrix(trainingdata["Text"])

#configure training data fire event identification
container.fire.event.ident <- create_container(dtMatrix, trainingdata$FIRE.EVENT.IDENTIFICATION, trainSize = 1:NROW(trainingdata), virgin = FALSE)
#configure training data other.emergency.event
container.other.emerg.event <- create_container(dtMatrix, trainingdata$OTHER.EMERGENCY.EVENT, trainSize = 1:NROW(trainingdata), virgin = FALSE)
#configure training data fire event of kalamos identification
container.consequences <- create_container(dtMatrix, trainingdata$CONSEQUENCES.OF.FIRE..KALAMOS.EVENT., trainSize = 1:NROW(trainingdata), virgin = FALSE)
container.event.tracking <- create_container(dtMatrix, trainingdata$FIRE.EVENT.TRACKING.OF.KALAMOS, trainSize = 1:NROW(trainingdata), virgin = FALSE)
container.emerg.management <- create_container(dtMatrix, trainingdata$FIRE.EMERGENCY.MANAGEMENT.OF.KALAMOS.EVENT, trainSize = 1:NROW(trainingdata), virgin = FALSE)

#training of SVM Model
model1 <- train_model(container.fire.event.ident, "SVM", kernel="linear", cost=1)
model2 <- train_model(container.other.emerg.event, "SVM", kernel="linear", cost=1)
model3 <- train_model(container.consequences, "SVM", kernel="linear", cost=1)
model4 <- train_model(container.event.tracking, "SVM", kernel="linear", cost=1)
model5 <- train_model(container.emerg.management, "SVM", kernel="linear", cost=1)

#new data
predictionData <- alltweets

#create a prediction document term matrix
#change Acronym to acronym in line42
trace("create_matrix", edit=T)

predMatrix <- create_matrix(predictionData, originalMatrix = dtMatrix)

#create the corresponding container
predSize = length(predictionData$Text)
predSize
NROW(predMatrix)
predictionContainer <- create_container(predMatrix, labels = rep(0, NROW(predictionData)), testSize = 1:NROW(predictionData), virgin = FALSE)

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
results1$id <- row.names(results1)
results2$id <- row.names(results2)
results3$id <- row.names(results3)
results4$id <- row.names(results4)
results5$id <- row.names(results5)
predictionData$id <- row.names(predictionData)
predicted1 <- merge(results1, predictionData, by = "id")
predicted2 <- merge(results2, predictionData, by = "id")
predicted3 <- merge(results3, predictionData, by = "id")
predicted4 <- merge(results4, predictionData, by = "id")
predicted5 <- merge(results5, predictionData, by = "id")

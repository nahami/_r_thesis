#### Thesis first analysis ####
# March 2017, by Nadir Ahami
#  

## Clean-up environment
rm(list=ls())

## Install necessary packages
library(sp)        # spatial tools lat lon data
library(data.table)# dataframe extension
library(geosphere) # distance calcultion based on lat lon (haversine,great circle method)
# library(xgboost)   # ensemble method: extreme gradient boosted trees
# library(lattice)   # dependable of caret
# library(ggplot2)   # dependable caret
# library(caret)     # caret predictive modelling tools
library(Matrix)    # matrices

## Set working directory
dir <- "/usr/people/ahami/_msc_thesis/_r_thesis"
# dir <- "C:/Users/Nadir/hubiC/_Studie/New folder"
setwd(dir)

#### Import files and data ####






## Location data
locationCoordinates <- fread(input = "data/Adam/WS_wow.txt", 
                             sep = ",", header = TRUE,
                             stringsAsFactors = FALSE,
                             na.strings = "-")

## Radiation data import
radiation_data <- as.data.frame(readRDS("KEDexpSICCS(1).rda"))

rownames(radiation_data) <- c('1000000779',
                       '1000000112',
                       '932806001',
                       '926656002',
                       '942486001',
                       '924276001',
                       '933736001',
                       '916696001')

colnames(radiation_data) <- strptime(colnames(radiation_data), format = "X%Y.%m.%d")

radiation_data <- as.data.frame(t(radiation_data))
radiation_data <- radiation_data[3700:4351,]

raddata <- cbind(datum =as.Date(row.names(radiation_data)), radiation_data)
rownames(raddata) <- 1:length(raddata[,1])

rm(radiation_data)

## sky view factor import
svfwow <- as.data.frame(read.csv(file = 'SVF_WOW.txt'))
svfwow$id <- as.numeric(gsub("\\D", "", svfwow$id))
svfwow <- cbind(id = svfwow$id, svf = svfwow$sampled)

## Create distance matrix
distanceMatrix <- as.data.frame(distm (locationCoordinates[c("lon", "lat")], 
                                       locationCoordinates[c("lon", "lat")], 
                                       fun = distHaversine))
distanceMatrix <- cbind(locationCoordinates$id, distanceMatrix)
names(distanceMatrix) <- c("id", locationCoordinates$id)


#### Import location data ####

## List files in directory
files <- list.files()
files <- files[grep(pattern = ".csv", x = files)]

## Get location IDs from each csv
locationIDs <- as.numeric(gsub("\\D", "", files))

for (i in 1:length(files))
{
  ## Import file
  data <- fread(input = files[i], sep = ";", header = TRUE, stringsAsFactors = FALSE, na.strings = "-")
  data <- as.data.frame(data)
  data <- cbind(as.character(locationIDs[i]), data)
  colnames(data) <-  c('ID',
                       'datumtijd', 
                       'temperatuur (C)', 
                       'windsnelheid (m/s)', 
                       'windrichting (graden)', 
                       'relatieve lochtvuchtigheid (%)',
                       'luchtdruk (hPa)')
  
  ## Convert datum datetype to datetime
  data$datumtijd <- strptime(data$datumtijd, format = "%Y-%m-%dT%H:%M:%S")
  
  # seperate columns for date and time
  data <- cbind(data[,1:2], datum = as.Date(data$datumtijd), tijd = as.ITime(data$datumtijd), data[,3:7])
  hod <- as.integer(substr(data$tijd, 1, 2))
  
  data <- cbind(data, hod = hod)
  
  # add radiation data
  data <- merge(x = data, y = cbind(datum = raddata[,1], 'radiation (w/m2)' = raddata[,i+1]), by.x = 'datum', by.y = 'datum')
  
  # add sky view factor
  data <-  merge(x = data, y = svfwow, by.x = 'ID', by.y = 'id')
  
  
  ## Change dataframe name
  assign(paste("data", locationIDs[i], sep = '_'), data)
  
  ## Clean-up environment
  rm(data)
  
  ## Print current state
  if (i == 1){
    print(sprintf("Imported %s file", i))
  }
  else{
    print(sprintf("Imported %s files", i))  
  }
  
}

#### Create input data for model ####

## Concatenate all stations rowwise
data_in <- rbind.data.frame(data_1000000112,
             data_1000000779,
             data_916696001,
             data_924276001,
             data_926656002,
             data_932806001,
             data_933736001,
             data_942486001)

save(data_in, locationCoordinates, distanceMatrix, file = 'data_in.Rda')

data_in <- data_in[!is.na(data_in$`temperatuur (C)`),]
## Clean up environment
rm(data_1000000112,
   data_1000000779,
   data_916696001,
   data_924276001,
   data_926656002,
   data_932806001,
   data_933736001,
   data_942486001,
   raddata,
   radiation_data)

#### Add other variables  
## Add hour of day
## Add dayparts
## Add Cloud cover
## Add Land cover
## Add NDVI


#### Create input data for model ####
## Create a generic dataframe using the temperate of each location as the target
## Loop
featureInput <- NULL
for (i in 1:length(distanceMatrix[,1]))
for (i in 1:length(unique(data_in$ID)))
{
  
  ## Get nearest locations per location i wrt pivot
  stations <- as.character(unique(data_in$ID))
  # stations =  colnames(distanceMatrix)[2:9]   # List of stations
  pivotstation = stations[i]                  # Select pivot station
  
  ## List of distance ordered id stations wrt pivotstation
  
  rownames(distanceMatrix) <- colnames(distanceMatrix)[2:9]
  ids_ascending_all <- names(distanceMatrix)[order(distanceMatrix[pivotstation,])[2:8]]
  ids_ascending <- intersect(ids_ascending_all, stations)
  
  ## Get TMP SVF and SR at pivot
  temp <- subset(data_in, data_in$ID == pivotstation, select = c('datumtijd','datum','tijd','temperatuur (C)', 'radiation (w/m2)', 'svf', 'hod'))

  
  # for (j in 1:length(ids_ascending))
  for (j in 1:2)
  {
    
    ## get variables from other locations
    #data
    j_th_station <- subset(data_in, data_in$ID == ids_ascending[j],select = c('datumtijd','temperatuur (C)'))
    ## get distance
    dist_j <- distanceMatrix[pivotstation,ids_ascending[j]]

    
    ## add distance to jth observation
    j_th_station <- cbind(j_th_station, 'dist_j' = dist_j)

    
    ## Rename columns
    colnames(j_th_station) <- c(sprintf('N_%i T',j),   # Temperature
                                sprintf('N_%i D',j))   # Distance to pivot i
    
    temp <-  merge(x = temp, y = j_th_station, by.x = 'datumtijd', by.y = 'datumtijd')
    
    # temp <- cbind(temp, j_th_station)
    
    
  }
  
  featureInput <- rbind(featureInput, temp)
  
  ## Clean-up loop environment
  # rm(tempInput, distances, dfNameCore, dfNameJoins, distanceColName, genericLocationName)
  
  ## Print current state
  print(sprintf("Prepared data for %i location(s)", i))
  
}




#### Remove with target temperatue column equal to NA
featureInput <- featureInput[!is.na(featureInput$`temperatuur (C)`),]


#### Split into train and test set #####

## Random row IDs
set.seed(123)
randomizedRowIDs <- sample(1:length(featureInput[,1]), length(featureInput[,1]), replace=FALSE)

## Create train and test row IDs
cutOff <- floor(0.2*length(featureInput[,1]))
trainRowIDs <- randomizedRowIDs[1:cutOff]
testRowIDs <- randomizedRowIDs[(cutOff+1):length(featureInput[,1])]

## Column names targets and predictors
target <- names(featureInput)[3]
predictors <- setdiff(names(featureInput), names(featureInput)[1:3])

## Split total set into train and test sets
trainX <- featureInput[trainRowIDs, predictors]
trainY <- featureInput[trainRowIDs, target]
testX <- featureInput[testRowIDs, predictors]
testY <- featureInput[testRowIDs, target]

## Convert dataframes to matrices
matrixTrainX <- as.matrix(trainX)
matrixTrainY <- as.matrix(trainY)
matrixTestX <- as.matrix(testX)
matrixTestY <- as.matrix(testY)

##### Train model #####

## Train model boosted tree
bst <- xgboost(objective = "reg:linear",
               data = matrixTrainX, 
               label = matrixTrainY, 
               nround = 100,
               eta = 0.1,
               max_depth = 15,
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 123
)

## 

linm <- train(matrixTrainY ~ matrixTrainX
              ,method = "lm"
)




## Generate predictions
predictions_bst <- predict(bst, matrixTestX)

## Check accurary by comparing measured temperatures with predictions
RMSE <- sqrt(mean((predictions_bst - matrixTestY)^2))
RMSE

head(predictions_bst,10)
head(matrixTestY,10)


importance <- xgb.importance(feature_names = names(trainX), model = bst)
head(importance,100)

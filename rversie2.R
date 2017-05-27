#### Thesis first analysis ####
# May 2017, by Nadir Ahami
#  


## Clean-up environment
rm(list=ls())

## Install necessary packages
library(sp)        # spatial tools lat lon data
library(data.table)# dataframe extension
library(geosphere) # distance calcultion based on lat lon (haversine,great circle method)
library(xgboost)   # ensemble method: extreme gradient boosted trees
library(lattice)   # dependable of caret
library(ggplot2)   # dependable caret
library(caret)     # caret predictive modelling tools
library(Matrix)    # matrices
library(rpart)     # Recursive partioning and decision trees
library(randomForest) # Random forest classification and regression library

#for plotting
library(grid)
library(gridExtra)
library(lattice)


## Set working directory
dir <- "C:/Users/Nadir/hubiC/_Studie/MscThesis/_r_thesis"
setwd(dir)

#### Import files and data ####

load(file = 'data_in.Rda')

data_in <- data_in[!is.na(data_in$`temperatuur (C)`),]


#### Add other variables  
## Add dayparts
## Add Cloud cover
## Add Land cover
## Add NDVI


#### Create input data for model ####
## Create a generic dataframe using the temperate of each location as the target
## Loop
featureInput <- NULL
n=2 #number of neighbours



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
    for (j in 1:n)
    {
      
      ## get variables from other locations
      #data
      j_th_station <- subset(data_in, data_in$ID == ids_ascending[j],select = c('datum','tijd','temperatuur (C)'))
      ## get distance
      dist_j <- distanceMatrix[pivotstation,ids_ascending[j]]
      
      
      ## add distance to jth observation
      j_th_station <- cbind(j_th_station, 'dist_j' = dist_j)
      
      
      ## Rename columns
      colnames(j_th_station) <- c('datum',
                                  'tijd',
                                  sprintf('N_%i T',j),   # Temperature
                                  sprintf('N_%i D',j))   # Distance to pivot i
      
      temp <-  merge(x = temp, y = j_th_station, by = c('datum','tijd'))
      
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

a <- colnames(featureInput)
a[5] <- 'rad'
colnames(featureInput) <- a



#### Split into train and test set #####

## Random row IDs
set.seed(123)
randomizedRowIDs <- sample(1:length(featureInput[,1]), length(featureInput[,1]), replace=FALSE)

## Create train and test row IDs
cutOff <- floor(0.2*length(featureInput[,1]))
trainRowIDs <- randomizedRowIDs[1:cutOff]
testRowIDs <- randomizedRowIDs[(cutOff+1):length(featureInput[,1])]

## Column names targets and predictors
target <- names(featureInput)[4]
predictors <- setdiff(names(featureInput), names(featureInput)[1:4])

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

## linear regression

matrixTrainxy <- as.data.frame(cbind('y' = matrixTrainY, matrixTrainX))


lmFit <- train(V1 ~ .,
               data = matrixTrainxy,
               method = 'lm')

lmFit_svf <- train(V1 ~ . - svf,
                   data = matrixTrainxy,
                   method = 'lm')

lmFit_hod <- train(V1 ~ . - hod,
                   data = matrixTrainxy,
                   method = 'lm')

lmFit_rad <- train(V1 ~ . - rad,
                   data = matrixTrainxy,
                   method = 'lm')

summary(lmFit)


## decision tree
dectree <-  rpart(V1 ~ .,
                  data = matrixTrainxy,
                  method = 'anova')

dectree_svf <-  rpart(V1 ~ . - svf,
                  data = matrixTrainxy,
                  method = 'anova')

dectree_hod <-  rpart(V1 ~ . - hod,
                      data = matrixTrainxy,
                      method = 'anova')

dectree_rad <-  rpart(V1 ~ . - rad,
                      data = matrixTrainxy,
                      method = 'anova')

## Random forest
# rf_model <- train(V1 ~ .,
#                   data = matrixTrainxy,
#                   method="rf")



## Generate predictions
# gr. boost
predictions_bst <- predict(bst, matrixTestX)
# lin model
predictions_lin <- predict(lmFit, matrixTestX)
predictions_lin_svf <- predict(lmFit_svf, matrixTestX)
predictions_lin_hod <- predict(lmFit_hod, matrixTestX)
predictions_lin_rad <- predict(lmFit_rad, matrixTestX)
# dectree
predictions_dectr <- predict(dectree, as.data.frame(matrixTestX),type ='vector')
predictions_dectr_svf <- predict(dectree_svf, as.data.frame(matrixTestX),type ='vector')
predictions_dectr_hod <- predict(dectree_hod, as.data.frame(matrixTestX),type ='vector')
predictions_dectr_rad <- predict(dectree_rad, as.data.frame(matrixTestX),type ='vector')
# randomforest
# predictions_rf <- 

## Check accurary by comparing measured temperatures with predictions
RMSE_bst <- sqrt(mean((predictions_bst - matrixTestY)^2))
RMSE_bst

rmse_linr <- sqrt(mean((predictions_lin - matrixTestY)^2))
rmse_linr

rmse_linr_svf <- sqrt(mean((predictions_lin_svf - matrixTestY)^2))
rmse_linr_svf
rmse_linr_hod <- sqrt(mean((predictions_lin_hod - matrixTestY)^2))
rmse_linr_hod
rmse_linr_rad <- sqrt(mean((predictions_lin_rad - matrixTestY)^2))
rmse_linr_rad

rmse_dectr <- sqrt(mean((predictions_dectr - matrixTestY)^2))
rmse_dectr

rmse_dectr_svf <- sqrt(mean((predictions_dectr_svf - matrixTestY)^2))
rmse_dectr_svf
rmse_dectr_hod <- sqrt(mean((predictions_dectr_hod - matrixTestY)^2))
rmse_dectr_hod
rmse_dectr_rad <- sqrt(mean((predictions_dectr_rad - matrixTestY)^2))
rmse_dectr_rad

head(predictions_bst,10)
head(predictions_lin,10)
head(matrixTestY,10)


importance <- xgb.importance(feature_names = names(trainX), model = bst)
head(importance,100)



####### plotting

g <- ggplot(data = data.frame(boos = predictions_bst, 
                              y = matrixTestY,
                              lin = predictions_lin,
                              dec = predictions_dectr,
                              lin_svf = predictions_lin_svf,
                              lin_hod = predictions_lin_hod,
                              lin_rad = predictions_lin_rad,
                              dec_svf = predictions_dectr_svf,
                              dec_hod = predictions_dectr_hod,
                              dec_rad = predictions_dectr_rad))
g1 <- g + geom_point(aes(x = boos, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("gradient boost reg",paste0("rmse = ", round(RMSE_bst,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g2 <- g + geom_point(aes(x = lin, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin model",paste0("rmse = ", round(rmse_linr,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g3 <- g + geom_point(aes(x = dec, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec tree",paste0("rmse = ", round(rmse_dectr,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g4 <- g + geom_point(aes(x = lin_svf, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin - svf",paste0("rmse = ", round(rmse_linr_svf,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g5 <- g + geom_point(aes(x = lin_hod, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin - hod",paste0("rmse = ", round(rmse_linr_hod,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g6 <- g + geom_point(aes(x = lin_rad, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin - rad",paste0("rmse = ", round(rmse_linr_rad,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g7 <- g + geom_point(aes(x = dec_svf, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec - svf",paste0("rmse = ", round(rmse_dectr_svf,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g8 <- g + geom_point(aes(x = dec_hod, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec - hod",paste0("rmse = ", round(rmse_dectr_hod,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g9 <- g + geom_point(aes(x = dec_rad, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec - rad",paste0("rmse = ", round(rmse_dectr_rad,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)

g10 <- arrangeGrob(g1,g2,g3,g4,g5,g6,g7,g8,g9, ncol = 3)


plot(g10)

ggsave(plot = g10, filename = 'plot_first_res', dpi = 600,device = 'png' )

########################################################################
########################### NOTES ON RESULTS ###########################
########################################################################
# 1. We have 6 observations per hour, randomly selecting train/test is 
#    not wise. Because the testset can be too much in the proximity of 
#    the training data.
# 2. variables as landcover, cloud cover, data nearest KNMI (pressure,).
# 3. Preferable to use more observation locations.
# 4. Maybe its an idea to change the label to UHII <- [city observation 
#    - rural observation].
# 5. Expected behaviour is nonlinear, so its expected that trees will 
#    probably perform better.
# 6. Later on (irrelevant to this script) try SVM (grouping of some 
#    parameters) to identify hot and cool zones. 
# 
# 
# 


########################################################################
###########################   Still needed   ###########################
########################################################################
# 1 Dataset interpolation temperature, wind direction over the Netherlends


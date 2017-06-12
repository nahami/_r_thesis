#------------------
# Thesis: Statistical modeling of the Urban Heat Island
# May 2017, by Nadir Ahami
# 
# 1: subsetten nachten aanapssen
# 2: dependent variable aanpassen
# 
# 
# 

#### Set up environment ####
rm(list=ls())
graphics.off()

# Set working directory
# dir <- "C:/Users/Nadir/hubiC/_Studie/MscThesis/_r_thesis"
dir <- "/usr/people/ahami/_msc_thesis/_r_thesis"
setwd(dir)

# packages
library(xgboost)   # ensemble method: extreme gradient boosted trees
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(grid)
library(gridExtra)
library(gsw)
library(oce)


#### Import data ####
load("obs.RData")
ancdat <- read.csv('data/Rdam/rdam_all', header = TRUE)
# Choose KNMI AWS 1.=airport rdam, 2.= groene hart 
knmi_aws <- KNMI_1[,c(1,2,3,5)]



m_lat <- 51.93   
m_lon <- 4.509 
t0 <- as.POSIXct(knmi_aws[1,1], tz="UTC")
tn <- as.POSIXct(knmi_aws[nrow(knmi_aws),1], tz="UTC")
t <- seq(from=t0, to=tn, by="hour")
sunel = as.data.frame(sunAngle(t, longitude = m_lon, latitude = m_lat))[,1:3]
colnames(sunel) <- c('datetime', 's_azimuth', 's_altitude')
sunel$datetime <- as.character(sunel$datetime)




#### Merge df ####
feat_inp <- merge(alld, knmi_aws, by = 'datetime') #KNMI AWS
feat_inp$tempdif <- feat_inp$temp - feat_inp$temp_aws
feat_inp <- feat_inp[,c(1,2,3,7,4,5,6)]
feat_inp <- merge(feat_inp, ancdat[,c(2,6,7)], by = 'id', all = TRUE) #SVF, LC
feat_inp <- feat_inp[!(feat_inp$id=="Bernisse" | feat_inp$id == 'Lansingerland'),] #Removing stations outside area of interest 
feat_inp <- merge(feat_inp, sunel, by = 'datetime')




feat_inp <- na.omit(feat_inp)

# feat_inp$datetime <- as.POSIXct(feat_inp$datetime, tz = "UTC")
# feat_inp <- subset(feat_inp, format(datetime,'%H') %in% c('20','21','22','23','00','01','02','03','04'))


# subset nights

#### Split into train and test set #####

# Random row IDs
set.seed(123)
randomizedRowIDs <- sample(1:length(feat_inp[,1]), length(feat_inp[,1]), replace=FALSE)
# randomizedRowIDs <- as.integer(row.names(feat_inp))


# Create train and test row IDs
test_fr <- 0.2
cut_off <- floor(test_fr*length(feat_inp[,1]))
trainRowIDs <- randomizedRowIDs[1:cut_off]
testRowIDs <- randomizedRowIDs[(cut_off+1):length(feat_inp[,1])]

# Column names targets and predictors

tn = 3 # 3 = temp, 4 = tempdif
target <- names(feat_inp)[tn]
predictors <- setdiff(names(feat_inp), names(feat_inp)[1:4])

# Split total set into train and test sets
trainX <- feat_inp[trainRowIDs, predictors]
trainY <- feat_inp[trainRowIDs, target]
testX <- feat_inp[testRowIDs, predictors]
testY <- feat_inp[testRowIDs, target]

# Convert dataframes to matrices
matrixTrainX <- as.matrix(trainX)
matrixTrainY <- as.matrix(trainY)
matrixTestX <- as.matrix(testX)
matrixTestY <- as.matrix(testY)



##### Train model #####

# Train model boosted tree
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

# linear regression
matrixTrainxy <- as.data.frame(cbind('y' = matrixTrainY, matrixTrainX))

lmFit <- train(V1 ~ .,
               data = matrixTrainxy,
               method = 'lm')

lmFit_svf <- train(V1 ~ . - svf,
                   data = matrixTrainxy,
                   method = 'lm')

lmFit_lc_b <- train(V1 ~ . - lc_b,
                   data = matrixTrainxy,
                   method = 'lm')

lmFit_ws <- train(V1 ~ . - ws,
                   data = matrixTrainxy,
                   method = 'lm')

# summary(lmFit)


# decision tree
dectree <-  randomForest(V1 ~ .,
                  data = matrixTrainxy
                  )

dectree_svf <-  randomForest(V1 ~ . - svf,
                      data = matrixTrainxy
                      )

dectree_lc_b <-  randomForest(V1 ~ . - lc_b,
                      data = matrixTrainxy
                      )

dectree_ws <-  randomForest(V1 ~ . - ws,
                      data = matrixTrainxy
                      )

# Random forest
# rf_model <- train(V1 ~ .,
#                   data = matrixTrainxy,
#                   method="rf")


# Generate predictions
# gr. boost
predictions_bst <- predict(bst, matrixTestX)
# lin model
predictions_lin <- predict(lmFit, matrixTestX)
predictions_lin_svf <- predict(lmFit_svf, matrixTestX)
predictions_lin_lc_b <- predict(lmFit_lc_b, matrixTestX)
predictions_lin_ws <- predict(lmFit_ws, matrixTestX)
# dectree
predictions_dectr <- predict(dectree, as.data.frame(matrixTestX))
predictions_dectr_svf <- predict(dectree_svf, as.data.frame(matrixTestX))
predictions_dectr_lc_b <- predict(dectree_lc_b, as.data.frame(matrixTestX))
predictions_dectr_ws <- predict(dectree_ws, as.data.frame(matrixTestX))


## Check accurary by comparing measured temperatures with predictions
RMSE_bst <- sqrt(mean((predictions_bst - matrixTestY)^2))
RMSE_bst

rmse_linr <- sqrt(mean((predictions_lin - matrixTestY)^2))
rmse_linr

rmse_linr_svf <- sqrt(mean((predictions_lin_svf - matrixTestY)^2))
rmse_linr_svf
rmse_linr_lc_b <- sqrt(mean((predictions_lin_lc_b - matrixTestY)^2))
rmse_linr_lc_b
rmse_linr_ws <- sqrt(mean((predictions_lin_ws - matrixTestY)^2))
rmse_linr_ws

rmse_dectr <- sqrt(mean((predictions_dectr - matrixTestY)^2))
rmse_dectr

rmse_dectr_svf <- sqrt(mean((predictions_dectr_svf - matrixTestY)^2))
rmse_dectr_svf
rmse_dectr_lc_b <- sqrt(mean((predictions_dectr_lc_b - matrixTestY)^2))
rmse_dectr_lc_b
rmse_dectr_ws <- sqrt(mean((predictions_dectr_ws - matrixTestY)^2))
rmse_dectr_ws


####### plotting
ds = 0.05
g <- ggplot(data = data.frame(boos = predictions_bst, 
                              y = matrixTestY,
                              lin = predictions_lin,
                              dec = predictions_dectr,
                              lin_svf = predictions_lin_svf,
                              lin_lc_b = predictions_lin_lc_b,
                              lin_ws = predictions_lin_ws,
                              dec_svf = predictions_dectr_svf,
                              dec_lc_b = predictions_dectr_lc_b,
                              dec_ws = predictions_dectr_ws))
g1 <- g + geom_point(aes(x = boos, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("gradient boost reg",paste0("rmse = ", round(RMSE_bst,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g2 <- g + geom_point(aes(x = lin, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("lin model",paste0("rmse = ", round(rmse_linr,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)  
g3 <- g + geom_point(aes(x = dec, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("dec tree",paste0("rmse = ", round(rmse_dectr,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g4 <- g + geom_point(aes(x = lin_svf, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("lin - svf",paste0("rmse = ", round(rmse_linr_svf,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g5 <- g + geom_point(aes(x = lin_lc_b, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("lin - lc",paste0("rmse = ", round(rmse_linr_lc_b,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g6 <- g + geom_point(aes(x = lin_ws, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("lin - ws",paste0("rmse = ", round(rmse_linr_ws,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g7 <- g + geom_point(aes(x = dec_svf, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("dec - svf",paste0("rmse = ", round(rmse_dectr_svf,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g8 <- g + geom_point(aes(x = dec_lc_b, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("dec - lc",paste0("rmse = ", round(rmse_dectr_lc_b,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g9 <- g + geom_point(aes(x = dec_ws, y = y), size = ds) + labs(x='fitted', y = 'actual') + ggtitle("dec - ws",paste0("rmse = ", round(rmse_dectr_ws,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 

g10 <- arrangeGrob(g1,g2,g3,g4,g5,g6,g7,g8,g9, ncol = 3)


plot(g10) 

ggsave(plot = g10, filename = 'temp_aws1_nights', dpi = 600,device = 'png')




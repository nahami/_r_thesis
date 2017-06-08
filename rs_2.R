#------------------
# Thesis: Statistical modeling of the Urban Heat Island
# May 2017, by Nadir Ahami
# 

#### Set up environment ####
rm(list=ls())

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


#### Import data ####
load("obs.RData")
ancdat <- read.csv('data/Rdam/rdam_all', header = TRUE)
knmi_aws <- KNMI_AWS[,c(2,3,4,6)]



#### Merge df ####
feat_inp <- merge(alld, knmi_aws, by = 'datetime',  all = TRUE)
feat_inp <- merge(feat_inp, ancdat[,c(2,6,7)], by = 'id', all = TRUE)
feat_inp <- feat_inp[!(feat_inp$id=="Bernisse" | feat_inp$id == 'Lansingerland'),]

cols = c(3, 4, 5, 6, 7, 8);    
feat_inp[,cols] = apply(feat_inp[,cols], 2, function(x) as.numeric(as.character(x)))


feat_inp <- na.omit(feat_inp)



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
target <- names(feat_inp)[3]
predictors <- setdiff(names(feat_inp), names(feat_inp)[1:3])

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
dectree <-  rpart(V1 ~ .,
                  data = matrixTrainxy,
                  method = 'anova')

dectree_svf <-  rpart(V1 ~ . - svf,
                      data = matrixTrainxy,
                      method = 'anova')

dectree_lc_b <-  rpart(V1 ~ . - lc_b,
                      data = matrixTrainxy,
                      method = 'anova')

dectree_ws <-  rpart(V1 ~ . - ws,
                      data = matrixTrainxy,
                      method = 'anova')

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
predictions_dectr <- predict(dectree, as.data.frame(matrixTestX),type ='vector')
predictions_dectr_svf <- predict(dectree_svf, as.data.frame(matrixTestX),type ='vector')
predictions_dectr_lc_b <- predict(dectree_lc_b, as.data.frame(matrixTestX),type ='vector')
predictions_dectr_ws <- predict(dectree_ws, as.data.frame(matrixTestX),type ='vector')


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
g1 <- g + geom_point(aes(x = boos, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("gradient boost reg",paste0("rmse = ", round(RMSE_bst,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g2 <- g + geom_point(aes(x = lin, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin model",paste0("rmse = ", round(rmse_linr,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)  
g3 <- g + geom_point(aes(x = dec, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec tree",paste0("rmse = ", round(rmse_dectr,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g4 <- g + geom_point(aes(x = lin_svf, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin - svf",paste0("rmse = ", round(rmse_linr_svf,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g5 <- g + geom_point(aes(x = lin_lc_b, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin - lc",paste0("rmse = ", round(rmse_linr_lc_b,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g6 <- g + geom_point(aes(x = lin_ws, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("lin - ws",paste0("rmse = ", round(rmse_linr_ws,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g7 <- g + geom_point(aes(x = dec_svf, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec - svf",paste0("rmse = ", round(rmse_dectr_svf,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1)
g8 <- g + geom_point(aes(x = dec_lc_b, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec - lc",paste0("rmse = ", round(rmse_dectr_lc_b,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 
g9 <- g + geom_point(aes(x = dec_ws, y = y)) + labs(x='fitted', y = 'actual') + ggtitle("dec - ws",paste0("rmse = ", round(rmse_dectr_ws,2))) + geom_abline(slope = 1, intercept = 0, col = 2, lwd =1) 

g10 <- arrangeGrob(g1,g2,g3,g4,g5,g6,g7,g8,g9, ncol = 3)


plot(g10) 

ggsave(plot = g10, filename = 'plot_first_res3', dpi = 600,device = 'png' )




# goal: T_obs (- T_AWS) SVF LC  WS T_AWS



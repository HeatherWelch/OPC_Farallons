###############################################################################################
# Build anchovy SDMs for OPC project, based on turtle SDM var names
# Input data updated as of May 2022, includes 2021 CPS survey data, and Thayer indices current 
# through 2021
###############################################################################################

library(mgcv)
library(dismo)
library(gbm)
library(pROC) 
library(caret)

# Load training data with env vars extracted: using ROMS reanalysis + ROMS NRT + CMEMS L4 chl
allROMSchl <- readRDS("./msa/data/allDataEnvExtracted_goodChl_updated2022.rds")
# Only using CPS and Predator cruise data
allROMSchl <- subset(allROMSchl, survey == "cps" | survey == "predator")

# Set varNames to match turtle SDM where necessary. Note I'm not using curl, su, sv, sustr, or svstr
# (ild, ssh, sst, sst_sd, ssh_sd, EKE, lunar, bv)
allROMSchl$bv <- allROMSchl$BV
allROMSchl$EKE <- allROMSchl$logEKE

# Split into test/train 50/50
set.seed(123)
index <- sample(1:nrow(allROMSchl), round(0.5 * nrow(allROMSchl))) 
train <- allROMSchl[index,]
test <- allROMSchl[-index,]

##################################################################################################################
# Build a GAM, test values of 3, 4, or 5 for k
gam3 <- gam(anchPA ~ s(bv, k = 3) + s(ild, k = 3) + s(ssh, k = 3) + s(sst, k = 3) + s(EKE, k = 3) + 
              s(sst_sd, k = 3) + s(ssh_sd, k = 3) + s(lunar, k = 3) +  s(chl4th, k = 3) + s(anchSSB, k = 3) + 
              s(distLand, k = 3), data = train, family = 'binomial', method = "REML", select = TRUE)
gam4 <- gam(anchPA ~ s(bv, k = 4) + s(ild, k = 4) + s(ssh, k = 4) + s(sst, k = 4) + s(EKE, k = 4) + 
              s(sst_sd, k = 4) + s(ssh_sd, k = 4) + s(lunar, k = 4) +  s(chl4th, k = 4) + s(anchSSB, k = 4) + 
              s(distLand, k = 4), data = train, family = 'binomial', method = "REML", select = TRUE)
gam5 <- gam(anchPA ~ s(bv, k = 5) + s(ild, k = 5) + s(ssh, k = 5) + s(sst, k = 5) + s(EKE, k = 5) + 
              s(sst_sd, k = 5) + s(ssh_sd, k = 5) + s(lunar, k = 5) +  s(chl4th, k = 5) + s(anchSSB, k = 5) + 
              s(distLand, k = 5), data = train, family = 'binomial', method = "REML", select = TRUE)
# AUCs against test data
(roc(test$anchPA, predict(gam3, test, type = "response"), quiet = TRUE))$auc # 0.828
(roc(test$anchPA, predict(gam4, test, type = "response"), quiet = TRUE))$auc # 0.829
(roc(test$anchPA, predict(gam5, test, type = "response"), quiet = TRUE))$auc # 0.833
# Pretty similar, let's go k = 5
bestGAM <- gam5

##################################################################################################################
# Build a BRT. Optimize using caret
# Set optimization options
fitControl <- trainControl(method = "cv", number = 5) # higher "number" = slower
gbmGrid <- expand.grid(interaction.depth = c(3,4,5), n.trees = (7:11)*200, # was (7:10) * 200
                       shrinkage = seq(from = 0.0025, to = 0.05, by = 0.005), n.minobsinnode = 10)

# Subset to vars needed, is easier
trainBRT <- train[c("anchPA", "bv", "ild", "ssh", "sst", "sst_sd", "ssh_sd", "EKE", "lunar", "chl4th", 
                    "distLand", "anchSSB")]
# Remove NAs, needed for caret optimization
trainBRT <- subset(trainBRT, !is.nan(EKE))

# Find the best configuration
set.seed(123)
gbmFit1 <- caret::train(as.factor(anchPA) ~ ., data = trainBRT, method = "gbm", 
                        trControl = fitControl, verbose = FALSE, tuneGrid = gbmGrid) # Takes a few minutes
plot(gbmFit1) # Make sure optimal parameters are not at edges of grid values
# Save the best values of each parameter
lr.best = gbmFit1$bestTune$shrinkage
tc.best = gbmFit1$bestTune$interaction.depth
n.trees.best = gbmFit1$bestTune$n.trees
# Build the final best BRT
bestBRT <- gbm.fixed(data = trainBRT, gbm.x = 2:(ncol(trainBRT)), gbm.y = 1, family = "bernoulli",
                     tree.complexity = tc.best, learning.rate = lr.best, bag.fraction = 0.6, 
                     n.trees = n.trees.best) 
summary(bestBRT)
# AUC (0.86)
(roc(test$anchPA, predict(bestBRT, test, n.trees = bestBRT$gbm.call$n.trees, type = "response"), quiet = TRUE))$auc

# Save models
save(bestGAM, file = "./opc/models/anchovyGAM.rda")
save(bestBRT, file = "./opc/models/anchovyBRT.rda")  

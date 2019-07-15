###############################
### MAXENT SCRIPT DRAFT for strict microhabitats only #######
###############################
# MAXENT MODELS AND EVALUATIONS

# the maxent models with lots of points/replications need extra help
options(java.parameters = "-Xmx8000m")
#
library(dismo); library(rJava); library(maptools)
library(raster)

#dont need
#install.packages("rmaxent")
#library(rmaxent)

###### maxent pipeline #########
# READ IN DATA #
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogetherGTiff.gri')

# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors <- crop(x = ClimateData, y = geographic.extent)

# load file with presence points
ArbPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_strict_ressmall/chull.shp")
TerrPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_strict_ressmall/chull.shp")
AquaPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Aqua_Points_strict_ressmall/chull.shp")
CavePointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Cave_Points_strict_ressmall/chull.shp")
FossPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Foss_Points_strict_ressmall/chull.shp")
SaxPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Sax_Points_strict_ressmall/chull.shp")

# make into data frame for maxent
ArbDF <- data.frame(ArbPointsS)
ArbDF <- ArbDF[,1:2]
TerrDF <- data.frame(TerrPointsS)
TerrDF <- TerrDF[,1:2]
AquaDF <- data.frame(AquaPointsS)
AquaDF <- AquaDF[,1:2]
CaveDF <- data.frame(CavePointsS)
CaveDF <- CaveDF[,1:2]
FossDF <- data.frame(FossPointsS)
FossDF <- FossDF[,1:2]
SaxDF <- data.frame(SaxPointsS)
SaxDF <- SaxDF[,1:2]

# RESOLUTION TESTING WITH 10 FOLD CV
ArbModTestCV <- maxent(predictors, ArbDF, args=c("-J","-P","replicates=10"), 
                       path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_resolution_testCV_10")
TerrModTestCV <- maxent(predictors, TerrDF, args=c("-J","-P","replicates=10"), 
                        path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_strict_resolution_testCV_10")

## take out points outside of the extent area - maxent will take out point without data automatically so no need
# like korea, italy, and water points
outliers <- extract(predictors, ArbPointsS)
out <- which(is.na(outliers))
out
View(outliers)
# arb outliers
dropA <- c(34)
ArbNew <- ArbDF[-out,]
#terr outliers
dropT <- c(16,37,71,98,114,135,145,164,165)
TerrNew <- TerrDF[-out,]
# aqua outliers
dropW <- c(20,47,87)
AquaNew <- AquaDF[-out,]
# cave outliers
dropC <- c(1,2,3,4,8,9,17)
CaveNew <- CaveDF[-out,]
# sax outliers
dropS <- c(6)
SaxNew <- SaxDF[-out,]

# assign occurrence points - dont do this because running 10-fold with the full dataset does this automatically
occA <- ArbDF
occA <- as.matrix(occA)
foldA <- kfold(occA, k=5)
occtestA <- occA[foldA == 1, ]
occtrainA <- occA[foldA != 1, ]
# assign occurrence points
occT <- TerrDF
occT <- as.matrix(occT)
foldT <- kfold(occT, k=5)
occtestT <- occT[foldT == 1, ]
occtrainT <- occT[foldT != 1, ]
# assign occurrence points
occW <- AquaDF
occW <- as.matrix(occW)
foldW <- kfold(occW, k=5)
occtestW <- occW[foldW == 1, ]
occtrainW <- occW[foldW != 1, ]
# assign occurrence points
occC <- CaveDF
occC <- as.matrix(occC)
foldC <- kfold(occC, k=5)
occtestC <- occC[foldC == 1, ]
occtrainC <- occC[foldC != 1, ]
# assign occurrence points
occF <- FossDF
occF <- as.matrix(occF)
foldF <- kfold(occF, k=5)
occtestF <- occF[foldF == 1, ]
occtrainF <- occF[foldF != 1, ]
# assign occurrence points
occS <- SaxDF
occS <- as.matrix(occS)
foldS <- kfold(occS, k=5)
occtestS <- occS[foldS == 1, ]
occtrainS <- occS[foldS != 1, ]
#
#testing
# cross validation
ArbModTestCV <- maxent(predictors, occtrainA, args=c("-J","-P",'replicates=2'), path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_resolution_testCV")
# bootstrap
ArbModTestBS <- maxent(predictors, ArbDF, args=c("-J","-P","replicates=2","replicatetype=bootstrap"), path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_resolution_testBS")
# subsampling
ArbModTestSS <- maxent(predictors, ArbDF, args=c("-J","-P","replicates=5","replicatetype=subsample","randomtestpoints=20","nooutputgrids"), path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_resolution_testSS")
#
#
#
# maxent model try with 10 folds 
ArbMod <- maxent(predictors, ArbDF, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_Newres")

TerrMod <- maxent(predictors, TerrDF, args=c("-J","-P",'replicates=10'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_strict_Newres")

AquaMod <- maxent(predictors, AquaDF, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_strict_Newres")

CaveMod <- maxent(predictors, CaveDF, args=c("-J","-P",'replicates=10'), 
                   path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_strict_Newres")

FossMod <- maxent(predictors, FossDF, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_strict_Newres")

SaxMod <- maxent(predictors, SaxDF, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_strict_Newres")

#predict to entire dataset
# arboreal prediction
#ArbMod <- import_maxent("./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_res")
ArbPrediction <- dismo::predict(ArbMod, predictors, progress="text",
                  filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_strict_prediction.grd',
                  overwrite=T)
ArbPredictionAverage <- mean(ArbPrediction) 
writeRaster(ArbPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_strict'),
            overwrite=T)
#
# terrestrial prediction
TerrPrediction <- predict(TerrMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_strict_prediction.grd',
                         overwrite=T)
TerrPredictionAverage <- mean(TerrPrediction) 
writeRaster(TerrPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_strict'),
            overwrite=T)
#
# Aquatic prediction
AquaPrediction <- predict(AquaMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_strict_prediction.grd',
                         overwrite=T)
AquaPredictionAverage <- mean(AquaPrediction) 
writeRaster(AquaPredictionAverage, overwrite=T, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_strict'))
# 
# cave prediction
CavePrediction <- predict(CaveMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_strict_prediction.grd',
                         overwrite=T)
CavePredictionAverage <- mean(CavePrediction) 
writeRaster(CavePredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_strict'), overwrite=T)
#
# Fossorial prediction
FossPrediction <- predict(FossMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_strict_prediction.grd',
                         overwrite=T)
FossPredictionAverage <- mean(FossPrediction) 
writeRaster(FossPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_strict'), overwrite=T)
#
# Saxicolous prediction
SaxPrediction <- predict(SaxMod, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_strict_prediction.grd',
                         overwrite=T)
SaxPredictionAverage <- mean(SaxPrediction) 
writeRaster(SaxPredictionAverage, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_strict'), overwrite=T)






#############
#testing points this is only necessary if you split it up beforehand and ran a manual evaluation with your own partition scheme.
# might need to do this when we test one dataset predicting another such as Arb1 and Arb2
#############
#background data
bgS <- randomPoints(predictors,1000)
bg <- randomPoints(predictors, 10000)
# resolution testing #
ASTest <- evaluate(ArbMod, p=occtestA, a=bg, x=predictors)
ASTest
# resolution 0.030 the AUC is 0.965
# resolution 0.04166667* the AUC is 0.966
# resolution 0.045 the AUC is 0.967
# resolution 0.050 the AUC is 0.967
# resolution 0.055 the AUC is 0.967
# resolution 0.070 the AUC is 0.967
# resolution 0.080 the AUC is 0.968
# resolution 0.100 the AUC is 0.970
# resolution 0.200 the AUC is 0.964
# resolution 0.300 the AUC is 0.970
# resolution 0.350 the AUC is 0.961
# resolution 0.375 the AUC is 0.956
# resolution 0.380 the AUC is 0.944
# resolution 0.385 the AUC is 0.957
# resolution 0.390 the AUC is 0.962
# resolution 0.395 the AUC is 0.957
# resolution 0.397 the AUC is 0.967
# resolution 0.400 the AUC is 0.965
# resolution 0.405 the AUC is 0.964
# resolution 0.410 the AUC is 0.965
# resolution 0.425 the AUC is 0.962
# resolution 0.430 the AUC is 0.966
# resolution 0.435 the AUC is 0.953
# resolution 0.440 the AUC is 0.947
# resolution 0.450 the AUC is 0.955
# resolution 0.475 the AUC is 0.961
# resolution 0.500 the AUC is 0.959
# resolution 1.000 the AUC is 0.978
# resolution 2.000 the AUC is 0.784
TSTest <- evaluate(TerrMod, p=occtestT, a=bg, x=predictors)
TSTest 
# resolution 0.030 the AUC is 0.881
# resolution 0.04166667* the AUC is 0.882
# resolution 0.045 the AUC is 0.883
# resolution 0.050 the AUC is 0.887
# resolution 0.055 the AUC is 0.886
# resolution 0.070 the AUC is 0.890
# resolution 0.080 the AUC is 0.892
# resolution 0.100 the AUC is 0.893
# resolution 0.200 the AUC is 0.903
# resolution 0.300 the AUC is 0.902
# resolution 0.350 the AUC is 0.907
# resolution 0.375 the AUC is 0.902
# resolution 0.380 the AUC is 0.908
# resolution 0.385 the AUC is 0.902
# resolution 0.390 the AUC is 0.902
# resolution 0.395 the AUC is 0.906
# resolution 0.397 the AUC is 0.905
# resoltuion 0.400 the AUC is 0.895
# resolution 0.405 the AUC is 0.906
# resolution 0.410 the AUC is 0.899
# resolution 0.425 the AUC is 0.901
# resolution 0.430 the AUC is 0.904
# resolution 0.435 the AUC is 0.907
# resolution 0.440 the AUC is 0.899
# resolution 0.450 the AUC is 0.902
# resolution 0.475 the AUC is 0.899
# resolution 0.500 the AUC is 0.898
# resolution 1.000 the AUC is 0.883
# resolution 2.000 the AUC is 0.896

#simplest way to use 'evaluate'
# evaluate every model from the replication in arb
AS1 <- evaluate(ArbMod@models[[1]], p=occtestA, a=bg, x=predictors)
AS1 # 0.91
AS2 <- evaluate(ArbMod@models[[2]], p=occtestA, a=bg, x=predictors)
AS2 # 0.90
AS3 <- evaluate(ArbMod@models[[3]], p=occtestA, a=bg, x=predictors)
AS3 # 0.90
AS4 <- evaluate(ArbMod@models[[4]], p=occtestA, a=bg, x=predictors)
AS4 # 0.91
AS5 <- evaluate(ArbMod@models[[5]], p=occtestA, a=bg, x=predictors)
AS5 # 0.89
# evaluate every model from the replication in terr
TS1 <- evaluate(TerrMod@models[[1]], p=occtestT, a=bg, x=predictors)
TS1 # 0.83
TS2 <- evaluate(TerrMod@models[[2]], p=occtestT, a=bg, x=predictors)
TS2 # 0.81
TS3 <- evaluate(TerrMod@models[[3]], p=occtestT, a=bg, x=predictors)
TS3 # 0.83
TS4 <- evaluate(TerrMod@models[[4]], p=occtestT, a=bg, x=predictors)
TS4 # 0.81
TS5 <- evaluate(TerrMod@models[[5]], p=occtestT, a=bg, x=predictors)
TS5 # 0.82
# evaluate every model from the replication in aquatic
WS1 <- evaluate(AquaMod@models[[1]], p=occtestW, a=bg, x=predictors)
WS1 # 0.92
WS2 <- evaluate(AquaMod@models[[2]], p=occtestW, a=bg, x=predictors)
WS2 # 0.92
WS3 <- evaluate(AquaMod@models[[3]], p=occtestW, a=bg, x=predictors)
WS3 # 0.91
WS4 <- evaluate(AquaMod@models[[4]], p=occtestW, a=bg, x=predictors)
WS4 # 0.93
WS5 <- evaluate(AquaMod@models[[5]], p=occtestW, a=bg, x=predictors)
WS5 # 0.92
# evaluate every model from the replication in cave
CS1 <- evaluate(CaveMod@models[[1]], p=occtestC, a=bg, x=predictors)
CS1 # 0.98
CS2 <- evaluate(CaveMod@models[[2]], p=occtestC, a=bg, x=predictors)
CS2 # 0.98
CS3 <- evaluate(CaveMod@models[[3]], p=occtestC, a=bg, x=predictors)
CS3 # 0.98
CS4 <- evaluate(CaveMod@models[[4]], p=occtestC, a=bg, x=predictors)
CS4 # 0.98
CS5 <- evaluate(CaveMod@models[[5]], p=occtestC, a=bg, x=predictors)
CS5 # 0.98
# evaluate every model from the replication in fossorial
FS1 <- evaluate(FossMod@models[[1]], p=occtestF, a=bg, x=predictors)
FS1 # 0.89
FS2 <- evaluate(FossMod@models[[2]], p=occtestF, a=bg, x=predictors)
FS2 # 0.82
FS3 <- evaluate(FossMod@models[[3]], p=occtestF, a=bg, x=predictors)
FS3 # 0.92
FS4 <- evaluate(FossMod@models[[4]], p=occtestF, a=bg, x=predictors)
FS4 # 0.91
FS5 <- evaluate(FossMod@models[[5]], p=occtestF, a=bg, x=predictors)
FS5 # 0.87
# evaluate every model from the replication in sax
SS1 <- evaluate(SaxMod@models[[1]], p=occtestS, a=bg, x=predictors)
SS1 # 0.68
SS2 <- evaluate(SaxMod@models[[2]], p=occtestS, a=bg, x=predictors)
SS2 # 0.88
SS3 <- evaluate(SaxMod@models[[3]], p=occtestS, a=bg, x=predictors)
SS3 # 0.86
SS4 <- evaluate(SaxMod@models[[4]], p=occtestS, a=bg, x=predictors)
SS4 # 0.85
SS5 <- evaluate(SaxMod@models[[5]], p=occtestS, a=bg, x=predictors)
SS5 # 0.89
# THESE ARE OTHER WAYS TO EVALUATE, BUT GIVE THE SAME RESULT
# alternative 1
# extract values
pvtest <- data.frame(extract(predictors, occtestV))
avtest <- data.frame(extract(predictors, bg))
e2 <- evaluate(p=pvtest, a=avtest)
e2
# alternative 2
# predict to testing points
testp <- predict(me, pvtest)
head(testp)
testa <- predict(me, avtest)
e3 <- evaluate(p=testp, a=testa)
e3
threshold(e3)
plot(e3, 'ROC')
# look into more...
v <- extract(predictors, VegNew)
mess <-mess(predictors, v, full=FALSE)
plot(mess)
mess

















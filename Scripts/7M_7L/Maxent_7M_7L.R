###############################
### MAXENT SCRIPT DRAFT for 7M and 7L microhabitats only #######
###############################
# MAXENT MODELS AND EVALUATIONS

# the maxent models with lots of points/replications need extra help
options(java.parameters = "-Xmx8000m")
#
library(dismo); library(rJava); library(maptools)
library(raster)

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
ArbPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_7M/chull.shp")
TerrPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_7M/chull.shp")
AquaPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Aqua_Points_7M/chull.shp")
CavePoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Cave_Points_7M/chull.shp")
FossPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Foss_Points_7M/chull.shp")
SaxPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Sax_Points_7M/chull.shp")
ArbPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_7L/chull.shp")
TerrPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_7L/chull.shp")
AquaPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Aqua_Points_7L/chull.shp")
CavePoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Cave_Points_7L/chull.shp")
FossPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Foss_Points_7L/chull.shp")
SaxPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Sax_Points_7L/chull.shp")


# make into data frame for maxent
ArbDFM <- data.frame(ArbPoints7M)
ArbDFM <- ArbDFM[,1:2]
TerrDFM <- data.frame(TerrPoints7M)
TerrDFM <- TerrDFM[,1:2]
AquaDFM <- data.frame(AquaPoints7M)
AquaDFM <- AquaDFM[,1:2]
CaveDFM <- data.frame(CavePoints7M)
CaveDFM <- CaveDFM[,1:2]
FossDFM <- data.frame(FossPoints7M)
FossDFM <- FossDFM[,1:2]
SaxDFM <- data.frame(SaxPoints7M)
SaxDFM <- SaxDFM[,1:2]

ArbDFL <- data.frame(ArbPoints7L)
ArbDFL <- ArbDFL[,1:2]
TerrDFL <- data.frame(TerrPoints7L)
TerrDFL <- TerrDFL[,1:2]
AquaDFL <- data.frame(AquaPoints7L)
AquaDFL <- AquaDFL[,1:2]
CaveDFL <- data.frame(CavePoints7L)
CaveDFL <- CaveDFL[,1:2]
FossDFL <- data.frame(FossPoints7L)
FossDFL <- FossDFL[,1:2]
SaxDFL <- data.frame(SaxPoints7L)
SaxDFL <- SaxDFL[,1:2]

# RESOLUTION TESTING WITH 10 FOLD CV
# REDO WITH 7M AND 7L
ArbModTestCV <- maxent(predictors, ArbDF, args=c("-J","-P","replicates=10"), 
                       path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_strict_resolution_testCV_10")
TerrModTestCV <- maxent(predictors, TerrDF, args=c("-J","-P","replicates=10"), 
                        path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_strict_resolution_testCV_10")
#
#

# 7M

# maxent model try with 10 folds 
ArbModM <- maxent(predictors, ArbDFM, args=c("-J","-P",'replicates=10'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_7M")

TerrModM <- maxent(predictors, TerrDFM, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_7M")

AquaModM <- maxent(predictors, AquaDFM, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_7M")

CaveModM <- maxent(predictors, CaveDFM, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_7M")

FossModM <- maxent(predictors, FossDFM, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_7M")

SaxModM <- maxent(predictors, SaxDFM, args=c("-J","-P",'replicates=10'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_7M")

#predict to entire dataset
# arboreal prediction
ArbPredictionM <- dismo::predict(ArbModM, predictors, progress="text",
                                filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_7M_prediction.grd',
                                overwrite=T)
ArbPredictionAverageM <- mean(ArbPredictionM) 
writeRaster(ArbPredictionAverageM, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_7M'),
            overwrite=T)
#
# terrestrial prediction
TerrPredictionM <- predict(TerrModM, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_7M_prediction.grd',
                          overwrite=T)
TerrPredictionAverageM <- mean(TerrPredictionM) 
writeRaster(TerrPredictionAverageM, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_7M'),
            overwrite=T)
#
# Aquatic prediction
AquaPredictionM <- predict(AquaModM, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_7M_prediction.grd',
                          overwrite=T)
AquaPredictionAverageM <- mean(AquaPredictionM) 
writeRaster(AquaPredictionAverageM, overwrite=T, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_7M'))
# 
# cave prediction
CavePredictionM <- predict(CaveModM, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_7M_prediction.grd',
                          overwrite=T)
CavePredictionAverageM <- mean(CavePredictionM) 
writeRaster(CavePredictionAverageM, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_7M'), overwrite=T)
#
# Fossorial prediction
FossPredictionM <- predict(FossModM, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_7M_prediction.grd',
                          overwrite=T)
FossPredictionAverageM <- mean(FossPredictionM) 
writeRaster(FossPredictionAverageM, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_7M'), overwrite=T)
#
# Saxicolous prediction
SaxPredictionM <- predict(SaxModM, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_7M_prediction.grd',
                         overwrite=T)
SaxPredictionAverageM <- mean(SaxPredictionM) 
writeRaster(SaxPredictionAverageM, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_7M'), overwrite=T)






# 7L

# maxent model try with 10 folds 
ArbModL <- maxent(predictors, ArbDFL, args=c("-J","-P",'replicates=10'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/ArbMod_7L")

TerrModL <- maxent(predictors, TerrDFL, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/TerrMod_7L")

AquaModL <- maxent(predictors, AquaDFL, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/AquaMod_7L")

CaveModL <- maxent(predictors, CaveDFL, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/CaveMod_7L")

FossModL <- maxent(predictors, FossDFL, args=c("-J","-P",'replicates=10'), 
                  path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/FossMod_7L")

SaxModL <- maxent(predictors, SaxDFL, args=c("-J","-P",'replicates=10'), 
                 path="./Analysis_Scripts/Chapter3/ENM/Maxent_Files/SaxMod_7L")

#predict to entire dataset
# arboreal prediction
ArbPredictionL <- dismo::predict(ArbModL, predictors, progress="text",
                                filename='./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_7L_prediction.grd',
                                overwrite=T)
ArbPredictionAverageL <- mean(ArbPredictionL) 
writeRaster(ArbPredictionAverageL, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_7L'),
            overwrite=T)
#
# terrestrial prediction
TerrPredictionL <- predict(TerrModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_7L_prediction.grd',
                          overwrite=T)
TerrPredictionAverageL <- mean(TerrPredictionL) 
writeRaster(TerrPredictionAverageL, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_7L'),
            overwrite=T)
#
# Aquatic prediction
AquaPredictionL <- predict(AquaModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_7L_prediction.grd',
                          overwrite=T)
AquaPredictionAverageL <- mean(AquaPredictionL) 
writeRaster(AquaPredictionAverageL, overwrite=T, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_7L'))
# 
# cave prediction
CavePredictionL <- predict(CaveModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_7L_prediction.grd',
                          overwrite=T)
CavePredictionAverageL <- mean(CavePredictionL) 
writeRaster(CavePredictionAverageL, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_7L'), overwrite=T)
#
# Fossorial prediction
FossPredictionL <- predict(FossModL, predictors, progress="text",
                          filename='./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_7L_prediction.grd',
                          overwrite=T)
FossPredictionAverageL <- mean(FossPredictionL) 
writeRaster(FossPredictionAverageL, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_7L'), overwrite=T)
#
# Saxicolous prediction
SaxPredictionL <- predict(SaxModL, predictors, progress="text",
                         filename='./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_7L_prediction.grd',
                         overwrite=T)
SaxPredictionAverageL <- mean(SaxPredictionL) 
writeRaster(SaxPredictionAverageL, paste0('./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_7L'), overwrite=T)




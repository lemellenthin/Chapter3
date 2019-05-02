###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###  for the microhabitat LEN_M1_M2  ##############
###################################################

# USES OUTPUT OF THE MAXENT MODEL
# MAKES A POLYGON OF A CERTAIN SUITABILITY THRESHOLD
# ASKS HOW MUCH OF THE POLYGON OVERLAP IS IN THE SUITABILITY BY AREA

###############################

# packages
library(phyloclim); library(geosphere); library(raster)
library(rgdal); library(rgeos)

# load the maxent predictions
ArbModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_lenient.grd")
TerrModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_lenient.grd")
AquaModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_lenient.grd")
CaveModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_lenient.grd")
FossModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_lenient.grd")
SaxModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_lenient.grd")
ArbModM1 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_M1.grd")
TerrModM1 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_M1.grd")
AquaModM1 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_M1.grd")
CaveModM1 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_M1.grd")
FossModM1 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_M1.grd")
SaxModM1 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_M1.grd")
ArbModM2 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_M2.grd")
TerrModM2 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_M2.grd")
AquaModM2 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/AquaMod_prediction_M2.grd")
CaveModM2 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/CaveMod_prediction_M2.grd")
FossModM2 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/FossMod_prediction_M2.grd")
SaxModM2 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/SaxMod_prediction_M2.grd")

# load the polygons
ArbPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/ArbPoly_lenient/chull.shp")
TerrPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/TerrPoly_lenient/chull.shp")
AquaPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/AquaPoly_lenient/chull.shp")
CavePolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/CavePoly_lenient/chull.shp")
FossPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/FossPoly_lenient/chull.shp")
SaxPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/SaxPoly_lenient/chull.shp")
ArbPolyM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/ArbPoly_M1/chull.shp")
TerrPolyM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/TerrPoly_M1/chull.shp")
AquaPolyM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/AquaPoly_M1/chull.shp")
CavePolyM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/CavePoly_M1/chull.shp")
FossPolyM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/FossPoly_M1/chull.shp")
SaxPolyM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/SaxPoly_M1/chull.shp")
ArbPolyM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/ArbPoly_M2/chull.shp")
TerrPolyM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/TerrPoly_M2/chull.shp")
AquaPolyM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/AquaPoly_M2/chull.shp")
CavePolyM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/CavePoly_M2/chull.shp")
FossPolyM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/FossPoly_M2/chull.shp")
SaxPolyM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Polygons/SaxPoly_M2/chull.shp")

# load the points
ArbPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_lenient/chull.shp")
TerrPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_lenient/chull.shp")
AquaPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Aqua_Points_lenient/chull.shp")
CavePointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Cave_Points_lenient/chull.shp")
FossPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Foss_Points_lenient/chull.shp")
SaxPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Sax_Points_lenient/chull.shp")
ArbPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_M1/chull.shp")
TerrPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_M1/chull.shp")
AquaPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Aqua_Points_M1/chull.shp")
CavePointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Cave_Points_M1/chull.shp")
FossPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Foss_Points_M1/chull.shp")
SaxPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Sax_Points_M1/chull.shp")
ArbPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_M2/chull.shp")
TerrPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_M2/chull.shp")
AquaPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Aqua_Points_M2/chull.shp")
CavePointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Cave_Points_M2/chull.shp")
FossPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Foss_Points_M2/chull.shp")
SaxPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Sax_Points_M2/chull.shp")


# check what they look like
plot(ArbR)

# give them a threshold suitability score
ArbModLSS <- ArbModL > 0.5
TerrModLSS <- TerrModL > 0.5
AquaModLSS <- AquaModL > 0.5
CaveModLSS <- CaveModL > 0.5
FossModLSS <- FossModL > 0.5
SaxModLSS <- SaxModL > 0.5
ArbModM1SS <- ArbModM1 > 0.5
TerrModM1SS <- TerrModM1 > 0.5
AquaModM1SS <- AquaModM1 > 0.5
CaveModM1SS <- CaveModM1 > 0.5
FossModM1SS <- FossModM1 > 0.5
SaxModM1SS <- SaxModM1 > 0.5
ArbModM2SS <- ArbModM2 > 0.5
TerrModM2SS <- TerrModM2 > 0.5
AquaModM2SS <- AquaModM2 > 0.5
CaveModM2SS <- CaveModM2 > 0.5
FossModM2SS <- FossModM2 > 0.5
SaxModM2SS <- SaxModM2 > 0.5

# turn it into a polygon
ArbModpolL <- rasterToPolygons(ArbModLSS,function(x) x == 1,dissolve=T)
TerrModpolL <- rasterToPolygons(TerrModLSS,function(x) x == 1,dissolve=T)
AquaModpolL <- rasterToPolygons(AquaModLSS,function(x) x == 1,dissolve=T)
CaveModpolL <- rasterToPolygons(CaveModLSS,function(x) x == 1,dissolve=T)
FossModpolL <- rasterToPolygons(FossModLSS,function(x) x == 1,dissolve=T)
SaxModpolL <- rasterToPolygons(SaxModLSS,function(x) x == 1,dissolve=T)
ArbModpolM1 <- rasterToPolygons(ArbModM1SS,function(x) x == 1,dissolve=T)
TerrModpolM1 <- rasterToPolygons(TerrModM1SS,function(x) x == 1,dissolve=T)
AquaModpolM1 <- rasterToPolygons(AquaModM1SS,function(x) x == 1,dissolve=T)
CaveModpolM1 <- rasterToPolygons(CaveModM1SS,function(x) x == 1,dissolve=T)
FossModpolM1 <- rasterToPolygons(FossModM1SS,function(x) x == 1,dissolve=T)
SaxModpolM1 <- rasterToPolygons(SaxModM1SS,function(x) x == 1,dissolve=T)
ArbModpolM2 <- rasterToPolygons(ArbModM2SS,function(x) x == 1,dissolve=T)
TerrModpolM2 <- rasterToPolygons(TerrModM2SS,function(x) x == 1,dissolve=T)
AquaModpolM2 <- rasterToPolygons(AquaModM2SS,function(x) x == 1,dissolve=T)
CaveModpolM2 <- rasterToPolygons(CaveModM2SS,function(x) x == 1,dissolve=T)
FossModpolM2 <- rasterToPolygons(FossModM2SS,function(x) x == 1,dissolve=T)
SaxModpolM2 <- rasterToPolygons(SaxModM2SS,function(x) x == 1,dissolve=T)

#
#maybe <- extract(ArbSS, ArbPoly)
#summary(maybe[[1]])

# get the area of the niche polygon
areaPolygon(ArbModpolL) / 1e6
# 2190731 
areaPolygon(TerrModpolL) / 1e6
# 5408492 
areaPolygon(AquaModpolL) / 1e6
# 3652572 
areaPolygon(CaveModpolL) / 1e6
# 1979474 
areaPolygon(FossModpolL) / 1e6
# 4037977 
areaPolygon(SaxModpolL) / 1e6
# 2926900 

areaPolygon(ArbModpolM1) / 1e6
# 2004242 
areaPolygon(TerrModpolM1) / 1e6
# 6508263 
areaPolygon(AquaModpolM1) / 1e6
# 3844713 
areaPolygon(CaveModpolM1) / 1e6
# 1317313 
areaPolygon(FossModpolM1) / 1e6
# 8001117 
areaPolygon(SaxModpolM1) / 1e6
# 3031525 

areaPolygon(ArbModpolM2) / 1e6
# 7069824 
areaPolygon(TerrModpolM2) / 1e6
# 2891744 
areaPolygon(AquaModpolM2) / 1e6
# 2214916 
areaPolygon(CaveModpolM2) / 1e6
# 1405879 
areaPolygon(FossModpolM2) / 1e6
# 3757558 
areaPolygon(SaxModpolM2) / 1e6
# 1615342 


# niche overlap 
# arb and terr
dismo::nicheOverlap(ArbModLSS, TerrModLSS, stat='I', mask=T, checkNegatives = T)
# 0.335074
dismo::nicheOverlap(ArbModLSS, TerrModLSS, stat='D', mask=T, checkNegatives = T)
# 0.2032413
dismo::nicheOverlap(ArbModL, TerrModL, stat='I', mask=T, checkNegatives = T)
# 0.80
dismo::nicheOverlap(ArbModL, TerrModL, stat='D', mask=T, checkNegatives = T)
# 0.53

# arb and water
dismo::nicheOverlap(ArbModLSS, AquaModLSS, stat='I', mask=T, checkNegatives = T)
# 0.07486559
dismo::nicheOverlap(ArbModLSS, AquaModLSS, stat='D', mask=T, checkNegatives = T)
# 0.05429894
dismo::nicheOverlap(ArbModL, AquaModL, stat='I', mask=T, checkNegatives = T)
# 0.49
dismo::nicheOverlap(ArbModL, AquaModL, stat='D', mask=T, checkNegatives = T)
# 0.25

# arb and cave
dismo::nicheOverlap(ArbModLSS, CaveModLSS, stat='I', mask=T, checkNegatives = T)
# 0.07258061
dismo::nicheOverlap(ArbModLSS, CaveModLSS, stat='D', mask=T, checkNegatives = T)
# 0.07097517
dismo::nicheOverlap(ArbModL, CaveModL, stat='I', mask=T, checkNegatives = T)
# 0.59
dismo::nicheOverlap(ArbModL, CaveModL, stat='D', mask=T, checkNegatives = T)
# 0.28

# arb and foss
dismo::nicheOverlap(ArbModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.2824101
dismo::nicheOverlap(ArbModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.1984237
dismo::nicheOverlap(ArbModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.84
dismo::nicheOverlap(ArbModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.55

# arb and sax
dismo::nicheOverlap(ArbModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.1914775
dismo::nicheOverlap(ArbModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.1612611
dismo::nicheOverlap(ArbModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.70
dismo::nicheOverlap(ArbModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.41

# terr and water
dismo::nicheOverlap(TerrModLSS, AquaModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6413645
dismo::nicheOverlap(TerrModLSS, AquaModLSS, stat='D', mask=T, checkNegatives = T)
# 0.5363731
dismo::nicheOverlap(TerrModL, AquaModL, stat='I', mask=T, checkNegatives = T)
# 0.78
dismo::nicheOverlap(TerrModL, AquaModL, stat='D', mask=T, checkNegatives = T)
# 0.54

# terr and cave
dismo::nicheOverlap(TerrModLSS, CaveModLSS, stat='I', mask=T, checkNegatives = T)
# 0.5470874
dismo::nicheOverlap(TerrModLSS, CaveModLSS, stat='D', mask=T, checkNegatives = T)
# 0.3244992
dismo::nicheOverlap(TerrModL, CaveModL, stat='I', mask=T, checkNegatives = T)
# 0.75
dismo::nicheOverlap(TerrModL, CaveModL, stat='D', mask=T, checkNegatives = T)
# 0.44

# terr and foos
dismo::nicheOverlap(TerrModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.7834162
dismo::nicheOverlap(TerrModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6763171
dismo::nicheOverlap(TerrModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.94
dismo::nicheOverlap(TerrModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.75

# terr and sax
dismo::nicheOverlap(TerrModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.5853243
dismo::nicheOverlap(TerrModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.4215566
dismo::nicheOverlap(TerrModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.81
dismo::nicheOverlap(TerrModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.54

# water and cave
dismo::nicheOverlap(AquaModLSS, CaveModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6320775
dismo::nicheOverlap(AquaModLSS, CaveModLSS, stat='D', mask=T, checkNegatives = T)
# 0.4482964
dismo::nicheOverlap(AquaModL, CaveModL, stat='I', mask=T, checkNegatives = T)
# 0.78
dismo::nicheOverlap(AquaModL, CaveModL, stat='D', mask=T, checkNegatives = T)
# 0.55

# water and foss
dismo::nicheOverlap(AquaModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6890141
dismo::nicheOverlap(AquaModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.667471
dismo::nicheOverlap(AquaModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.81
dismo::nicheOverlap(AquaModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.59

# water and sax
dismo::nicheOverlap(AquaModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6113709
dismo::nicheOverlap(AquaModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.5265045
dismo::nicheOverlap(AquaModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.70
dismo::nicheOverlap(AquaModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.54

# cave and foss
dismo::nicheOverlap(CaveModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6816974
dismo::nicheOverlap(CaveModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.4683719
dismo::nicheOverlap(CaveModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.81
dismo::nicheOverlap(CaveModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.53

# cave and sax
dismo::nicheOverlap(CaveModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.8041828
dismo::nicheOverlap(CaveModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6622965
dismo::nicheOverlap(CaveModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.94
dismo::nicheOverlap(CaveModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.75

# foss and sax
dismo::nicheOverlap(FossModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.687629
dismo::nicheOverlap(FossModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.5736615
dismo::nicheOverlap(FossModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.82
dismo::nicheOverlap(FossModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.58

######################################################################################

# arb and terr
dismo::nicheOverlap(ArbModM1SS, TerrModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.2718426 
dismo::nicheOverlap(ArbModM1SS, TerrModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1433844 
dismo::nicheOverlap(ArbModM1, TerrModM1, stat='I', mask=T, checkNegatives = T)
# 0.68
dismo::nicheOverlap(ArbModM1, TerrModM1, stat='D', mask=T, checkNegatives = T)
# 0.40

# arb and water
dismo::nicheOverlap(ArbModM1SS, AquaModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.05903348 
dismo::nicheOverlap(ArbModM1SS, AquaModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.03952373 
dismo::nicheOverlap(ArbModM1, AquaModM1, stat='I', mask=T, checkNegatives = T)
# 0.34
dismo::nicheOverlap(ArbModM1, AquaModM1, stat='D', mask=T, checkNegatives = T)
# 0.14

# arb and cave
dismo::nicheOverlap(ArbModM1SS, CaveModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.07125346 
dismo::nicheOverlap(ArbModM1SS, CaveModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.05990269
dismo::nicheOverlap(ArbModM1, CaveModM1, stat='I', mask=T, checkNegatives = T)
# 0.29
dismo::nicheOverlap(ArbModM1, CaveModM1, stat='D', mask=T, checkNegatives = T)
# 0.12

# arb and foss
dismo::nicheOverlap(ArbModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.4013211 
dismo::nicheOverlap(ArbModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.2056796 
dismo::nicheOverlap(ArbModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.90
dismo::nicheOverlap(ArbModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.69

# arb and sax
dismo::nicheOverlap(ArbModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.2839859 
dismo::nicheOverlap(ArbModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.2217712 
dismo::nicheOverlap(ArbModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.69
dismo::nicheOverlap(ArbModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.41

# terr and water
dismo::nicheOverlap(TerrModM1SS, AquaModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.6550893 
dismo::nicheOverlap(TerrModM1SS, AquaModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.51609 
dismo::nicheOverlap(TerrModM1, AquaModM1, stat='I', mask=T, checkNegatives = T)
# 0.78
dismo::nicheOverlap(TerrModM1, AquaModM1, stat='D', mask=T, checkNegatives = T)
# 0.53

# terr and cave
dismo::nicheOverlap(TerrModM1SS, CaveModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.4434297
dismo::nicheOverlap(TerrModM1SS, CaveModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1966299
dismo::nicheOverlap(TerrModM1, CaveModM1, stat='I', mask=T, checkNegatives = T)
# 0.60
dismo::nicheOverlap(TerrModM1, CaveModM1, stat='D', mask=T, checkNegatives = T)
# 0.33

# terr and foos
dismo::nicheOverlap(TerrModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.246201
dismo::nicheOverlap(TerrModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.239224
dismo::nicheOverlap(TerrModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.75
dismo::nicheOverlap(TerrModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.46

# terr and sax
dismo::nicheOverlap(TerrModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.6431054
dismo::nicheOverlap(TerrModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.4343685
dismo::nicheOverlap(TerrModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.91
dismo::nicheOverlap(TerrModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.69

# water and cave
dismo::nicheOverlap(AquaModM1SS, CaveModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.5038308
dismo::nicheOverlap(AquaModM1SS, CaveModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.2835859
dismo::nicheOverlap(AquaModM1, CaveModM1, stat='I', mask=T, checkNegatives = T)
# 0.68
dismo::nicheOverlap(AquaModM1, CaveModM1, stat='D', mask=T, checkNegatives = T)
# 0.44

# water and foss
dismo::nicheOverlap(AquaModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.1194583
dismo::nicheOverlap(AquaModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.09144416
dismo::nicheOverlap(AquaModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.49
dismo::nicheOverlap(AquaModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.22

# water and sax
dismo::nicheOverlap(AquaModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.5580733
dismo::nicheOverlap(AquaModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.4784565
dismo::nicheOverlap(AquaModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.69
dismo::nicheOverlap(AquaModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.52

# cave and foss
dismo::nicheOverlap(CaveModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.08602071
dismo::nicheOverlap(CaveModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.03706317
dismo::nicheOverlap(CaveModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.34
dismo::nicheOverlap(CaveModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.13

# cave and sax
dismo::nicheOverlap(CaveModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.6532449
dismo::nicheOverlap(CaveModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.428869
dismo::nicheOverlap(CaveModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.74
dismo::nicheOverlap(CaveModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.49

# foss and sax
dismo::nicheOverlap(FossModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.1873304
dismo::nicheOverlap(FossModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1229417
dismo::nicheOverlap(FossModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.70
dismo::nicheOverlap(FossModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.39

######################################################################################

# arb and terr
dismo::nicheOverlap(ArbModM2SS, TerrModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.5027109
dismo::nicheOverlap(ArbModM2SS, TerrModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.3100052
dismo::nicheOverlap(ArbModM2, TerrModM2, stat='I', mask=T, checkNegatives = T)
# 0.79
dismo::nicheOverlap(ArbModM2, TerrModM2, stat='D', mask=T, checkNegatives = T)
# 0.51

# arb and water
dismo::nicheOverlap(ArbModM2SS, AquaModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.5370759
dismo::nicheOverlap(ArbModM2SS, AquaModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2939174
dismo::nicheOverlap(ArbModM2, AquaModM2, stat='I', mask=T, checkNegatives = T)
# 0.75
dismo::nicheOverlap(ArbModM2, AquaModM2, stat='D', mask=T, checkNegatives = T)
# 0.44

# arb and cave
dismo::nicheOverlap(ArbModM2SS, CaveModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.4349831
dismo::nicheOverlap(ArbModM2SS, CaveModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1902263
dismo::nicheOverlap(ArbModM2, CaveModM2, stat='I', mask=T, checkNegatives = T)
# 0.56
dismo::nicheOverlap(ArbModM2, CaveModM2, stat='D', mask=T, checkNegatives = T)
# 0.29

# arb and foss
dismo::nicheOverlap(ArbModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.7049879
dismo::nicheOverlap(ArbModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.5129082
dismo::nicheOverlap(ArbModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.89
dismo::nicheOverlap(ArbModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.63

# arb and sax
dismo::nicheOverlap(ArbModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.4486552
dismo::nicheOverlap(ArbModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2072751
dismo::nicheOverlap(ArbModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.67
dismo::nicheOverlap(ArbModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.37

# terr and water
dismo::nicheOverlap(TerrModM2SS, AquaModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.3251004
dismo::nicheOverlap(TerrModM2SS, AquaModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2885071
dismo::nicheOverlap(TerrModM2, AquaModM2, stat='I', mask=T, checkNegatives = T)
# 0.59
dismo::nicheOverlap(TerrModM2, AquaModM2, stat='D', mask=T, checkNegatives = T)
# 0.33

# terr and cave
dismo::nicheOverlap(TerrModM2SS, CaveModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.3122789
dismo::nicheOverlap(TerrModM2SS, CaveModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2214574
dismo::nicheOverlap(TerrModM2, CaveModM2, stat='I', mask=T, checkNegatives = T)
# 0.54
dismo::nicheOverlap(TerrModM2, CaveModM2, stat='D', mask=T, checkNegatives = T)
# 0.29

# terr and foos
dismo::nicheOverlap(TerrModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.2985239
dismo::nicheOverlap(TerrModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2530299
dismo::nicheOverlap(TerrModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.69
dismo::nicheOverlap(TerrModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.44

# terr and sax
dismo::nicheOverlap(TerrModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.5005205
dismo::nicheOverlap(TerrModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.3749778
dismo::nicheOverlap(TerrModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.83
dismo::nicheOverlap(TerrModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.53

# water and cave
dismo::nicheOverlap(AquaModM2SS, CaveModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.689502
dismo::nicheOverlap(AquaModM2SS, CaveModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.5509906
dismo::nicheOverlap(AquaModM2, CaveModM2, stat='I', mask=T, checkNegatives = T)
# 0.83
dismo::nicheOverlap(AquaModM2, CaveModM2, stat='D', mask=T, checkNegatives = T)
# 0.63

# water and foss
dismo::nicheOverlap(AquaModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.7211122
dismo::nicheOverlap(AquaModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.5424186
dismo::nicheOverlap(AquaModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.88
dismo::nicheOverlap(AquaModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.65

# water and sax
dismo::nicheOverlap(AquaModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.6707631
dismo::nicheOverlap(AquaModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.5662575
dismo::nicheOverlap(AquaModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.75
dismo::nicheOverlap(AquaModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.58

# cave and foss
dismo::nicheOverlap(CaveModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.5704339
dismo::nicheOverlap(CaveModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.3428828
dismo::nicheOverlap(CaveModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.74
dismo::nicheOverlap(CaveModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.48

# cave and sax
dismo::nicheOverlap(CaveModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.5786303
dismo::nicheOverlap(CaveModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.5477281
dismo::nicheOverlap(CaveModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.79
dismo::nicheOverlap(CaveModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.58

# foss and sax
dismo::nicheOverlap(FossModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.5020885
dismo::nicheOverlap(FossModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.3188282
dismo::nicheOverlap(FossModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.72
dismo::nicheOverlap(FossModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.44

###
# arb poly intersect with arb niche
# ArbPolyL area
areaPolygon(ArbPolyL) / 1e6 # 675927
# ArbModpolL area
areaPolygon(ArbModpolL) / 1e6 # 2190731
# intersection between them
AAL <- raster::intersect(ArbPolyL,ArbModpolL)
areaPolygon(AAL) / 1e6 # 393267.4
# percent of the polygon explained by the niche
(393267.4/675927)*100 # 58.18

###
# terr poly intersect with terr niche
# TerrPolyL area
areaPolygon(TerrPolyL) / 1e6 # 3138294
# TerrModpolL area
areaPolygon(TerrModpolL) / 1e6 # 5408492
# intersection between them
TTL <- raster::intersect(TerrPolyL,TerrModpolL)
areaPolygon(TTL)/ 1e6 # 2631651
# percent of the polygon explained by the niche
(2631651/3138294)*100 # 83.8561

###
# water poly intersect with water niche
# AquaPolyL area
areaPolygon(AquaPolyL) / 1e6 # 2829823
# AquaModpolL area
areaPolygon(AquaModpolL) / 1e6 # 3652572
# intersection between them
WWL <- raster::intersect(AquaPolyL,AquaModpolL)
areaPolygon(WWL) / 1e6 # 2524340
# percent of the polygon explained by the niche
(2524340/2829823)*100 # 89.20

###
# cave poly intersect with cave niche
# CavePolyL area
areaPolygon(CavePolyL) / 1e6 # 1253455
# CaveModpolL area
areaPolygon(CaveModpolL) / 1e6 # 1979474
# intersection between them
CCL <- raster::intersect(CavePolyL,CaveModpolL)
areaPolygon(CCL) / 1e6 # 1129784
# percent of the polygon explained by the niche
(1129784/1253455)*100 # 90.13

###
# foss poly intersect with foss niche
# FossPolyL area
areaPolygon(FossPolyL) / 1e6 # 2373654
# FossModpolL area
areaPolygon(FossModpolL) / 1e6 # 4037977
# intersection between them
FFL <- raster::intersect(FossPolyL,FossModpolL)
areaPolygon(FFL) / 1e6 # 1991733
# percent of the polygon explained by the niche
(1991733/2373654)*100 # 83.91

###
# sax poly intersect with sax niche
# SaxPolyL area
areaPolygon(SaxPolyL) / 1e6 # 1886932
# SaxModpolL area
areaPolygon(SaxModpolL) / 1e6 # 2926900
# intersection between them
SSL <- raster::intersect(SaxPolyL,SaxModpolL)
areaPolygon(SSL) / 1e6 #  1684808
# percent of the polygon explained by the niche
(1684808/1886932)*100 # 89.28

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 2190731
areaPolygon(TerrModpolL) / 1e6 # 5408492
#inter bet Arb and Terr at 0.5 cutoff
interATLN <- raster::intersect(ArbModpolL, TerrModpolL)
areaPolygon(interATLN) / 1e6 # 1140372
# percent of each niche that overlaps with the other by area
# inter/arb niche
(1140372/2190731)*100 # 52.05
# inter/terr niche
(1140372/5408492)*100 # 21.08

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 2190731
areaPolygon(AquaModpolL) / 1e6 # 3652572
#inter bet Arb and water at 0.5 cutoff
interAWLN <- raster::intersect(ArbModpolL, AquaModpolL)
areaPolygon(interAWLN) / 1e6 # 215399
# percent of each niche that overlaps with the other by area
# inter/arb niche
(215399/2190731)*100 # 9.8
# inter/water niche
(215399/3652572)*100 # 5.89

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 2190731
areaPolygon(CaveModpolL) / 1e6 # 1979474
#inter bet Arb and cave at 0.5 cutoff
interACLN <- raster::intersect(ArbModpolL, CaveModpolL)
areaPolygon(interACLN) / 1e6 # 149254.2
# percent of each niche that overlaps with the other by area
# inter/arb niche
(149254.2/2190731)*100 # 6.8
# inter/cave niche
(149254.2/1979474)*100 # 7.5

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 2190731
areaPolygon(FossModpolL) / 1e6 # 4037977
#inter bet Arb and foss at 0.5 cutoff
interAFLN <- raster::intersect(ArbModpolL, FossModpolL)
areaPolygon(interAFLN) / 1e6 # 836433.7
# percent of each niche that overlaps with the other by area
# inter/arb niche
(836433.7/2190731)*100 # 38.18
# inter/foss niche
(836433.7/4037977)*100 # 20.71

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 2190731
areaPolygon(SaxModpolL) / 1e6 # 2926900
#inter bet Arb and sax at 0.5 cutoff
interASLN <- raster::intersect(ArbModpolL, SaxModpolL)
areaPolygon(interASLN) / 1e6 # 470506.9
# percent of each niche that overlaps with the other by area
# inter/arb niche
(470506.9/2190731)*100 # 21.5
# inter/sax niche
(470506.9/2926900)*100 # 16.07

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 5408492
areaPolygon(AquaModpolL) / 1e6 # 3652572
#inter bet Terr and Water at 0.5 cutoff
interTWLN <- raster::intersect(TerrModpolL, AquaModpolL)
areaPolygon(interTWLN) / 1e6 # 2846122
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2846122/5408492)*100 # 52.62
# inter/water niche
(2846122/3652572)*100 # 77.92

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 5408492
areaPolygon(CaveModpolL) / 1e6 # 1979474
#inter bet Terr and Cave at 0.5 cutoff
interTCLN <- raster::intersect(TerrModpolL, CaveModpolL)
areaPolygon(interTCLN) / 1e6 # 1818663
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1818663/5408492)*100 # 33.63
# inter/cave niche
(1818663/1979474)*100 # 91.87

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 5408492
areaPolygon(FossModpolL) / 1e6 # 4037977
#inter bet Terr and Foss at 0.5 cutoff
interTFLN <- raster::intersect(TerrModpolL, FossModpolL)
areaPolygon(interTFLN) / 1e6 # 3620837
# percent of each niche that overlaps with the other by area
# inter/terr niche
(3620837/5408492)*100 # 66.95
# inter/foss niche
(3620837/4037977)*100 # 89.66

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 5408492
areaPolygon(SaxModpolL) / 1e6 # 2926900
#inter bet Terr and Sax at 0.5 cutoff
interTSLN <- raster::intersect(TerrModpolL, SaxModpolL)
areaPolygon(interTSLN) / 1e6 # 2355656
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2355656/5408492)*100 # 43.55
# inter/sax niche
(2355656/2926900)*100 # 80.48

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolL) / 1e6 # 3652572
areaPolygon(CaveModpolL) / 1e6 # 1979474
#inter bet Water and Cave at 0.5 cutoff
interWCLN <- raster::intersect(AquaModpolL, CaveModpolL)
areaPolygon(interWCLN) / 1e6 # 1768032
# percent of each niche that overlaps with the other by area
# inter/water niche
(1768032/3652572)*100 # 48.4
# inter/cave niche
(1768032/1979474)*100 # 89.31

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolL) / 1e6 # 3652572
areaPolygon(FossModpolL) / 1e6 # 4037977
#inter bet Water and Foss at 0.5 cutoff
interWFLN <- raster::intersect(AquaModpolL, FossModpolL)
areaPolygon(interWFLN) / 1e6 # 2705865
# percent of each niche that overlaps with the other by area
# inter/water niche
(2705865/3652572)*100 # 74.08
# inter/foss niche
(2705865/4037977)*100 # 67.01

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolL) / 1e6 # 3652572
areaPolygon(SaxModpolL) / 1e6 # 2926900
#inter bet Water and Sax at 0.5 cutoff
interWSLN <- raster::intersect(AquaModpolL, SaxModpolL)
areaPolygon(interWSLN) / 1e6 # 2092599
# percent of each niche that overlaps with the other by area
# inter/water niche
(2092599/3652572)*100 # 57.29
# inter/Sax niche
(2092599/2926900)*100 # 71.49

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolL) / 1e6 # 1979474
areaPolygon(FossModpolL) / 1e6 # 4037977
#inter bet cave and foss at 0.5 cutoff
interCFLN <- raster::intersect(CaveModpolL, FossModpolL)
areaPolygon(interCFLN) / 1e6 # 1963321
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1963321/1979474)*100 # 99.18
# inter/foss niche
(1963321/4037977)*100 # 48.62

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolL) / 1e6 # 1979474
areaPolygon(SaxModpolL) / 1e6 # 2926900
#inter bet cave and sax at 0.5 cutoff
interCSLN <- raster::intersect(CaveModpolL, SaxModpolL)
areaPolygon(interCSLN) / 1e6 # 1935910
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1935910/1979474)*100 # 97.799
# inter/sax niche
(1935910/2926900)*100 # 66.14

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolL) / 1e6 # 4037977
areaPolygon(SaxModpolL) / 1e6 # 2926900
#inter bet foss and sax at 0.5 cutoff
interFSLN <- raster::intersect(FossModpolL, SaxModpolL)
areaPolygon(interFSLN) / 1e6 # 2388154
# percent of each niche that overlaps with the other by area
# inter/foss niche
(2388154/4037977)*100 # 59.14
# inter/sax niche
(2388154/2926900)*100 # 81.59

######################################################################################

# arb poly intersect with arb niche
# ArbPolyM1 area
areaPolygon(ArbPolyM1) / 1e6 # 659573
# ArbModpolM1 area
areaPolygon(ArbModpolM1) / 1e6 # 2004242
# intersection between them
AAM1 <- raster::intersect(ArbPolyM1,ArbModpolM1)
areaPolygon(AAM1) / 1e6 # 388394.2
# percent of the polygon explained by the niche
(388394.2/659573)*100 # 58.88

###
# terr poly intersect with terr niche
# TerrPolyM1 area
areaPolygon(TerrPolyM1) / 1e6 # 4456354
# TerrModpolM1 area
areaPolygon(TerrModpolM1) / 1e6 # 6508263
# intersection between them
TTM1 <- raster::intersect(TerrPolyM1,TerrModpolM1)
areaPolygon(TTM1) / 1e6 # 3862585
# percent of the polygon explained by the niche
(3862585/4456354)*100 # 86.67

###
# water poly intersect with water niche
# AquaPolyM1 area
areaPolygon(AquaPolyM1) / 1e6 # 2829823
# AquaModpolM1 area
areaPolygon(AquaModpolM1) / 1e6 # 3844713
# intersection between them
WWM1 <- raster::intersect(AquaPolyM1,AquaModpolM1)
areaPolygon(WWM1) / 1e6 # 2549519
# percent of the polygon explained by the niche
(2549519/2829823)*100 # 90.09

###
# cave poly intersect with cave niche
# CavePolyM1 area
areaPolygon(CavePolyM1) / 1e6 # 492224
# CaveModpolM1 area
areaPolygon(CaveModpolM1) / 1e6 # 1317313
# intersection between them
CCM1 <- raster::intersect(CavePolyM1,CaveModpolM1)
areaPolygon(CCM1) / 1e6 # 433288.7
# percent of the polygon explained by the niche
(433288.7/492224)*100 # 88.02

###
# foss poly intersect with foss niche
# FossPolyM1 area
areaPolygon(FossPolyM1) / 1e6 # 110435
# FossModpolS area
areaPolygon(FossModpolM1) / 1e6 # 8001117
# intersection between them
FFM1 <- raster::intersect(FossPolyM1,FossModpolM1)
areaPolygon(FFM1) / 1e6 # 106878.1
# percent of the polygon explained by the niche
(106878.1/110435)*100 # 96.77

###
# sax poly intersect with sax niche
# SaxPolyM1 area
areaPolygon(SaxPolyM1) / 1e6 # 180106.9
# SaxModpolM1 area
areaPolygon(SaxModpolM1) / 1e6 # 3031525
# intersection between them
SSM1 <- raster::intersect(SaxPolyM1,SaxModpolM1)
areaPolygon(SSM1) / 1e6 # 177934.5
# percent of the polygon explained by the niche
(177934.5/180106.9)*100 # 98.79

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 2004242
areaPolygon(TerrModpolM1) / 1e6 # 6508263
#inter bet Arb and Terr at 0.5 cutoff
interATM1N <- raster::intersect(ArbModpolM1, TerrModpolM1)
areaPolygon(interATM1N) / 1e6 # 945315.1
# percent of each niche that overlaps with the other by area
# inter/arb niche
(945315.1/2004242)*100 # 47.16
# inter/terr niche
(945315.1/6508263)*100 # 14.52

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 2004242
areaPolygon(AquaModpolM1) / 1e6 # 3844713
#inter bet Arb and water at 0.5 cutoff
interAWM1N <- raster::intersect(ArbModpolM1, AquaModpolM1)
areaPolygon(interAWM1N) / 1e6 # 172221.2
# percent of each niche that overlaps with the other by area
# inter/arb niche
(172221.2/2004242)*100 # 8.59
# inter/water niche
(172221.2/3844713)*100 # 4.479

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 2004242
areaPolygon(CaveModpolM1) / 1e6 # 1317313
#inter bet Arb and cave at 0.5 cutoff
interACM1N <- raster::intersect(ArbModpolM1, CaveModpolM1)
areaPolygon(interACM1N) / 1e6 # 111807.2
# percent of each niche that overlaps with the other by area
# inter/arb niche
(111807.2/2004242)*100 # 5.6
# inter/cave niche
(111807.2/1317313)*100 # 8.5

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 2004242
areaPolygon(FossModpolM1) / 1e6 # 8001117
#inter bet Arb and foss at 0.5 cutoff
interAFM1N <- raster::intersect(ArbModpolM1, FossModpolM1)
areaPolygon(interAFM1N) / 1e6 # 1530379
# percent of each niche that overlaps with the other by area
# inter/arb niche
(1530379/2004242)*100 # 76.4
# inter/foss niche
(1530379/8001117)*100 # 19.13

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 2004242
areaPolygon(SaxModpolM1) / 1e6 # 3031525
#inter bet Arb and sax at 0.5 cutoff
interASM1N <- raster::intersect(ArbModpolM1, SaxModpolM1)
areaPolygon(interASM1N) / 1e6 # 639932.5
# percent of each niche that overlaps with the other by area
# inter/arb niche
(639932.5/2004242)*100 # 31.9
# inter/sax niche
(639932.5/3031525)*100 # 21.1

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 6508263
areaPolygon(AquaModpolM1) / 1e6 # 3844713
#inter bet Terr and Water at 0.5 cutoff
interTWM1N <- raster::intersect(TerrModpolM1, AquaModpolM1)
areaPolygon(interTWM1N) / 1e6 # 3298756
# percent of each niche that overlaps with the other by area
# inter/terr niche
(3298756/6508263)*100 # 50.7
# inter/water niche
(3298756/3844713)*100 # 85.8

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 6508263
areaPolygon(CaveModpolM1) / 1e6 # 1317313
#inter bet Terr and Cave at 0.5 cutoff
interTCM1N <- raster::intersect(TerrModpolM1, CaveModpolM1)
areaPolygon(interTCM1N) / 1e6 # 1317313
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1317313/6508263)*100 # 20.24
# inter/cave niche
(1317313/1317313)*100 # 100

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 6508263
areaPolygon(FossModpolM1) / 1e6 # 8001117
#inter bet Terr and Foss at 0.5 cutoff
interTFM1N <- raster::intersect(TerrModpolM1, FossModpolM1)
areaPolygon(interTFM1N) / 1e6 # 1547993
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1547993/6508263)*100 # 23.8
# inter/foss niche
(1547993/8001117)*100 # 19.3

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 6508263
areaPolygon(SaxModpolM1) / 1e6 # 3031525
#inter bet Terr and Sax at 0.5 cutoff
interTSM1N <- raster::intersect(TerrModpolM1, SaxModpolM1)
areaPolygon(interTSM1N) / 1e6 # 2891230
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2891230/6508263)*100 # 44.4
# inter/sax niche
(2891230/3031525)*100 # 95.4

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolM1) / 1e6 # 3844713
areaPolygon(CaveModpolM1) / 1e6 # 1317313
#inter bet Water and Cave at 0.5 cutoff
interWCM1N <- raster::intersect(AquaModpolM1, CaveModpolM1)
areaPolygon(interWCM1N) / 1e6 # 1182930
# percent of each niche that overlaps with the other by area
# inter/water niche
(1182930/3844713)*100 # 30.8
# inter/cave niche
(1182930/1317313)*100 # 89.8

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolM1) / 1e6 # 3844713
areaPolygon(FossModpolM1) / 1e6 # 8001117
#inter bet Water and Foss at 0.5 cutoff
interWFM1N <- raster::intersect(AquaModpolM1, FossModpolM1)
areaPolygon(interWFM1N) / 1e6 # 631424.6
# percent of each niche that overlaps with the other by area
# inter/water niche
(631424.6/3844713)*100 # 16.4
# inter/foss niche
(631424.6/8001117)*100 # 7.9

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolM1) / 1e6 # 3844713
areaPolygon(SaxModpolM1) / 1e6 # 2506735
#inter bet Water and Sax at 0.5 cutoff
interWSM1N <- raster::intersect(AquaModpolM1, SaxModpolM1)
areaPolygon(interWSM1N) / 1e6 # 2007863
# percent of each niche that overlaps with the other by area
# inter/water niche
(2007863/3844713)*100 # 52.2
# inter/Sax niche
(2007863/3031525)*100 # 66.23

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolM1) / 1e6 # 1317313
areaPolygon(FossModpolM1) / 1e6 # 8001117
#inter bet cave and foss at 0.5 cutoff
interCFM1N <- raster::intersect(CaveModpolM1, FossModpolM1)
areaPolygon(interCFM1N) / 1e6 # 267966.8
# percent of each niche that overlaps with the other by area
# inter/cave niche
(267966.8/1317313)*100 # 20.3
# inter/foss niche
(267966.8/8001117)*100 # 3.3

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolM1) / 1e6 # 1317313
areaPolygon(SaxModpolM1) / 1e6 # 3031525
#inter bet cave and sax at 0.5 cutoff
interCSM1N <- raster::intersect(CaveModpolM1, SaxModpolM1)
areaPolygon(interCSM1N) / 1e6 # 1311009
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1311009/1317313)*100 # 99.5
# inter/sax niche
(1311009/3031525)*100 # 43.2

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolM1) / 1e6 # 8001117
areaPolygon(SaxModpolM1) / 1e6 # 3031525
#inter bet foss and sax at 0.5 cutoff
interFSM1N <- raster::intersect(FossModpolM1, SaxModpolM1)
areaPolygon(interFSM1N) / 1e6 # 819313.3
# percent of each niche that overlaps with the other by area
# inter/foss niche
(819313.3/8001117)*100 # 10.2
# inter/sax niche
(819313.3/3031525)*100 # 27.02


######################################################################################

# arb poly intersect with arb niche
# ArbPolyM2 area
areaPolygon(ArbPolyM2) / 1e6 # 4598454
# ArbModpolM2 area
areaPolygon(ArbModpolM2) / 1e6 # 7069824
# intersection between them
AAM2 <- raster::intersect(ArbPolyM2,ArbModpolM2)
areaPolygon(AAM2) / 1e6 # 783495.5
# percent of the polygon explained by the niche
(783495.5/4598454)*100 # 17.03

###
# terr poly intersect with terr niche
# TerrPolyM2 area
areaPolygon(TerrPolyM2) / 1e6 # 586519.8
# TerrModpolM1 area
areaPolygon(TerrModpolM2) / 1e6 # 2891744
# intersection between them
TTM2 <- raster::intersect(TerrPolyM2,TerrModpolM2)
areaPolygon(TTM2) / 1e6 # 388261.9
# percent of the polygon explained by the niche
(388261.9/586519.8)*100 # 66.2

###
# water poly intersect with water niche
# AquaPolyM2 area
areaPolygon(AquaPolyM2) / 1e6 # 2191861
# AquaModpolM2 area
areaPolygon(AquaModpolM2) / 1e6 # 2214916
# intersection between them
WWM2 <- raster::intersect(AquaPolyM2,AquaModpolM2)
areaPolygon(WWM2) / 1e6 # 1735944
# percent of the polygon explained by the niche
(1735944/2829823)*100 # 61.3

###
# cave poly intersect with cave niche
# CavePolyM2 area
areaPolygon(CavePolyM2) / 1e6 # 484303.2
# CaveModpolM2 area
areaPolygon(CaveModpolM2) / 1e6 # 1405879
# intersection between them
CCM2 <- raster::intersect(CavePolyM2,CaveModpolM2)
areaPolygon(CCM2) / 1e6 # 441455.9
# percent of the polygon explained by the niche
(441455.9/484303.2)*100 # 91.1

###
# foss poly intersect with foss niche
# FossPolyM2 area
areaPolygon(FossPolyM2) / 1e6 # 2178794
# FossModpolM2 area
areaPolygon(FossModpolM2) / 1e6 # 3757558
# intersection between them
FFM2 <- raster::intersect(FossPolyM2,FossModpolM2)
areaPolygon(FFM2) / 1e6 # 1972081
# percent of the polygon explained by the niche
(1972081/2178794)*100 # 90.5

###
# sax poly intersect with sax niche
# SaxPolyM2 area
areaPolygon(SaxPolyM2) / 1e6 # 486247.8
# SaxModpolM2 area
areaPolygon(SaxModpolM2) / 1e6 # 1615342
# intersection between them
SSM2 <- raster::intersect(SaxPolyM2,SaxModpolM2)
areaPolygon(SSM2) / 1e6 # 431468.1
# percent of the polygon explained by the niche
(431468.1/486247.8)*100 # 88.7

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 7069824
areaPolygon(TerrModpolM2) / 1e6 # 2891744
#inter bet Arb and Terr at 0.5 cutoff
interATM2N <- raster::intersect(ArbModpolM2, TerrModpolM2)
areaPolygon(interATM2N) / 1e6 # 2316881
# percent of each niche that overlaps with the other by area
# inter/arb niche
(2316881/7069824)*100 # 32.7
# inter/terr niche
(2316881/2891744)*100 # 80.1

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 7069824
areaPolygon(AquaModpolM2) / 1e6 # 2214916
#inter bet Arb and water at 0.5 cutoff
interAWM2N <- raster::intersect(ArbModpolM2, AquaModpolM2)
areaPolygon(interAWM2N) / 1e6 # 2170841
# percent of each niche that overlaps with the other by area
# inter/arb niche
(2170841/7069824)*100 # 30.7
# inter/water niche
(2170841/2214916)*100 # 98.01

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 7069824
areaPolygon(CaveModpolM2) / 1e6 # 1405879
#inter bet Arb and cave at 0.5 cutoff
interACM2N <- raster::intersect(ArbModpolM2, CaveModpolM2)
areaPolygon(interACM2N) / 1e6 # 1398388
# percent of each niche that overlaps with the other by area
# inter/arb niche
(1398388/7069824)*100 # 19.8
# inter/cave niche
(1398388/1405879)*100 # 99.5

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 7069824
areaPolygon(FossModpolM2) / 1e6 # 3757558
#inter bet Arb and foss at 0.5 cutoff
interAFM2N <- raster::intersect(ArbModpolM2, FossModpolM2)
areaPolygon(interAFM2N) / 1e6 # 3621071
# percent of each niche that overlaps with the other by area
# inter/arb niche
(3621071/7069824)*100 # 51.2
# inter/foss niche
(3621071/3757558)*100 # 96.4

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 7069824
areaPolygon(SaxModpolM2) / 1e6 # 1615342
#inter bet Arb and sax at 0.5 cutoff
interASM2N <- raster::intersect(ArbModpolM2, SaxModpolM2)
areaPolygon(interASM2N) / 1e6 # 1567445
# percent of each niche that overlaps with the other by area
# inter/arb niche
(1567445/7069824)*100 # 22.2
# inter/sax niche
(1567445/1615342)*100 # 97.03

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 2891744
areaPolygon(AquaModpolM2) / 1e6 # 2214916
#inter bet Terr and Water at 0.5 cutoff
interTWM2N <- raster::intersect(TerrModpolM2, AquaModpolM2)
areaPolygon(interTWM2N) / 1e6 # 834921.5
# percent of each niche that overlaps with the other by area
# inter/terr niche
(834921.5/2891744)*100 # 28.9
# inter/water niche
(834921.5/2214916)*100 # 37.7

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 2891744
areaPolygon(CaveModpolM2) / 1e6 # 1405879
#inter bet Terr and Cave at 0.5 cutoff
interTCM2N <- raster::intersect(TerrModpolM2, CaveModpolM2)
areaPolygon(interTCM2N) / 1e6 # 630460.9
# percent of each niche that overlaps with the other by area
# inter/terr niche
(630460.9/2891744)*100 # 21.8
# inter/cave niche
(630460.9/1405879)*100 # 44.8

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 2891744
areaPolygon(FossModpolM2) / 1e6 # 3757558
#inter bet Terr and Foss at 0.5 cutoff
interTFM2N <- raster::intersect(TerrModpolM2, FossModpolM2)
areaPolygon(interTFM2N) / 1e6 # 992514.3
# percent of each niche that overlaps with the other by area
# inter/terr niche
(992514.3/2891744)*100 # 34.3
# inter/foss niche
(992514.3/3757558)*100 # 26.4

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 2891744
areaPolygon(SaxModpolM2) / 1e6 # 1615342
#inter bet Terr and Sax at 0.5 cutoff
interTSM2N <- raster::intersect(TerrModpolM2, SaxModpolM2)
areaPolygon(interTSM2N) / 1e6 # 1076879
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1076879/2891744)*100 # 37.2
# inter/sax niche
(1076879/1615342)*100 # 66.7

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolM2) / 1e6 # 2214916
areaPolygon(CaveModpolM2) / 1e6 # 1405879
#inter bet Water and Cave at 0.5 cutoff
interWCM2N <- raster::intersect(AquaModpolM2, CaveModpolM2)
areaPolygon(interWCM2N) / 1e6 # 1220006
# percent of each niche that overlaps with the other by area
# inter/water niche
(1220006/2214916)*100 # 55.1
# inter/cave niche
(1220006/1405879)*100 # 86.8

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolM2) / 1e6 # 2214916
areaPolygon(FossModpolM2) / 1e6 # 3757558
#inter bet Water and Foss at 0.5 cutoff
interWFM2N <- raster::intersect(AquaModpolM2, FossModpolM2)
areaPolygon(interWFM2N) / 1e6 # 2117673
# percent of each niche that overlaps with the other by area
# inter/water niche
(2117673/2214916)*100 # 95.6
# inter/foss niche
(2117673/3757558)*100 # 56.3

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolM2) / 1e6 # 2214916
areaPolygon(SaxModpolM2) / 1e6 # 1615342
#inter bet Water and Sax at 0.5 cutoff
interWSM2N <- raster::intersect(AquaModpolM2, SaxModpolM2)
areaPolygon(interWSM2N) / 1e6 # 1291558
# percent of each niche that overlaps with the other by area
# inter/water niche
(1291558/2214916)*100 # 58.3
# inter/Sax niche
(1291558/1615342)*100 # 79.96

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolM2) / 1e6 # 1405879
areaPolygon(FossModpolM2) / 1e6 # 3757558
#inter bet cave and foss at 0.5 cutoff
interCFM2N <- raster::intersect(CaveModpolM2, FossModpolM2)
areaPolygon(interCFM2N) / 1e6 # 1337564
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1337564/1405879)*100 # 95.1
# inter/foss niche
(1337564/3757558)*100 # 35.6

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolM2) / 1e6 # 1405879
areaPolygon(SaxModpolM2) / 1e6 # 1615342
#inter bet cave and sax at 0.5 cutoff
interCSM2N <- raster::intersect(CaveModpolM2, SaxModpolM2)
areaPolygon(interCSM2N) / 1e6 # 875332.2
# percent of each niche that overlaps with the other by area
# inter/cave niche
(875332.2/1405879)*100 # 62.3
# inter/sax niche
(875332.2/1615342)*100 # 54.2

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolM2) / 1e6 # 3757558
areaPolygon(SaxModpolM2) / 1e6 # 1615342
#inter bet foss and sax at 0.5 cutoff
interFSM2N <- raster::intersect(FossModpolM2, SaxModpolM2)
areaPolygon(interFSM2N) / 1e6 # 1283081
# percent of each niche that overlaps with the other by area
# inter/foss niche
(1283081/3757558)*100 # 34.14
# inter/sax niche
(1283081/1615342)*100 # 79.4


#######################################
#### distribution inter with niche ###
#######################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolL) / 1e6 # 5408492
interADTL <- raster::intersect(ArbPolyL, TerrModpolL)
areaPolygon(interADTL) / 1e6 # 253027.7
(253027.7/5408492)*100 # 4.7
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1979474
interADCL <- raster::intersect(ArbPolyL, CaveModpolL)
areaPolygon(interADCL) / 1e6 # 14753.05
(14753.05/1979474)*100 # 0.7453015
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2926900
interADSL <- raster::intersect(ArbPolyL, SaxModpolL)
areaPolygon(interADSL) / 1e6 # 108980.4
(108980.4/2926900)*100 # 3.723407
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolL) / 1e6 # 3652572
interADWL <- raster::intersect(ArbPolyL, AquaModpolL)
areaPolygon(interADWL) / 1e6 # 6764.454
(6764.454/3652572)*100 #0.185197
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 4037977
interADFL <- raster::intersect(ArbPolyL, FossModpolL)
areaPolygon(interADFL) / 1e6 # 193791
(193791/4037977)*100 # 4.79921
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolL) / 1e6 # 2190731
interTDAL <- raster::intersect(TerrPolyL, ArbModpolL)
areaPolygon(interTDAL) / 1e6 # 559046.2
(559046.2/2190731)*100 # 25.51871
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1979474
interTDCL <- raster::intersect(TerrPolyL, CaveModpolL)
areaPolygon(interTDCL) / 1e6 # 902565
(902565/1979474)*100 # 45.5962
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2926900
interTDSL <- raster::intersect(TerrPolyL, SaxModpolL)
areaPolygon(interTDSL) / 1e6 # 1114785
(1114785/2926900)*100 #  38.08757
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolL) / 1e6 # 3652572
interTDWL <- raster::intersect(TerrPolyL, AquaModpolL)
areaPolygon(interTDWL) / 1e6 # 1778568
(1778568/3652572)*100 # 48.69358
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 4037977
interTDFL <- raster::intersect(TerrPolyL, FossModpolL)
areaPolygon(interTDFL) / 1e6 # 2006834
(2006834/4037977)*100 # 49.699
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 2190731
interWDAL <- raster::intersect(AquaPolyL, ArbModpolL)
areaPolygon(interWDAL) / 1e6 # 192616.1
(192616.1/2190731)*100 # 8.792321
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1979474
interWDCL <- raster::intersect(AquaPolyL, CaveModpolL)
areaPolygon(interWDCL) / 1e6 # 1610668
(1610668/1979474)*100 # 81.36848
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2926900
interWDSL <- raster::intersect(AquaPolyL, SaxModpolL)
areaPolygon(interWDSL) / 1e6 # 1821721
(1821721/2926900)*100 # 62.24063
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolL) / 1e6 # 5408492
interWDTL <- raster::intersect(AquaPolyL, TerrModpolL)
areaPolygon(interWDTL) / 1e6 # 2400350
(2400350/5408492)*100 # 44.38113
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 4037977
interWDFL <- raster::intersect(AquaPolyL, FossModpolL)
areaPolygon(interWDFL) / 1e6 # 2282488
(2282488/4037977)*100 # 56.52553
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 2190731
interCDAL <- raster::intersect(CavePolyL, ArbModpolL)
areaPolygon(interCDAL) / 1e6 # 80505.6
(80505.6/2190731)*100 # 3.674828
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolL) / 1e6 # 3652572
interCDWL <- raster::intersect(CavePolyL, AquaModpolL)
areaPolygon(interCDWL) / 1e6 # 1075630
(1075630/3652572)*100 # 29.44856
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2926900
interCDSL <- raster::intersect(CavePolyL, SaxModpolL)
areaPolygon(interCDSL) / 1e6 # 1115486
(1115486/2926900)*100 # 38.11152
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolL) / 1e6 # 5408492
interCDTL <- raster::intersect(CavePolyL, TerrModpolL)
areaPolygon(interCDTL) / 1e6 # 1184639
(1184639/5408492)*100 # 21.90331
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolL) / 1e6 # 4037977
interCDFL <- raster::intersect(CavePolyL, FossModpolL)
areaPolygon(interCDFL) / 1e6 # 1218438
(1218438/4037977)*100 # 30.17447
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 2190731
interFDAL <- raster::intersect(FossPolyL, ArbModpolL)
areaPolygon(interFDAL) / 1e6 #  297467.2
( 297467.2/2190731)*100 # 13.57844
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolL) / 1e6 # 3652572
interFDWL <- raster::intersect(FossPolyL, AquaModpolL)
areaPolygon(interFDWL) / 1e6 # 1643511
(1643511/3652572)*100 # 44.99599
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2926900
interFDSL <- raster::intersect(FossPolyL, SaxModpolL)
areaPolygon(interFDSL) / 1e6 #1374328
(1374328/2926900)*100 # 46.95507
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolL) / 1e6 # 5408492
interFDTL <- raster::intersect(FossPolyL, TerrModpolL)
areaPolygon(interFDTL) / 1e6 # 1902620
(1902620/5408492)*100 # 35.17838
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1979474
interFDCL <- raster::intersect(FossPolyL, CaveModpolL)
areaPolygon(interFDCL) / 1e6 #1281696
(1281696/1979474)*100 # 64.74932
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 2190731
interSDAL <- raster::intersect(SaxPolyL, ArbModpolL)
areaPolygon(interSDAL) / 1e6 # 184501
(184501/2190731)*100 # 8.421892
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolL) / 1e6 # 3652572
interSDWL <- raster::intersect(SaxPolyL, AquaModpolL)
areaPolygon(interSDWL) / 1e6 # 1601007
(1601007/3652572)*100 # 43.83232
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 4037977
interSDFL <- raster::intersect(SaxPolyL, FossModpolL)
areaPolygon(interSDFL) / 1e6 # 1696941
(1696941/4037977)*100 # 42.02453
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolL) / 1e6 # 5408492
interSDTL <- raster::intersect(SaxPolyL, TerrModpolL)
areaPolygon(interSDTL) / 1e6 # 1632758
(1632758/5408492)*100 # 30.18878
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1979474
interSDCL <- raster::intersect(SaxPolyL, CaveModpolL)
areaPolygon(interSDCL) / 1e6 # 1504028
(1504028/1979474)*100 # 75.9812
# this is sax spp present where cave can live

##############################################################################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 6508263
interADTM1 <- raster::intersect(ArbPolyM1, TerrModpolM1)
areaPolygon(interADTM1) / 1e6 # 254997.6
(254997.6/6508263)*100 # 3.918059
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 1317313
interADCM1 <- raster::intersect(ArbPolyM1, CaveModpolM1)
areaPolygon(interADCM1) / 1e6 # 21626.82
(21626.82/1317313)*100 # 1.641737
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 3031525
interADSM1 <- raster::intersect(ArbPolyM1, SaxModpolM1)
areaPolygon(interADSM1) / 1e6 # 146211.3
(146211.3/3031525)*100 # 4.823028
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3844713
interADWM1 <- raster::intersect(ArbPolyM1, AquaModpolM1)
# this is zero, they dont overlap
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 8001117
interADFM1 <- raster::intersect(ArbPolyM1, FossModpolM1)
areaPolygon(interADFM1) / 1e6 # 483649.5
(483649.5/8001117)*100 # 6.044775
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 2004242
interTDAM1 <- raster::intersect(TerrPolyM1, ArbModpolM1)
areaPolygon(interTDAM1) / 1e6 # 698608
(698608/2004242)*100 # 34.85647
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 1317313
interTDCM1 <- raster::intersect(TerrPolyM1, CaveModpolM1)
areaPolygon(interTDCM1) / 1e6 # 1198917
(1198917/1317313)*100 # 91.01231
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 3031525
interTDSM1 <- raster::intersect(TerrPolyM1, SaxModpolM1)
areaPolygon(interTDSM1) / 1e6 # 2190280
(2190280/3031525)*100 # 72.25011
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3844713
interTDWM1 <- raster::intersect(TerrPolyM1, AquaModpolM1)
areaPolygon(interTDWM1) / 1e6 # 2742525
(2742525/3844713)*100 # 71.33237
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 8001117
interTDFM1 <- raster::intersect(TerrPolyM1, FossModpolM1)
areaPolygon(interTDFM1) / 1e6 # 1188821
(1188821/8001117)*100 # 14.85819
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 2004242
interWDAM1 <- raster::intersect(AquaPolyM1, ArbModpolM1)
areaPolygon(interWDAM1) / 1e6 # 173803.6
(173803.6/2004242)*100 # 8.671787
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 1317313
interWDCM1 <- raster::intersect(AquaPolyM1, CaveModpolM1)
areaPolygon(interWDCM1) / 1e6 # 1064592
(1064592/1317313)*100 # 80.81542
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 3031525
interWDSM1 <- raster::intersect(AquaPolyM1, SaxModpolM1)
areaPolygon(interWDSM1) / 1e6 # 1768477
(1768477/3031525)*100 # 58.33622
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 6508263
interWDTM1 <- raster::intersect(AquaPolyM1, TerrModpolM1)
areaPolygon(interWDTM1) / 1e6 # 2691229
(2691229/6508263)*100 # 41.35096
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 8001117
interWDFM1 <- raster::intersect(AquaPolyM1, FossModpolM1)
areaPolygon(interWDFM1) / 1e6 # 529616.1
(529616.1/8001117)*100 # 6.619277
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 2004242
interCDAM1 <- raster::intersect(CavePolyM1, ArbModpolM1)
areaPolygon(interCDAM1) / 1e6 # 11266.7
(11266.7/2004242)*100 # 0.5621427
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3844713
interCDWM1 <- raster::intersect(CavePolyM1, AquaModpolM1)
areaPolygon(interCDWM1) / 1e6 # 414112.5
(414112.5/3844713)*100 # 10.77096
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 3031525
interCDSM1 <- raster::intersect(CavePolyM1, SaxModpolM1)
areaPolygon(interCDSM1) / 1e6 # 456968.9
(456968.9/3031525)*100 # 15.0739
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 6508263
interCDTM1 <- raster::intersect(CavePolyM1, TerrModpolM1)
areaPolygon(interCDTM1) / 1e6 # 459493.1
(459493.1/6508263)*100 # 7.06015
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolM1) / 1e6 # 8001117
interCDFM1 <- raster::intersect(CavePolyM1, FossModpolM1)
areaPolygon(interCDFM1) / 1e6 # 59040.59
(59040.59/8001117)*100 # 0.7379043
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 2004242
interFDAM1 <- raster::intersect(FossPolyM1, ArbModpolM1)
areaPolygon(interFDAM1) / 1e6 # 86378
(86378/2004242)*100 # 4.309759
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3844713
interFDWM1 <- raster::intersect(FossPolyM1, AquaModpolM1)
# this is zero, they do not intersect
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 3031525
interFDSM1 <- raster::intersect(FossPolyM1, SaxModpolM1)
areaPolygon(interFDSM1) / 1e6 # 132.4006
(132.4006/3031525)*100 # 0.004367459
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 6508263
interFDTM1 <- raster::intersect(FossPolyM1, TerrModpolM1)
areaPolygon(interFDTM1) / 1e6 # 8759.187
(8759.187/6508263)*100 # 0.1345856
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 1317313
interFDCM1 <- raster::intersect(FossPolyM1, CaveModpolM1)
# this is zero, they do not intersect
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 #  2004242
interSDAM1 <- raster::intersect(SaxPolyM1, ArbModpolM1)
areaPolygon(interSDAM1) / 1e6 # 16688.77
(16688.77/2004242)*100 # 0.8326724
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3844713
interSDWM1 <- raster::intersect(SaxPolyM1, AquaModpolM1)
areaPolygon(interSDWM1) / 1e6 #  125439.5
(125439.5/3844713)*100 # 3.262649
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 8001117
interSDFM1 <- raster::intersect(SaxPolyM1, FossModpolM1)
areaPolygon(interSDFM1) / 1e6 # 48538.48
(48538.48/8001117)*100 # 0.6066463
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 6508263
interSDTM1 <- raster::intersect(SaxPolyM1, TerrModpolM1)
areaPolygon(interSDTM1) / 1e6 # 178898.1
(178898.1/6508263)*100 # 2.748784
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 1317313
interSDCM1 <- raster::intersect(SaxPolyM1, CaveModpolM1)
areaPolygon(interSDCM1) / 1e6 # 148092.4
(148092.4/1317313)*100 # 11.24201
# this is sax spp present where cave can live

##############################################################################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolM2) / 1e6 # 2891744
interADTM2 <- raster::intersect(ArbPolyM2, TerrModpolM2)
areaPolygon(interADTM2) / 1e6 # 1311364
(1311364/2891744)*100 # 45.34855
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 1405879
interADCM2 <- raster::intersect(ArbPolyM2, CaveModpolM2)
areaPolygon(interADCM2) / 1e6 # 1231383
(1231383/1405879)*100 # 87.58812
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 1615342
interADSM2 <- raster::intersect(ArbPolyM2, SaxModpolM2)
areaPolygon(interADSM2) / 1e6 # 1278082
(1278082/1615342)*100 # 79.12145
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2214916
interADWM2 <- raster::intersect(ArbPolyM2, AquaModpolM2)
areaPolygon(interADWM2) / 1e6 # 1806221
(1806221/2214916)*100 # 81.54806
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3757558
interADFM2 <- raster::intersect(ArbPolyM2, FossModpolM2)
areaPolygon(interADFM2) / 1e6 # 2669063
(2669063/3757558)*100 # 71.03185
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 7069824
interTDAM2 <- raster::intersect(TerrPolyM2, ArbModpolM2)
areaPolygon(interTDAM2) / 1e6 # 456202.4
(456202.4/7069824)*100 # 6.452811
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 1405879
interTDCM2 <- raster::intersect(TerrPolyM2, CaveModpolM2)
areaPolygon(interTDCM2) / 1e6 # 77345.76
(77345.76/1405879)*100 # 5.501594
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 1615342
interTDSM2 <- raster::intersect(TerrPolyM2, SaxModpolM2)
areaPolygon(interTDSM2) / 1e6 # 244599.7
(244599.7/1615342)*100 # 15.14229
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2214916
interTDWM2 <- raster::intersect(TerrPolyM2, AquaModpolM2)
areaPolygon(interTDWM2) / 1e6 # 205186.5
(205186.5/2214916)*100 # 9.26385
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3757558
interTDFM2 <- raster::intersect(TerrPolyM2, FossModpolM2)
areaPolygon(interTDFM2) / 1e6 # 321956.1
(321956.1/3757558)*100 # 8.568227
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 7069824
interWDAM2 <- raster::intersect(AquaPolyM2, ArbModpolM2)
areaPolygon(interWDAM2) / 1e6 # 2076688
(2076688/7069824)*100 # 29.37397
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 1405879
interWDCM2 <- raster::intersect(AquaPolyM2, CaveModpolM2)
areaPolygon(interWDCM2) / 1e6 # 1052952
(1052952/1405879)*100 # 74.89635
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 1615342
interWDSM2 <- raster::intersect(AquaPolyM2, SaxModpolM2)
areaPolygon(interWDSM2) / 1e6 # 1201398
(1201398/1615342)*100 # 74.37422
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolM2) / 1e6 # 2891744
interWDTM2 <- raster::intersect(AquaPolyM2, TerrModpolM2)
areaPolygon(interWDTM2) / 1e6 # 776202.9
(776202.9/2891744)*100 # 26.84203
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3757558
interWDFM2 <- raster::intersect(AquaPolyM2, FossModpolM2)
areaPolygon(interWDFM2) / 1e6 # 2012075
(2012075/3757558)*100 # 53.54741
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 7069824
interCDAM2 <- raster::intersect(CavePolyM2, ArbModpolM2)
areaPolygon(interCDAM2) / 1e6 # 460583.2
(460583.2/7069824)*100 # 6.514776
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2214916
interCDWM2 <- raster::intersect(CavePolyM2, AquaModpolM2)
areaPolygon(interCDWM2) / 1e6 # 419199.5
(419199.5/2214916)*100 # 18.9262
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 1615342
interCDSM2 <- raster::intersect(CavePolyM2, SaxModpolM2)
areaPolygon(interCDSM2) / 1e6 # 280878.3
(280878.3/1615342)*100 # 17.38816
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolM2) / 1e6 # 2891744
interCDTM2 <- raster::intersect(CavePolyM2, TerrModpolM2)
areaPolygon(interCDTM2) / 1e6 # 229112.2
(229112.2/2891744)*100 #  7.922977
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3757558
interCDFM2 <- raster::intersect(CavePolyM2, FossModpolM2)
areaPolygon(interCDFM2) / 1e6 # 463073.3
(463073.3/3757558)*100 # 12.32378
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 7069824
interFDAM2 <- raster::intersect(FossPolyM2, ArbModpolM2)
areaPolygon(interFDAM2) / 1e6 # 1975428
(1975428/7069824)*100 # 27.94169
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2214916
interFDWM2 <- raster::intersect(FossPolyM2, AquaModpolM2)
areaPolygon(interFDWM2) / 1e6 #  1399241
(1399241/2214916)*100 # 63.17355
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 1615342
interFDSM2 <- raster::intersect(FossPolyM2, SaxModpolM2)
areaPolygon(interFDSM2) / 1e6 # 832857
(832857/1615342)*100 # 51.55917
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolM2) / 1e6 #  2891744
interFDTM2 <- raster::intersect(FossPolyM2, TerrModpolM2)
areaPolygon(interFDTM2) / 1e6 # 523687.5
(523687.5/2891744)*100 # 18.10975
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 1405879
interFDCM2 <- raster::intersect(FossPolyM2, CaveModpolM2)
areaPolygon(interFDCM2) / 1e6 # 894300.6
(894300.6/1405879)*100 # 63.61149
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 #  7069824
interSDAM2 <- raster::intersect(SaxPolyM2, ArbModpolM2)
areaPolygon(interSDAM2) / 1e6 # 472748.7
(472748.7/7069824)*100 # 6.686852
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2214916
interSDWM2 <- raster::intersect(SaxPolyM2, AquaModpolM2)
areaPolygon(interSDWM2) / 1e6 # 407093.7
(407093.7/2214916)*100 # 18.37965
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3757558
interSDFM2 <- raster::intersect(SaxPolyM2, FossModpolM2)
areaPolygon(interSDFM2) / 1e6 # 435987.3
(435987.3/3757558)*100 # 11.60294
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolM2) / 1e6 # 2891744
interSDTM2 <- raster::intersect(SaxPolyM2, TerrModpolM2)
areaPolygon(interSDTM2) / 1e6 # 238153.2
(238153.2/2891744)*100 # 8.235625
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 1405879
interSDCM2 <- raster::intersect(SaxPolyM2, CaveModpolM2)
areaPolygon(interSDCM2) / 1e6 # 338502
(338502/1405879)*100 # 24.07761
# this is sax spp present where cave can live

##################
# niche equivalency
##################


# climate data
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogCGTiff.gri')
# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors.crop <- crop(x = ClimateData, y = geographic.extent)
predictors <- predictors.crop

# get the points from the Maxent_strict script and make sure its the *New versions

# takes a shit ton of time and crashes computer :(
dismo::nicheEquivalency(sp1=ArbNew, sp2=TerrNew, predictors = predictors,
                        n=2, model=maxent, verbose=T)

# also note there is another niche.equivalency in ENM with breadth but i cant load it 
# also in phyloclim that requires one file of occ points and not defined two

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
library(rgdal); library(rgeos); library(dismo)

# load the maxent predictions
ArbModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/ArbMod_prediction_lenient.grd")
TerrModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/TerrMod_prediction_lenient.grd")
AquaModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/AquaMod_prediction_lenient.grd")
CaveModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/CaveMod_prediction_lenient.grd")
FossModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/FossMod_prediction_lenient.grd")
SaxModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/SaxMod_prediction_lenient.grd")
ArbModM1 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/ArbMod_prediction_M1.grd")
TerrModM1 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/TerrMod_prediction_M1.grd")
AquaModM1 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/AquaMod_prediction_M1.grd")
CaveModM1 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/CaveMod_prediction_M1.grd")
FossModM1 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/FossMod_prediction_M1.grd")
SaxModM1 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/SaxMod_prediction_M1.grd")
ArbModM2 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/ArbMod_prediction_M2.grd")
TerrModM2 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/TerrMod_prediction_M2.grd")
AquaModM2 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/AquaMod_prediction_M2.grd")
CaveModM2 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/CaveMod_prediction_M2.grd")
FossModM2 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/FossMod_prediction_M2.grd")
SaxModM2 <- raster("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Predictions/SaxMod_prediction_M2.grd")

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

# get the area of the niche polygon at a 0.5 cutoff scale
areaPolygon(ArbModpolL) / 1e6
# 1285231
areaPolygon(TerrModpolL) / 1e6
# 4485079
areaPolygon(AquaModpolL) / 1e6
# 3177861
areaPolygon(CaveModpolL) / 1e6
# 1450650
areaPolygon(FossModpolL) / 1e6
# 3432082
areaPolygon(SaxModpolL) / 1e6
# 2259151

areaPolygon(ArbModpolM1) / 1e6
# 1238624
areaPolygon(TerrModpolM1) / 1e6
# 5034443
areaPolygon(AquaModpolM1) / 1e6
# 3185717
areaPolygon(CaveModpolM1) / 1e6
# 568391.4
areaPolygon(FossModpolM1) / 1e6
# 220695.9
areaPolygon(SaxModpolM1) / 1e6
# 358996.1 

areaPolygon(ArbModpolM2) / 1e6
# 5422425
areaPolygon(TerrModpolM2) / 1e6
# 1210240
areaPolygon(AquaModpolM2) / 1e6
# 2438405 
areaPolygon(CaveModpolM2) / 1e6
# 619542
areaPolygon(FossModpolM2) / 1e6
# 3000761
areaPolygon(SaxModpolM2) / 1e6
# 744082.7

#############################################################################################

# 10/20/19
##############################################################
# lenient reciprocal suitability

# How much habitat that terrestrial species live in is suitable for arboreal life 0.5 cut off arboreal niche
areaPolygon(ArbModpolL) / 1e6 #  1285231
areaPolygon(TerrPolyL) / 1e6 # 3138294
intersectionTA <- raster::intersect(ArbModpolL, TerrPolyL)
areaPolygon(intersectionTA) / 1e6 # 402392.4
# divide the intersection by the terrestrial polygon area
(402392.4/3138294)*100 # 12.82201

# How much habitat that arboreal species live in is suitable for terrestrial life 0.5 cut off terrestrial niche
areaPolygon(TerrModpolL) / 1e6 #  4485079
areaPolygon(ArbPolyL) / 1e6 # 675927
intersectionAT <- raster::intersect(TerrModpolL, ArbPolyL)
areaPolygon(intersectionAT) / 1e6 # 228119.1
# divide the intersection by the arboreal polygon area
(228119.1/675927)*100 # 33.74907

# arb polygon and arb niche 0.5 cut off arb niche
areaPolygon(ArbModpolL) / 1e6 # 1285231
areaPolygon(ArbPolyL) / 1e6 # 675927
intersectionAA <- raster::intersect(ArbModpolL, ArbPolyL)
areaPolygon(intersectionAA) / 1e6 # 500333.2
# divide the intersection by the arboreal polygon area
(500333.2/675927)*100 # 74.02178

# terr polygon and terr niche 0.5 cut off terr niche
areaPolygon(TerrModpolL) / 1e6 #  4485079
# terrestrial polygon
areaPolygon(TerrPolyL) / 1e6 # 3138294
intersectionTT <- raster::intersect(TerrModpolL, TerrPolyL)
areaPolygon(intersectionTT) / 1e6 # 2654509
# divide the intersection by the terr polygon area
(2654509/3138294)*100 # 84.58446

##############################################################
# M1 reciprocal suitability

# How much habitat that terrestrial species live in is suitable for arboreal life 0.5 cut off arboreal niche
areaPolygon(ArbModpolM1) / 1e6 #  1238624
areaPolygon(TerrPolyM1) / 1e6 # 4456354
intersectionTA <- raster::intersect(ArbModpolM1, TerrPolyM1)
areaPolygon(intersectionTA) / 1e6 # 523088.6
# divide the intersection by the terrestrial polygon area
(523088.6/4456354)*100 # 11.73804

# How much habitat that arboreal species live in is suitable for terrestrial life 0.5 cut off terrestrial niche
areaPolygon(TerrModpolM1) / 1e6 #  5034443
areaPolygon(ArbPolyM1) / 1e6 # 659573
intersectionAT <- raster::intersect(TerrModpolM1, ArbPolyM1)
areaPolygon(intersectionAT) / 1e6 # 212216.4
# divide the intersection by the arboreal polygon area
(212216.4/659573)*100 # 32.17482

# arb polygon and arb niche 0.5 cut off arb niche
areaPolygon(ArbModpolM1) / 1e6 #  1238624
# arboreal polygon
areaPolygon(ArbPolyM1) / 1e6 # 659573
intersectionAA <- raster::intersect(ArbModpolM1, ArbPolyM1)
areaPolygon(intersectionAA) / 1e6 # 479367.3
# divide the intersection by the arboreal polygon area
(479367.3/659573)*100 # 72.67843

# terr polygon and terr niche 0.5 cut off terr niche
areaPolygon(TerrModpolM1) / 1e6 #  5034443
# terrestrial polygon
areaPolygon(TerrPolyM1) / 1e6 # 4456354
intersectionTT <- raster::intersect(TerrModpolM1, TerrPolyM1)
areaPolygon(intersectionTT) / 1e6 # 3740585
# divide the intersection by the terr polygon area
(3740585/4456354)*100 # 83.93824

##############################################################
# M2 reciprocal suitability

# How much habitat that terrestrial species live in is suitable for arboreal life 0.5 cut off arboreal niche
areaPolygon(ArbModpolM2) / 1e6 #  5422425
areaPolygon(TerrPolyM2) / 1e6 # 586519.8
intersectionTA <- raster::intersect(ArbModpolM2, TerrPolyM2)
areaPolygon(intersectionTA) / 1e6 # 445856.3
# divide the intersection by the terrestrial polygon area
(445856.3/586519.8)*100 # 76.01726

# How much habitat that arboreal species live in is suitable for terrestrial life 0.5 cut off terrestrial niche
areaPolygon(TerrModpolM2) / 1e6 #  1210240
areaPolygon(ArbPolyM2) / 1e6 # 4598454
intersectionAT <- raster::intersect(TerrModpolM2, ArbPolyM2)
areaPolygon(intersectionAT) / 1e6 # 860980.4
# divide the intersection by the arboreal polygon area
(860980.4/4598454)*100 # 18.72326

# arb polygon and arb niche 0.5 cut off arb niche
areaPolygon(ArbModpolM2) / 1e6 #  5422425
areaPolygon(ArbPolyM2) / 1e6 #  4598454
intersectionAA <- raster::intersect(ArbModpolM2, ArbPolyM2)
areaPolygon(intersectionAA) / 1e6 # 569226
# divide the intersection by the arboreal polygon area
(569226/4598454)*100 # 12.37864 -- yikes

# terr polygon and terr niche 0.5 cut off terr niche
areaPolygon(TerrModpolM2) / 1e6 #  1210240
areaPolygon(TerrPolyM2) / 1e6 # 586519.8
intersectionTT <- raster::intersect(TerrModpolM2, TerrPolyM2)
areaPolygon(intersectionTT) / 1e6 # 442997
# divide the intersection by the terr polygon area
(442997/586519.8)*100 # 75.52976

#############################################################################################

# niche overlap 
# arb and terr
dismo::nicheOverlap(ArbModLSS, TerrModLSS, stat='I', mask=T, checkNegatives = T)
# 0.1955743
dismo::nicheOverlap(ArbModLSS, TerrModLSS, stat='D', mask=T, checkNegatives = T)
# 0.0983781
dismo::nicheOverlap(ArbModL, TerrModL, stat='I', mask=T, checkNegatives = T)
# 0.608951
dismo::nicheOverlap(ArbModL, TerrModL, stat='D', mask=T, checkNegatives = T)
# 0.3390061

# arb and water
dismo::nicheOverlap(ArbModLSS, AquaModLSS, stat='I', mask=T, checkNegatives = T)
# 0.0008268609
dismo::nicheOverlap(ArbModLSS, AquaModLSS, stat='D', mask=T, checkNegatives = T)
# 0.0004979821
dismo::nicheOverlap(ArbModL, AquaModL, stat='I', mask=T, checkNegatives = T)
# 0.3645555
dismo::nicheOverlap(ArbModL, AquaModL, stat='D', mask=T, checkNegatives = T)
# 0.151287

# arb and cave
dismo::nicheOverlap(ArbModLSS, CaveModLSS, stat='I', mask=T, checkNegatives = T)
# 0.0002856979
dismo::nicheOverlap(ArbModLSS, CaveModLSS, stat='D', mask=T, checkNegatives = T)
# 0.0002594248
dismo::nicheOverlap(ArbModL, CaveModL, stat='I', mask=T, checkNegatives = T)
# 0.3462906
dismo::nicheOverlap(ArbModL, CaveModL, stat='D', mask=T, checkNegatives = T)
# 0.1188417

# arb and foss
dismo::nicheOverlap(ArbModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.06293269
dismo::nicheOverlap(ArbModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.03666998
dismo::nicheOverlap(ArbModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.6032334
dismo::nicheOverlap(ArbModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.309408

# arb and sax
dismo::nicheOverlap(ArbModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.009301952
dismo::nicheOverlap(ArbModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.006813264
dismo::nicheOverlap(ArbModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.4633461
dismo::nicheOverlap(ArbModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.1974114

# terr and water
dismo::nicheOverlap(TerrModLSS, AquaModLSS, stat='I', mask=T, checkNegatives = T)
# 0.711416
dismo::nicheOverlap(TerrModLSS, AquaModLSS, stat='D', mask=T, checkNegatives = T)
# 0.5941949
dismo::nicheOverlap(TerrModL, AquaModL, stat='I', mask=T, checkNegatives = T)
# 0.8202297
dismo::nicheOverlap(TerrModL, AquaModL, stat='D', mask=T, checkNegatives = T)
# 0.5942015

# terr and cave
dismo::nicheOverlap(TerrModLSS, CaveModLSS, stat='I', mask=T, checkNegatives = T)
# 0.5441924
dismo::nicheOverlap(TerrModLSS, CaveModLSS, stat='D', mask=T, checkNegatives = T)
# 0.3014634
dismo::nicheOverlap(TerrModL, CaveModL, stat='I', mask=T, checkNegatives = T)
# 0.6751833
dismo::nicheOverlap(TerrModL, CaveModL, stat='D', mask=T, checkNegatives = T)
# 0.3597413

# terr and foos
dismo::nicheOverlap(TerrModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.7730273
dismo::nicheOverlap(TerrModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6673397
dismo::nicheOverlap(TerrModL, FossModL, stat='I', mask=T, checkNegatives = T)
#  0.8859938
dismo::nicheOverlap(TerrModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.6629953

# terr and sax
dismo::nicheOverlap(TerrModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.5979951
dismo::nicheOverlap(TerrModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.4106795
dismo::nicheOverlap(TerrModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.7784816
dismo::nicheOverlap(TerrModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.46822

# water and cave
dismo::nicheOverlap(AquaModLSS, CaveModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6514005
dismo::nicheOverlap(AquaModLSS, CaveModLSS, stat='D', mask=T, checkNegatives = T)
# 0.432041
dismo::nicheOverlap(AquaModL, CaveModL, stat='I', mask=T, checkNegatives = T)
# 0.8161407
dismo::nicheOverlap(AquaModL, CaveModL, stat='D', mask=T, checkNegatives = T)
# 0.5568423

# water and foss
dismo::nicheOverlap(AquaModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.721857
dismo::nicheOverlap(AquaModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6984001
dismo::nicheOverlap(AquaModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.8341135
dismo::nicheOverlap(AquaModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.6630022

# water and sax
dismo::nicheOverlap(AquaModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.774839
dismo::nicheOverlap(AquaModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6371059
dismo::nicheOverlap(AquaModL, SaxModL, stat='I', mask=T, checkNegatives = T)
#  0.8214744
dismo::nicheOverlap(AquaModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.6282889

# cave and foss
dismo::nicheOverlap(CaveModLSS, FossModLSS, stat='I', mask=T, checkNegatives = T)
# 0.637346
dismo::nicheOverlap(CaveModLSS, FossModLSS, stat='D', mask=T, checkNegatives = T)
# 0.408983
dismo::nicheOverlap(CaveModL, FossModL, stat='I', mask=T, checkNegatives = T)
# 0.7844299
dismo::nicheOverlap(CaveModL, FossModL, stat='D', mask=T, checkNegatives = T)
# 0.5057985

# cave and sax
dismo::nicheOverlap(CaveModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.8008988
dismo::nicheOverlap(CaveModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6460325
dismo::nicheOverlap(CaveModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.9267335
dismo::nicheOverlap(CaveModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.7595158

# foss and sax
dismo::nicheOverlap(FossModLSS, SaxModLSS, stat='I', mask=T, checkNegatives = T)
# 0.7170482
dismo::nicheOverlap(FossModLSS, SaxModLSS, stat='D', mask=T, checkNegatives = T)
# 0.570429
dismo::nicheOverlap(FossModL, SaxModL, stat='I', mask=T, checkNegatives = T)
# 0.8204486
dismo::nicheOverlap(FossModL, SaxModL, stat='D', mask=T, checkNegatives = T)
# 0.5793485

######################################################################################

# arb and terr
dismo::nicheOverlap(ArbModM1SS, TerrModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.178439
dismo::nicheOverlap(ArbModM1SS, TerrModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.08370851
dismo::nicheOverlap(ArbModM1, TerrModM1, stat='I', mask=T, checkNegatives = T)
# 0.5557399
dismo::nicheOverlap(ArbModM1, TerrModM1, stat='D', mask=T, checkNegatives = T)
# 0.2916225

# arb and water
dismo::nicheOverlap(ArbModM1SS, AquaModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.0009891281
dismo::nicheOverlap(ArbModM1SS, AquaModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.0005853103
dismo::nicheOverlap(ArbModM1, AquaModM1, stat='I', mask=T, checkNegatives = T)
# 0.2382702
dismo::nicheOverlap(ArbModM1, AquaModM1, stat='D', mask=T, checkNegatives = T)
# 0.090378

# arb and cave
dismo::nicheOverlap(ArbModM1SS, CaveModM1SS, stat='I', mask=T, checkNegatives = T)
# -4.440892e-16 - ie 0
dismo::nicheOverlap(ArbModM1SS, CaveModM1SS, stat='D', mask=T, checkNegatives = T)
# -4.440892e-16 - ie 0
dismo::nicheOverlap(ArbModM1, CaveModM1, stat='I', mask=T, checkNegatives = T)
# 0.1799592
dismo::nicheOverlap(ArbModM1, CaveModM1, stat='D', mask=T, checkNegatives = T)
# 0.05310483

# arb and foss
dismo::nicheOverlap(ArbModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.2638333
dismo::nicheOverlap(ArbModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1075708
dismo::nicheOverlap(ArbModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.7133249
dismo::nicheOverlap(ArbModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.4100377

# arb and sax
dismo::nicheOverlap(ArbModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.0005073488
dismo::nicheOverlap(ArbModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.0002810568
dismo::nicheOverlap(ArbModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.2482679
dismo::nicheOverlap(ArbModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.09386756

# terr and water
dismo::nicheOverlap(TerrModM1SS, AquaModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.7272885
dismo::nicheOverlap(TerrModM1SS, AquaModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.5765711
dismo::nicheOverlap(TerrModM1, AquaModM1, stat='I', mask=T, checkNegatives = T)
# 0.8088024
dismo::nicheOverlap(TerrModM1, AquaModM1, stat='D', mask=T, checkNegatives = T)
# 0.5676178

# terr and cave
dismo::nicheOverlap(TerrModM1SS, CaveModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.3283165
dismo::nicheOverlap(TerrModM1SS, CaveModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1077917
dismo::nicheOverlap(TerrModM1, CaveModM1, stat='I', mask=T, checkNegatives = T)
# 0.5550476
dismo::nicheOverlap(TerrModM1, CaveModM1, stat='D', mask=T, checkNegatives = T)
# 0.2500686

# terr and foos
dismo::nicheOverlap(TerrModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.02658499
dismo::nicheOverlap(TerrModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.005084884
dismo::nicheOverlap(TerrModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.3997504
dismo::nicheOverlap(TerrModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.1585387

# terr and sax
dismo::nicheOverlap(TerrModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.2594507
dismo::nicheOverlap(TerrModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.06742517
dismo::nicheOverlap(TerrModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.5872771
dismo::nicheOverlap(TerrModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.2899973

# water and cave
dismo::nicheOverlap(AquaModM1SS, CaveModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.3987055
dismo::nicheOverlap(AquaModM1SS, CaveModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1651197
dismo::nicheOverlap(AquaModM1, CaveModM1, stat='I', mask=T, checkNegatives = T)
# 0.6534194
dismo::nicheOverlap(AquaModM1, CaveModM1, stat='D', mask=T, checkNegatives = T)
# 0.3565309

# water and foss
dismo::nicheOverlap(AquaModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# -2.220446e-16 - ie 0
dismo::nicheOverlap(AquaModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# -2.220446e-16 - ie 0
dismo::nicheOverlap(AquaModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.160487
dismo::nicheOverlap(AquaModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
# 0.04351328

# water and sax
dismo::nicheOverlap(AquaModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.3218205
dismo::nicheOverlap(AquaModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.1054957
dismo::nicheOverlap(AquaModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.6435325
dismo::nicheOverlap(AquaModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.3638205

# cave and foss
dismo::nicheOverlap(CaveModM1SS, FossModM1SS, stat='I', mask=T, checkNegatives = T)
# 2.220446e-16 - ie 0
dismo::nicheOverlap(CaveModM1SS, FossModM1SS, stat='D', mask=T, checkNegatives = T)
# 2.220446e-16 - ie 0
dismo::nicheOverlap(CaveModM1, FossModM1, stat='I', mask=T, checkNegatives = T)
# 0.09904485
dismo::nicheOverlap(CaveModM1, FossModM1, stat='D', mask=T, checkNegatives = T)
#  0.02188748

# cave and sax
dismo::nicheOverlap(CaveModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0.5959274
dismo::nicheOverlap(CaveModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0.4717021
dismo::nicheOverlap(CaveModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.8865106
dismo::nicheOverlap(CaveModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.6565927

# foss and sax
dismo::nicheOverlap(FossModM1SS, SaxModM1SS, stat='I', mask=T, checkNegatives = T)
# 0
dismo::nicheOverlap(FossModM1SS, SaxModM1SS, stat='D', mask=T, checkNegatives = T)
# 0
dismo::nicheOverlap(FossModM1, SaxModM1, stat='I', mask=T, checkNegatives = T)
# 0.1432894
dismo::nicheOverlap(FossModM1, SaxModM1, stat='D', mask=T, checkNegatives = T)
# 0.03728147

######################################################################################

# arb and terr
dismo::nicheOverlap(ArbModM2SS, TerrModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.374176
dismo::nicheOverlap(ArbModM2SS, TerrModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1736879
dismo::nicheOverlap(ArbModM2, TerrModM2, stat='I', mask=T, checkNegatives = T)
# 0.689022
dismo::nicheOverlap(ArbModM2, TerrModM2, stat='D', mask=T, checkNegatives = T)
# 0.4132892

# arb and water
dismo::nicheOverlap(ArbModM2SS, AquaModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.6420395
dismo::nicheOverlap(ArbModM2SS, AquaModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.4198469
dismo::nicheOverlap(ArbModM2, AquaModM2, stat='I', mask=T, checkNegatives = T)
# 0.7414803
dismo::nicheOverlap(ArbModM2, AquaModM2, stat='D', mask=T, checkNegatives = T)
# 0.4351758

# arb and cave
dismo::nicheOverlap(ArbModM2SS, CaveModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.3289353
dismo::nicheOverlap(ArbModM2SS, CaveModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1081984
dismo::nicheOverlap(ArbModM2, CaveModM2, stat='I', mask=T, checkNegatives = T)
# 0.5267262
dismo::nicheOverlap(ArbModM2, CaveModM2, stat='D', mask=T, checkNegatives = T)
# 0.2326008

# arb and foss
dismo::nicheOverlap(ArbModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.7193701
dismo::nicheOverlap(ArbModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
#  0.5274245
dismo::nicheOverlap(ArbModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
#  0.8423721
dismo::nicheOverlap(ArbModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
#  0.5641709

# arb and sax
dismo::nicheOverlap(ArbModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.3549937
dismo::nicheOverlap(ArbModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1265254
dismo::nicheOverlap(ArbModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.603678
dismo::nicheOverlap(ArbModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.3055357

# terr and water
dismo::nicheOverlap(TerrModM2SS, AquaModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.3384483
dismo::nicheOverlap(TerrModM2SS, AquaModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2402463
dismo::nicheOverlap(TerrModM2, AquaModM2, stat='I', mask=T, checkNegatives = T)
# 0.5834851
dismo::nicheOverlap(TerrModM2, AquaModM2, stat='D', mask=T, checkNegatives = T)
# 0.3980999

# terr and cave
dismo::nicheOverlap(TerrModM2SS, CaveModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.1442275
dismo::nicheOverlap(TerrModM2SS, CaveModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1022033
dismo::nicheOverlap(TerrModM2, CaveModM2, stat='I', mask=T, checkNegatives = T)
# 0.5027169
dismo::nicheOverlap(TerrModM2, CaveModM2, stat='D', mask=T, checkNegatives = T)
# 0.2210212

# terr and foos
dismo::nicheOverlap(TerrModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.2863212
dismo::nicheOverlap(TerrModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1812755
dismo::nicheOverlap(TerrModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.585446
dismo::nicheOverlap(TerrModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.3742807

# terr and sax
dismo::nicheOverlap(TerrModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.3900042
dismo::nicheOverlap(TerrModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2994558
dismo::nicheOverlap(TerrModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.7371388
dismo::nicheOverlap(TerrModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.4503192

# water and cave
dismo::nicheOverlap(AquaModM2SS, CaveModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.4625405
dismo::nicheOverlap(AquaModM2SS, CaveModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.232665
dismo::nicheOverlap(AquaModM2, CaveModM2, stat='I', mask=T, checkNegatives = T)
# 0.7392924
dismo::nicheOverlap(AquaModM2, CaveModM2, stat='D', mask=T, checkNegatives = T)
# 0.4413781

# water and foss
dismo::nicheOverlap(AquaModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.7945295
dismo::nicheOverlap(AquaModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.7086492
dismo::nicheOverlap(AquaModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.9101405
dismo::nicheOverlap(AquaModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.7310683

# water and sax
dismo::nicheOverlap(AquaModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.4999002
dismo::nicheOverlap(AquaModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2724652
dismo::nicheOverlap(AquaModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.7775118
dismo::nicheOverlap(AquaModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.5372352

# cave and foss
dismo::nicheOverlap(CaveModM2SS, FossModM2SS, stat='I', mask=T, checkNegatives = T)
#  0.4394674
dismo::nicheOverlap(CaveModM2SS, FossModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.1971647
dismo::nicheOverlap(CaveModM2, FossModM2, stat='I', mask=T, checkNegatives = T)
# 0.69817
dismo::nicheOverlap(CaveModM2, FossModM2, stat='D', mask=T, checkNegatives = T)
# 0.3889897

# cave and sax
dismo::nicheOverlap(CaveModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.2280512
dismo::nicheOverlap(CaveModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.2104679
dismo::nicheOverlap(CaveModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.7349768
dismo::nicheOverlap(CaveModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.4552424

# foss and sax
dismo::nicheOverlap(FossModM2SS, SaxModM2SS, stat='I', mask=T, checkNegatives = T)
# 0.4453843
dismo::nicheOverlap(FossModM2SS, SaxModM2SS, stat='D', mask=T, checkNegatives = T)
# 0.216513
dismo::nicheOverlap(FossModM2, SaxModM2, stat='I', mask=T, checkNegatives = T)
# 0.7069527
dismo::nicheOverlap(FossModM2, SaxModM2, stat='D', mask=T, checkNegatives = T)
# 0.4359512

###
# arb poly intersect with arb niche
# ArbPolyL area
areaPolygon(ArbPolyL) / 1e6 # 675927
# ArbModpolL area
areaPolygon(ArbModpolL) / 1e6 # 1285231
# intersection between them
AAL <- raster::intersect(ArbPolyL,ArbModpolL)
areaPolygon(AAL) / 1e6 # 500333.2
# percent of the polygon explained by the niche
(500333.2/675927)*100 # 74.02178

###
# terr poly intersect with terr niche
# TerrPolyL area
areaPolygon(TerrPolyL) / 1e6 # 3138294
# TerrModpolL area
areaPolygon(TerrModpolL) / 1e6 # 4485079
# intersection between them
TTL <- raster::intersect(TerrPolyL,TerrModpolL)
areaPolygon(TTL)/ 1e6 # 2654509
# percent of the polygon explained by the niche
(2654509/3138294)*100 # 84.58446

###
# water poly intersect with water niche
# AquaPolyL area
areaPolygon(AquaPolyL) / 1e6 # 2835207
AquaPolyL <- gBuffer(AquaPolyL, byid = T, width = 0)
# AquaModpolL area
areaPolygon(AquaModpolL) / 1e6 # 3177861
# intersection between them
WWL <- raster::intersect(AquaPolyL,AquaModpolL)
areaPolygon(WWL) / 1e6 # 2632277
# percent of the polygon explained by the niche
(2632277/2835207)*100 # 92.8425

###
# cave poly intersect with cave niche
# CavePolyL area
areaPolygon(CavePolyL) / 1e6 # 1253455
# CaveModpolL area
areaPolygon(CaveModpolL) / 1e6 # 1450650
# intersection between them
CCL <- raster::intersect(CavePolyL,CaveModpolL)
areaPolygon(CCL) / 1e6 # 1098905
# percent of the polygon explained by the niche
(1098905/1253455)*100 # 87.67008

###
# foss poly intersect with foss niche
# FossPolyL area
areaPolygon(FossPolyL) / 1e6 # 2373654
# FossModpolL area
areaPolygon(FossModpolL) / 1e6 # 3432082
# intersection between them
FFL <- raster::intersect(FossPolyL,FossModpolL)
areaPolygon(FFL) / 1e6 # 2087956
# percent of the polygon explained by the niche
(2087956/2373654)*100 # 87.96379

###
# sax poly intersect with sax niche
# SaxPolyL area
areaPolygon(SaxPolyL) / 1e6 # 1886932
# SaxModpolL area
areaPolygon(SaxModpolL) / 1e6 # 2259151
# intersection between them
SSL <- raster::intersect(SaxPolyL,SaxModpolL)
areaPolygon(SSL) / 1e6 #  1706255
# percent of the polygon explained by the niche
(1706255/1886932)*100 # 90.42483

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 1285231
areaPolygon(TerrModpolL) / 1e6 # 4485079
#inter bet Arb and Terr at 0.5 cutoff
interATLN <- raster::intersect(ArbModpolL, TerrModpolL)
areaPolygon(interATLN) / 1e6 # 436090.6
# percent of each niche that overlaps with the other by area
# inter/arb niche
(436090.6/1285231)*100 # 33.93
# inter/terr niche
(436090.6/4485079)*100 # 9.723

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 1285231
areaPolygon(AquaModpolL) / 1e6 # 3177861
#inter bet Arb and water at 0.5 cutoff
interAWLN <- raster::intersect(ArbModpolL, AquaModpolL)
areaPolygon(interAWLN) / 1e6 # 1562.941
# percent of each niche that overlaps with the other by area
# inter/arb niche
(1562.941/1285231)*100 # 0.1216078
# inter/water niche
(1562.941/3177861)*100 # 0.04918217

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 1285231
areaPolygon(CaveModpolL) / 1e6 # 1450650
#inter bet Arb and cave at 0.5 cutoff
interACLN <- raster::intersect(ArbModpolL, CaveModpolL)
areaPolygon(interACLN) / 1e6 # 372.1321
# percent of each niche that overlaps with the other by area
# inter/arb niche
(372.1321/1285231)*100 # 0.02895449
# inter/cave niche
(372.1321/1450650)*100 # 0.02565278

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 1285231
areaPolygon(FossModpolL) / 1e6 # 3432082
#inter bet Arb and foss at 0.5 cutoff
interAFLN <- raster::intersect(ArbModpolL, FossModpolL)
areaPolygon(interAFLN) / 1e6 # 151899
# percent of each niche that overlaps with the other by area
# inter/arb niche
(151899/1285231)*100 # 11.81881
# inter/foss niche
(151899/3432082)*100 # 4.425856

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolL) / 1e6 # 1285231
areaPolygon(SaxModpolL) / 1e6 # 2259151
#inter bet Arb and sax at 0.5 cutoff
interASLN <- raster::intersect(ArbModpolL, SaxModpolL)
areaPolygon(interASLN) / 1e6 # 14654.73
# percent of each niche that overlaps with the other by area
# inter/arb niche
(14654.73/1285231)*100 # 1.140241
# inter/sax niche
(14654.73/2259151)*100 # 0.6486831

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 4485079
areaPolygon(AquaModpolL) / 1e6 # 3177861
#inter bet Terr and Water at 0.5 cutoff
interTWLN <- raster::intersect(TerrModpolL, AquaModpolL)
areaPolygon(interTWLN) / 1e6 # 2705383
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2705383/4485079)*100 # 60.31963
# inter/water niche
(2705383/3177861)*100 # 85.1322

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 4485079
areaPolygon(CaveModpolL) / 1e6 # 1450650
#inter bet Terr and Cave at 0.5 cutoff
interTCLN <- raster::intersect(TerrModpolL, CaveModpolL)
areaPolygon(interTCLN) / 1e6 # 1424333
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1424333/4485079)*100 # 31.75714
# inter/cave niche
(1424333/1450650)*100 # 98.18585

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 4485079
areaPolygon(FossModpolL) / 1e6 # 3432082
#inter bet Terr and Foss at 0.5 cutoff
interTFLN <- raster::intersect(TerrModpolL, FossModpolL)
areaPolygon(interTFLN) / 1e6 # 3047749
# percent of each niche that overlaps with the other by area
# inter/terr niche
(3047749/4485079)*100 # 67.95307
# inter/foss niche
(3047749/3432082)*100 # 88.80175

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolL) / 1e6 # 4485079
areaPolygon(SaxModpolL) / 1e6 # 2259151
#inter bet Terr and Sax at 0.5 cutoff
interTSLN <- raster::intersect(TerrModpolL, SaxModpolL)
areaPolygon(interTSLN) / 1e6 # 1954294
# percent of each niche that overlaps with the other by area
# inter/terr niche
(1954294/4485079)*100 # 43.57323
# inter/sax niche
(1954294/2259151)*100 # 86.50568

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolL) / 1e6 # 3177861
areaPolygon(CaveModpolL) / 1e6 # 1450650
#inter bet Water and Cave at 0.5 cutoff
interWCLN <- raster::intersect(AquaModpolL, CaveModpolL)
areaPolygon(interWCLN) / 1e6 # 1425004
# percent of each niche that overlaps with the other by area
# inter/water niche
(1425004/3177861)*100 # 44.84161
# inter/cave niche
(1425004/1450650)*100 # 98.2321

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolL) / 1e6 # 3177861
areaPolygon(FossModpolL) / 1e6 # 3432082
#inter bet Water and Foss at 0.5 cutoff
interWFLN <- raster::intersect(AquaModpolL, FossModpolL)
areaPolygon(interWFLN) / 1e6 # 2428182
# percent of each niche that overlaps with the other by area
# inter/water niche
(2428182/3177861)*100 # 76.40932
# inter/foss niche
(2428182/3432082)*100 # 70.74953

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolL) / 1e6 # 3177861
areaPolygon(SaxModpolL) / 1e6 # 2259151
#inter bet Water and Sax at 0.5 cutoff
interWSLN <- raster::intersect(AquaModpolL, SaxModpolL)
areaPolygon(interWSLN) / 1e6 # 2130045
# percent of each niche that overlaps with the other by area
# inter/water niche
(2130045/3652572)*100 # 58.3163
# inter/Sax niche
(2130045/2259151)*100 # 94.2852

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolL) / 1e6 # 1450650
areaPolygon(FossModpolL) / 1e6 # 3432082
#inter bet cave and foss at 0.5 cutoff
interCFLN <- raster::intersect(CaveModpolL, FossModpolL)
areaPolygon(interCFLN) / 1e6 # 1440614
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1440614/1450650)*100 # 99.30817
# inter/foss niche
(1440614/3432082)*100 # 41.97493

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolL) / 1e6 # 1450650
areaPolygon(SaxModpolL) / 1e6 # 2259151
#inter bet cave and sax at 0.5 cutoff
interCSLN <- raster::intersect(CaveModpolL, SaxModpolL)
areaPolygon(interCSLN) / 1e6 # 1440980
# percent of each niche that overlaps with the other by area
# inter/cave niche
(1440980/1450650)*100 # 99.3334
# inter/sax niche
(1440980/2259151)*100 # 63.78414

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolL) / 1e6 # 3432082
areaPolygon(SaxModpolL) / 1e6 # 2259151
#inter bet foss and sax at 0.5 cutoff
interFSLN <- raster::intersect(FossModpolL, SaxModpolL)
areaPolygon(interFSLN) / 1e6 # 2026926
# percent of each niche that overlaps with the other by area
# inter/foss niche
(2026926/3432082)*100 # 59.0582
# inter/sax niche
(2026926/2259151)*100 # 89.7207

######################################################################################

# arb poly intersect with arb niche
# ArbPolyM1 area
areaPolygon(ArbPolyM1) / 1e6 # 659573
# ArbModpolM1 area
areaPolygon(ArbModpolM1) / 1e6 # 1238624
# intersection between them
AAM1 <- raster::intersect(ArbPolyM1,ArbModpolM1)
areaPolygon(AAM1) / 1e6 # 479367.3
# percent of the polygon explained by the niche
(479367.3/659573)*100 # 72.67843

###
# terr poly intersect with terr niche
# TerrPolyM1 area
areaPolygon(TerrPolyM1) / 1e6 # 4456354
# TerrModpolM1 area
areaPolygon(TerrModpolM1) / 1e6 # 5034443
# intersection between them
TTM1 <- raster::intersect(TerrPolyM1,TerrModpolM1)
areaPolygon(TTM1) / 1e6 # 3740585
# percent of the polygon explained by the niche
(3740585/4456354)*100 # 83.93824

###
# water poly intersect with water niche
# AquaPolyM1 area
areaPolygon(AquaPolyM1) / 1e6 # 2829823
# AquaModpolM1 area
areaPolygon(AquaModpolM1) / 1e6 # 3185717
# intersection between them
WWM1 <- raster::intersect(AquaPolyM1,AquaModpolM1)
areaPolygon(WWM1) / 1e6 # 2640386
# percent of the polygon explained by the niche
(2640386/2829823)*100 # 93.30569

###
# cave poly intersect with cave niche
# CavePolyM1 area
areaPolygon(CavePolyM1) / 1e6 # 492224
# CaveModpolM1 area
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
# intersection between them
CCM1 <- raster::intersect(CavePolyM1,CaveModpolM1)
areaPolygon(CCM1) / 1e6 # 408692.5
# percent of the polygon explained by the niche
(408692.5/492224)*100 # 83.02978

###
# foss poly intersect with foss niche
# FossPolyM1 area
areaPolygon(FossPolyM1) / 1e6 # 110435
# FossModpolS area
areaPolygon(FossModpolM1) / 1e6 # 220695.9
# intersection between them
FFM1 <- raster::intersect(FossPolyM1,FossModpolM1)
areaPolygon(FFM1) / 1e6 # 87274.94
# percent of the polygon explained by the niche
(87274.94/110435)*100 # 79.02833

###
# sax poly intersect with sax niche
# SaxPolyM1 area
areaPolygon(SaxPolyM1) / 1e6 # 180106.9
# SaxModpolM1 area
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
# intersection between them
SSM1 <- raster::intersect(SaxPolyM1,SaxModpolM1)
areaPolygon(SSM1) / 1e6 # 153589.4
# percent of the polygon explained by the niche
(153589.4/180106.9)*100 # 85.2768

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 1238624
areaPolygon(TerrModpolM1) / 1e6 # 5034443
#inter bet Arb and Terr at 0.5 cutoff
interATM1N <- raster::intersect(ArbModpolM1, TerrModpolM1)
areaPolygon(interATM1N) / 1e6 # 410471.5
# percent of each niche that overlaps with the other by area
# inter/arb niche
(410471.5/1238624)*100 # 33.13931
# inter/terr niche
(410471.5/5034443)*100 # 8.153265

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 1238624
areaPolygon(AquaModpolM1) / 1e6 # 3185717
#inter bet Arb and water at 0.5 cutoff
interAWM1N <- raster::intersect(ArbModpolM1, AquaModpolM1)
areaPolygon(interAWM1N) / 1e6 # 2068.363
# percent of each niche that overlaps with the other by area
# inter/arb niche
(2068.363/1238624)*100 # 0.1669888
# inter/water niche
(2068.363/3185717)*100 # 0.06492614

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 1238624
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
#inter bet Arb and cave at 0.5 cutoff
interACM1N <- raster::intersect(ArbModpolM1, CaveModpolM1)
areaPolygon(interACM1N) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/2004242)*100 # 0
# inter/cave niche
(0/568391.4)*100 # 0

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 1238624
areaPolygon(FossModpolM1) / 1e6 # 220695.9
#inter bet Arb and foss at 0.5 cutoff
interAFM1N <- raster::intersect(ArbModpolM1, FossModpolM1)
areaPolygon(interAFM1N) / 1e6 # 140123.1
# percent of each niche that overlaps with the other by area
# inter/arb niche
(140123.1/1238624)*100 # 11.3128
# inter/foss niche
(140123.1/220695.9)*100 # 63.49148

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolM1) / 1e6 # 1238624
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
#inter bet Arb and sax at 0.5 cutoff
interASM1N <- raster::intersect(ArbModpolM1, SaxModpolM1)
areaPolygon(interASM1N) / 1e6 # 388.6133
# percent of each niche that overlaps with the other by area
# inter/arb niche
(388.6133/1238624)*100 # 0.0313746
# inter/sax niche
(388.6133/358996.1)*100 # 0.10825

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 5034443
areaPolygon(AquaModpolM1) / 1e6 # 3185717
#inter bet Terr and Water at 0.5 cutoff
interTWM1N <- raster::intersect(TerrModpolM1, AquaModpolM1)
areaPolygon(interTWM1N) / 1e6 # 2953425
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2953425/5034443)*100 # 58.66438
# inter/water niche
(2953425/3185717)*100 # 92.70833

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 5034443
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
#inter bet Terr and Cave at 0.5 cutoff
interTCM1N <- raster::intersect(TerrModpolM1, CaveModpolM1)
areaPolygon(interTCM1N) / 1e6 # 568391.4
# percent of each niche that overlaps with the other by area
# inter/terr niche
(568391.4/5034443)*100 # 11.29006
# inter/cave niche
(568391.4/568391.4)*100 # 100

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 5034443
areaPolygon(FossModpolM1) / 1e6 # 220695.9
#inter bet Terr and Foss at 0.5 cutoff
interTFM1N <- raster::intersect(TerrModpolM1, FossModpolM1)
areaPolygon(interTFM1N) / 1e6 # 27518.65
# percent of each niche that overlaps with the other by area
# inter/terr niche
(27518.65/5034443)*100 # 0.5466076
# inter/foss niche
(27518.65/220695.9)*100 # 12.46904

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolM1) / 1e6 # 5034443
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
#inter bet Terr and Sax at 0.5 cutoff
interTSM1N <- raster::intersect(TerrModpolM1, SaxModpolM1)
areaPolygon(interTSM1N) / 1e6 # 358276.1
# percent of each niche that overlaps with the other by area
# inter/terr niche
(358276.1/5034443)*100 # 7.116499
# inter/sax niche
(358276.1/358996.1)*100 # 99.79944

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolM1) / 1e6 # 3185717
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
#inter bet Water and Cave at 0.5 cutoff
interWCM1N <- raster::intersect(AquaModpolM1, CaveModpolM1)
areaPolygon(interWCM1N) / 1e6 # 547455.2
# percent of each niche that overlaps with the other by area
# inter/water niche
(547455.2/3185717)*100 # 17.18468
# inter/cave niche
(547455.2/568391.4)*100 # 96.31659

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolM1) / 1e6 # 3185717
areaPolygon(FossModpolM1) / 1e6 #  220695.9
#inter bet Water and Foss at 0.5 cutoff
interWFM1N <- raster::intersect(AquaModpolM1, FossModpolM1)
areaPolygon(interWFM1N) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/water niche
(0/3185717)*100 # 0
# inter/foss niche
(0/220695.9)*100 # 0

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolM1) / 1e6 # 3185717
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
#inter bet Water and Sax at 0.5 cutoff
interWSM1N <- raster::intersect(AquaModpolM1, SaxModpolM1)
areaPolygon(interWSM1N) / 1e6 # 351980.2
# percent of each niche that overlaps with the other by area
# inter/water niche
(351980.2/3185717)*100 # 11.0487
# inter/Sax niche
(351980.2/358996.1)*100 # 98.04569

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
areaPolygon(FossModpolM1) / 1e6 # 220695.9
#inter bet cave and foss at 0.5 cutoff
interCFM1N <- raster::intersect(CaveModpolM1, FossModpolM1)
areaPolygon(interCFM1N) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/cave niche
(0/568391.4)*100 # 0
# inter/foss niche
(0/220695.9)*100 # 0

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
#inter bet cave and sax at 0.5 cutoff
interCSM1N <- raster::intersect(CaveModpolM1, SaxModpolM1)
areaPolygon(interCSM1N) / 1e6 # 270299.6
# percent of each niche that overlaps with the other by area
# inter/cave niche
(270299.6/568391.4)*100 # 47.55519
# inter/sax niche
(270299.6/358996.1)*100 # 75.29319

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolM1) / 1e6 # 220695.9
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
#inter bet foss and sax at 0.5 cutoff
interFSM1N <- raster::intersect(FossModpolM1, SaxModpolM1)
areaPolygon(interFSM1N) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/foss niche
(0/8001117)*100 # 0
# inter/sax niche
(0/358996.1)*100 # 0


######################################################################################

# arb poly intersect with arb niche
# ArbPolyM2 area
areaPolygon(ArbPolyM2) / 1e6 # 4598454
# ArbModpolM2 area
areaPolygon(ArbModpolM2) / 1e6 # 5422425
# intersection between them
AAM2 <- raster::intersect(ArbPolyM2,ArbModpolM2)
areaPolygon(AAM2) / 1e6 # 569226
# percent of the polygon explained by the niche
(569226/4598454)*100 # 12.37864

###
# terr poly intersect with terr niche
# TerrPolyM2 area
areaPolygon(TerrPolyM2) / 1e6 # 586519.8
# TerrModpolM1 area
areaPolygon(TerrModpolM2) / 1e6 # 1210240
# intersection between them
TTM2 <- raster::intersect(TerrPolyM2,TerrModpolM2)
areaPolygon(TTM2) / 1e6 # 442997
# percent of the polygon explained by the niche
(442997/586519.8)*100 # 75.52976

###
# water poly intersect with water niche
# AquaPolyM2 area
areaPolygon(AquaPolyM2) / 1e6 # 2191861
# AquaModpolM2 area
areaPolygon(AquaModpolM2) / 1e6 # 2438405
# intersection between them
WWM2 <- raster::intersect(AquaPolyM2,AquaModpolM2)
areaPolygon(WWM2) / 1e6 # 2041626
# percent of the polygon explained by the niche
(2041626/2191861)*100 # 93.14578

###
# cave poly intersect with cave niche
# CavePolyM2 area
areaPolygon(CavePolyM2) / 1e6 # 484303.2
# CaveModpolM2 area
areaPolygon(CaveModpolM2) / 1e6 # 619542
# intersection between them
CCM2 <- raster::intersect(CavePolyM2,CaveModpolM2)
areaPolygon(CCM2) / 1e6 # 421105.1
# percent of the polygon explained by the niche
(421105.1/484303.2)*100 # 86.95072

###
# foss poly intersect with foss niche
# FossPolyM2 area
areaPolygon(FossPolyM2) / 1e6 # 2178794
# FossModpolM2 area
areaPolygon(FossModpolM2) / 1e6 # 3000761
# intersection between them
FFM2 <- raster::intersect(FossPolyM2,FossModpolM2)
areaPolygon(FFM2) / 1e6 # 1970358
# percent of the polygon explained by the niche
(1970358/2178794)*100 # 90.43342

###
# sax poly intersect with sax niche
# SaxPolyM2 area
areaPolygon(SaxPolyM2) / 1e6 # 486247.8
# SaxModpolM2 area
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
# intersection between them
SSM2 <- raster::intersect(SaxPolyM2,SaxModpolM2)
areaPolygon(SSM2) / 1e6 # 425038.6
# percent of the polygon explained by the niche
(425038.6/486247.8)*100 # 87.41193

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 5422425
areaPolygon(TerrModpolM2) / 1e6 # 1210240
#inter bet Arb and Terr at 0.5 cutoff
interATM2N <- raster::intersect(ArbModpolM2, TerrModpolM2)
areaPolygon(interATM2N) / 1e6 # 995023.9
# percent of each niche that overlaps with the other by area
# inter/arb niche
(995023.9/5422425)*100 # 18.35016
# inter/terr niche
(995023.9/1210240)*100 # 82.21707

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 5422425
areaPolygon(AquaModpolM2) / 1e6 # 2438405
#inter bet Arb and water at 0.5 cutoff
interAWM2N <- raster::intersect(ArbModpolM2, AquaModpolM2)
areaPolygon(interAWM2N) / 1e6 # 2390726
# percent of each niche that overlaps with the other by area
# inter/arb niche
(2390726/5422425)*100 # 44.08961
# inter/water niche
(2390726/2438405)*100 # 98.04466

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 5422425
areaPolygon(CaveModpolM2) / 1e6 # 619542
#inter bet Arb and cave at 0.5 cutoff
interACM2N <- raster::intersect(ArbModpolM2, CaveModpolM2)
areaPolygon(interACM2N) / 1e6 # 619542
# percent of each niche that overlaps with the other by area
# inter/arb niche
(619542/5422425)*100 # 11.42555
# inter/cave niche
(619542/619542)*100 # 100

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 5422425
areaPolygon(FossModpolM2) / 1e6 # 3000761
#inter bet Arb and foss at 0.5 cutoff
interAFM2N <- raster::intersect(ArbModpolM2, FossModpolM2)
areaPolygon(interAFM2N) / 1e6 # 2931211
# percent of each niche that overlaps with the other by area
# inter/arb niche
(2931211/5422425)*100 # 54.0572
# inter/foss niche
(2931211/3000761)*100 # 97.68225

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolM2) / 1e6 # 5422425
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
#inter bet Arb and sax at 0.5 cutoff
interASM2N <- raster::intersect(ArbModpolM2, SaxModpolM2)
areaPolygon(interASM2N) / 1e6 # 741251.8
# percent of each niche that overlaps with the other by area
# inter/arb niche
(741251.8/5422425)*100 # 13.67012
# inter/sax niche
(741251.8/744082.7)*100 # 99.61954

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 1210240
areaPolygon(AquaModpolM2) / 1e6 # 2438405
#inter bet Terr and Water at 0.5 cutoff
interTWM2N <- raster::intersect(TerrModpolM2, AquaModpolM2)
areaPolygon(interTWM2N) / 1e6 # 618757.2
# percent of each niche that overlaps with the other by area
# inter/terr niche
(618757.2/1210240)*100 # 51.12682
# inter/water niche
(618757.2/2438405)*100 # 25.37549

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 1210240
areaPolygon(CaveModpolM2) / 1e6 # 619542
#inter bet Terr and Cave at 0.5 cutoff
interTCM2N <- raster::intersect(TerrModpolM2, CaveModpolM2)
areaPolygon(interTCM2N) / 1e6 # 128342
# percent of each niche that overlaps with the other by area
# inter/terr niche
(128342/1210240)*100 # 10.60467
# inter/cave niche
(128342/619542)*100 # 20.71563

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 1210240
areaPolygon(FossModpolM2) / 1e6 # 3000761
#inter bet Terr and Foss at 0.5 cutoff
interTFM2N <- raster::intersect(TerrModpolM2, FossModpolM2)
areaPolygon(interTFM2N) / 1e6 # 585655.3
# percent of each niche that overlaps with the other by area
# inter/terr niche
(585655.3/1210240)*100 # 48.39167
# inter/foss niche
(585655.3/3000761)*100 # 19.51689

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolM2) / 1e6 # 1210240
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
#inter bet Terr and Sax at 0.5 cutoff
interTSM2N <- raster::intersect(TerrModpolM2, SaxModpolM2)
areaPolygon(interTSM2N) / 1e6 # 380625.9
# percent of each niche that overlaps with the other by area
# inter/terr niche
(380625.9/1210240)*100 # 31.45045
# inter/sax niche
(380625.9/744082.7)*100 # 51.15371

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolM2) / 1e6 # 2438405
areaPolygon(CaveModpolM2) / 1e6 # 619542
#inter bet Water and Cave at 0.5 cutoff
interWCM2N <- raster::intersect(AquaModpolM2, CaveModpolM2)
areaPolygon(interWCM2N) / 1e6 # 570314.4
# percent of each niche that overlaps with the other by area
# inter/water niche
(570314.4/2438405)*100 # 23.38883
# inter/cave niche
(570314.4/619542)*100 # 92.05419

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolM2) / 1e6 # 2438405
areaPolygon(FossModpolM2) / 1e6 # 3000761
#inter bet Water and Foss at 0.5 cutoff
interWFM2N <- raster::intersect(AquaModpolM2, FossModpolM2)
areaPolygon(interWFM2N) / 1e6 # 2160864
# percent of each niche that overlaps with the other by area
# inter/water niche
(2160864/2438405)*100 # 88.61793
# inter/foss niche
(2160864/3000761)*100 # 72.01053

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolM2) / 1e6 # 2438405
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
#inter bet Water and Sax at 0.5 cutoff
interWSM2N <- raster::intersect(AquaModpolM2, SaxModpolM2)
areaPolygon(interWSM2N) / 1e6 # 686827.4
# percent of each niche that overlaps with the other by area
# inter/water niche
(686827.4/2214916)*100 # 31.00918
# inter/Sax niche
(686827.4/744082.7)*100 # 92.30525

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolM2) / 1e6 # 619542
areaPolygon(FossModpolM2) / 1e6 # 3000761
#inter bet cave and foss at 0.5 cutoff
interCFM2N <- raster::intersect(CaveModpolM2, FossModpolM2)
areaPolygon(interCFM2N) / 1e6 # 606644.5
# percent of each niche that overlaps with the other by area
# inter/cave niche
(606644.5/619542)*100 # 97.91822
# inter/foss niche
(606644.5/3000761)*100 # 20.21636

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolM2) / 1e6 # 619542
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
#inter bet cave and sax at 0.5 cutoff
interCSM2N <- raster::intersect(CaveModpolM2, SaxModpolM2)
areaPolygon(interCSM2N) / 1e6 # 155806.1
# percent of each niche that overlaps with the other by area
# inter/cave niche
(155806.1/619542)*100 # 25.14859
# inter/sax niche
(155806.1/744082.7)*100 # 20.93935

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolM2) / 1e6 # 3000761
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
#inter bet foss and sax at 0.5 cutoff
interFSM2N <- raster::intersect(FossModpolM2, SaxModpolM2)
areaPolygon(interFSM2N) / 1e6 # 686078.2
# percent of each niche that overlaps with the other by area
# inter/foss niche
(686078.2/3000761)*100 # 22.86347
# inter/sax niche
(686078.2/744082.7)*100 # 92.20456


#######################################
#### distribution inter with niche ###
#######################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolL) / 1e6 # 4485079
interADTL <- raster::intersect(ArbPolyL, TerrModpolL)
areaPolygon(interADTL) / 1e6 # 228119.1
(228119.1/4485079)*100 # 5.086178
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1450650
interADCL <- raster::intersect(ArbPolyL, CaveModpolL)
areaPolygon(interADCL) / 1e6 # 15506.35
(15506.35/1450650)*100 # 1.068924
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2259151
interADSL <- raster::intersect(ArbPolyL, SaxModpolL)
areaPolygon(interADSL) / 1e6 # 24037.58
(24037.58/2259151)*100 # 1.064009
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolL) / 1e6 # 3177861
interADWL <- raster::intersect(ArbPolyL, AquaModpolL)
areaPolygon(interADWL) / 1e6 # 15606.52
(15606.52/3177861)*100 # 0.4911014
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 3432082
interADFL <- raster::intersect(ArbPolyL, FossModpolL)
areaPolygon(interADFL) / 1e6 # 76418.94
(76418.94/3432082)*100 # 2.226606
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolL) / 1e6 # 1285231
interTDAL <- raster::intersect(TerrPolyL, ArbModpolL)
areaPolygon(interTDAL) / 1e6 # 402392.4
(402392.4/1285231)*100 # 31.30896
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1450650
interTDCL <- raster::intersect(TerrPolyL, CaveModpolL)
areaPolygon(interTDCL) / 1e6 # 787702.9
(787702.9/1450650)*100 # 54.3
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2259151
interTDSL <- raster::intersect(TerrPolyL, SaxModpolL)
areaPolygon(interTDSL) / 1e6 # 1068010
(1068010/2259151)*100 # 47.27484
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolL) / 1e6 # 3177861
interTDWL <- raster::intersect(TerrPolyL, AquaModpolL)
areaPolygon(interTDWL) / 1e6 # 1800371
(1800371/3177861)*100 # 56.65355
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 3432082
interTDFL <- raster::intersect(TerrPolyL, FossModpolL)
areaPolygon(interTDFL) / 1e6 # 1831950
(1831950/3432082)*100 # 53.37722
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 1285231
interWDAL <- raster::intersect(AquaPolyL, ArbModpolL)
areaPolygon(interWDAL) / 1e6 # 3677.841
(3677.841/1285231)*100 # 0.2861619
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1450650
interWDCL <- raster::intersect(AquaPolyL, CaveModpolL)
areaPolygon(interWDCL) / 1e6 # 1280260
(1280260/1450650)*100 # 88.25423
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2259151
interWDSL <- raster::intersect(AquaPolyL, SaxModpolL)
areaPolygon(interWDSL) / 1e6 # 1873615
(1873615/2259151)*100 # 82.93447
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolL) / 1e6 # 4485079
interWDTL <- raster::intersect(AquaPolyL, TerrModpolL)
areaPolygon(interWDTL) / 1e6 # 2424644
(2424644/4485079)*100 # 54.06023
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 3432082
interWDFL <- raster::intersect(AquaPolyL, FossModpolL)
areaPolygon(interWDFL) / 1e6 # 2176119
(2176119/3432082)*100 # 63.40522
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 1285231
interCDAL <- raster::intersect(CavePolyL, ArbModpolL)
areaPolygon(interCDAL) / 1e6 # 758.3557
(758.3557/1285231)*100 # 0.0590054
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolL) / 1e6 #  3177861
interCDWL <- raster::intersect(CavePolyL, AquaModpolL)
areaPolygon(interCDWL) / 1e6 # 1161286
(1161286/3177861)*100 # 36.54301
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 # 2259151
interCDSL <- raster::intersect(CavePolyL, SaxModpolL)
areaPolygon(interCDSL) / 1e6 # 1177995
(1177995/2259151)*100 # 52.14326
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolL) / 1e6 # 4485079
interCDTL <- raster::intersect(CavePolyL, TerrModpolL)
areaPolygon(interCDTL) / 1e6 # 1185329
(1185329/4485079)*100 # 26.42827
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolL) / 1e6 # 3432082
interCDFL <- raster::intersect(CavePolyL, FossModpolL)
areaPolygon(interCDFL) / 1e6 # 1207149
(1207149/3432082)*100 # 35.1725
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 1285231
interFDAL <- raster::intersect(FossPolyL, ArbModpolL)
areaPolygon(interFDAL) / 1e6 # 208744.7
(208744.7/1285231)*100 # 16.2418
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolL) / 1e6 # 3177861
interFDWL <- raster::intersect(FossPolyL, AquaModpolL)
areaPolygon(interFDWL) / 1e6 # 1690441
(1690441/3177861)*100 # 53.1943
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolL) / 1e6 #  2259151
interFDSL <- raster::intersect(FossPolyL, SaxModpolL)
areaPolygon(interFDSL) / 1e6 # 1453786
(1453786/2259151)*100 # 64.35099
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolL) / 1e6 # 4485079
interFDTL <- raster::intersect(FossPolyL, TerrModpolL)
areaPolygon(interFDTL) / 1e6 # 1997694
(1997694/4485079)*100 # 44.54089
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1450650
interFDCL <- raster::intersect(FossPolyL, CaveModpolL)
areaPolygon(interFDCL) / 1e6 # 1117259
(1117259/1450650)*100 # 77.01782
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolL) / 1e6 # 1285231
interSDAL <- raster::intersect(SaxPolyL, ArbModpolL)
areaPolygon(interSDAL) / 1e6 # 51210.41
(51210.41/1285231)*100 # 3.98453
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolL) / 1e6 # 3177861
interSDWL <- raster::intersect(SaxPolyL, AquaModpolL)
areaPolygon(interSDWL) / 1e6 # 1693343
(1693343/3177861)*100 # 53.28562
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolL) / 1e6 # 3432082
interSDFL <- raster::intersect(SaxPolyL, FossModpolL)
areaPolygon(interSDFL) / 1e6 # 1667418
(1667418/3432082)*100 # 48.58328
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolL) / 1e6 # 4485079
interSDTL <- raster::intersect(SaxPolyL, TerrModpolL)
areaPolygon(interSDTL) / 1e6 # 1690966
(1690966/4485079)*100 # 37.70203
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolL) / 1e6 # 1450650
interSDCL <- raster::intersect(SaxPolyL, CaveModpolL)
areaPolygon(interSDCL) / 1e6 # 1314201
(1314201/1450650)*100 # 90.59394
# this is sax spp present where cave can live

##############################################################################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 5034443
interADTM1 <- raster::intersect(ArbPolyM1, TerrModpolM1)
areaPolygon(interADTM1) / 1e6 # 212216.4
(212216.4/5034443)*100 # 4.215291
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
interADCM1 <- raster::intersect(ArbPolyM1, CaveModpolM1)
areaPolygon(interADCM1) / 1e6 # 0
(0/568391.4)*100 # 0
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
interADSM1 <- raster::intersect(ArbPolyM1, SaxModpolM1)
areaPolygon(interADSM1) / 1e6 # 142.7997
(142.7997/358996.1)*100 #  0.03977751
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3185717
interADWM1 <- raster::intersect(ArbPolyM1, AquaModpolM1)
# this is zero, they dont overlap
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 220695.9
interADFM1 <- raster::intersect(ArbPolyM1, FossModpolM1)
areaPolygon(interADFM1) / 1e6 # 91648.27
(91648.27/220695.9)*100 # 41.52695
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 1238624
interTDAM1 <- raster::intersect(TerrPolyM1, ArbModpolM1)
areaPolygon(interTDAM1) / 1e6 # 523088.6
(523088.6/1238624)*100 # 42.23143
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
interTDCM1 <- raster::intersect(TerrPolyM1, CaveModpolM1)
areaPolygon(interTDCM1) / 1e6 # 562254.6
(562254.6/568391.4)*100 # 98.92032
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
interTDSM1 <- raster::intersect(TerrPolyM1, SaxModpolM1)
areaPolygon(interTDSM1) / 1e6 # 356901.4
(356901.4/358996.1)*100 # 99.41651
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3185717
interTDWM1 <- raster::intersect(TerrPolyM1, AquaModpolM1)
areaPolygon(interTDWM1) / 1e6 # 2757471
(2757471/3185717)*100 # 86.55731
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 220695.9
interTDFM1 <- raster::intersect(TerrPolyM1, FossModpolM1)
areaPolygon(interTDFM1) / 1e6 #  68932.18
(68932.18/220695.9)*100 # 31.23401
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 1238624
interWDAM1 <- raster::intersect(AquaPolyM1, ArbModpolM1)
areaPolygon(interWDAM1) / 1e6 # 1601.906
(1601.906/1238624)*100 # 0.1293295
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
interWDCM1 <- raster::intersect(AquaPolyM1, CaveModpolM1)
areaPolygon(interWDCM1) / 1e6 # 463548.4
(463548.4/568391.4)*100 # 81.55444
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
interWDSM1 <- raster::intersect(AquaPolyM1, SaxModpolM1)
areaPolygon(interWDSM1) / 1e6 # 356338
(356338/358996.1)*100 # 99.25957
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 5034443
interWDTM1 <- raster::intersect(AquaPolyM1, TerrModpolM1)
areaPolygon(interWDTM1) / 1e6 # 2611339
(2611339/5034443)*100 # 51.86947
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 220695.9
interWDFM1 <- raster::intersect(AquaPolyM1, FossModpolM1)
areaPolygon(interWDFM1) / 1e6 # 0
(0/220695.9)*100 # 0
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 1238624
interCDAM1 <- raster::intersect(CavePolyM1, ArbModpolM1)
areaPolygon(interCDAM1) / 1e6 # 428.6043
(428.6043/1238624)*100 # 0.03460326
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3185717
interCDWM1 <- raster::intersect(CavePolyM1, AquaModpolM1)
areaPolygon(interCDWM1) / 1e6 # 440487.8
(440487.8/3185717)*100 # 13.82696
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
interCDSM1 <- raster::intersect(CavePolyM1, SaxModpolM1)
areaPolygon(interCDSM1) / 1e6 # 195580.5
(195580.5/358996.1)*100 # 54.47984
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 5034443
interCDTM1 <- raster::intersect(CavePolyM1, TerrModpolM1)
areaPolygon(interCDTM1) / 1e6 # 459201.2
(459201.2/5034443)*100 # 9.121192
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolM1) / 1e6 # 220695.9
interCDFM1 <- raster::intersect(CavePolyM1, FossModpolM1)
areaPolygon(interCDFM1) / 1e6 # 0
(0/220695.9)*100 # 0
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 1238624
interFDAM1 <- raster::intersect(FossPolyM1, ArbModpolM1)
areaPolygon(interFDAM1) / 1e6 # 68450.68
(68450.68/1238624)*100 # 5.526349
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3185717
interFDWM1 <- raster::intersect(FossPolyM1, AquaModpolM1)
# this is zero, they do not intersect
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolM1) / 1e6 # 358996.1
interFDSM1 <- raster::intersect(FossPolyM1, SaxModpolM1)
areaPolygon(interFDSM1) / 1e6 # 0
(0/358996.1)*100 # 0
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 5034443
interFDTM1 <- raster::intersect(FossPolyM1, TerrModpolM1)
areaPolygon(interFDTM1) / 1e6 # 3849.092
(3849.092/5034443)*100 # 0.07645517
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
interFDCM1 <- raster::intersect(FossPolyM1, CaveModpolM1)
# this is zero, they do not intersect
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolM1) / 1e6 # 1238624
interSDAM1 <- raster::intersect(SaxPolyM1, ArbModpolM1)
areaPolygon(interSDAM1) / 1e6 # 751.0628
(751.0628/1238624)*100 # 0.06063687
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolM1) / 1e6 # 3185717
interSDWM1 <- raster::intersect(SaxPolyM1, AquaModpolM1)
areaPolygon(interSDWM1) / 1e6 #  171737.1
(171737.1/3185717)*100 # 5.390846
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolM1) / 1e6 # 220695.9
interSDFM1 <- raster::intersect(SaxPolyM1, FossModpolM1)
areaPolygon(interSDFM1) / 1e6 # 216.7672
(216.7672/220695.9)*100 # 0.09821986
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolM1) / 1e6 # 5034443
interSDTM1 <- raster::intersect(SaxPolyM1, TerrModpolM1)
areaPolygon(interSDTM1) / 1e6 # 177320.5
(177320.5/5034443)*100 # 3.522147
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolM1) / 1e6 # 568391.4
interSDCM1 <- raster::intersect(SaxPolyM1, CaveModpolM1)
areaPolygon(interSDCM1) / 1e6 # 120628.8
(120628.8/568391.4)*100 # 21.22284
# this is sax spp present where cave can live

##############################################################################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolM2) / 1e6 #1210240
interADTM2 <- raster::intersect(ArbPolyM2, TerrModpolM2)
areaPolygon(interADTM2) / 1e6 # 860980.4
(860980.4/1210240)*100 # 71.14129
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 619542
interADCM2 <- raster::intersect(ArbPolyM2, CaveModpolM2)
areaPolygon(interADCM2) / 1e6 # 611974.8
(611974.8/619542)*100 # 98.77858
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
interADSM2 <- raster::intersect(ArbPolyM2, SaxModpolM2)
areaPolygon(interADSM2) / 1e6 # 707441.6
(707441.6/744082.7)*100 # 95.07567
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2438405
interADWM2 <- raster::intersect(ArbPolyM2, AquaModpolM2)
areaPolygon(interADWM2) / 1e6 # 2064579
(2064579/2438405)*100 # 84.66924
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3000761
interADFM2 <- raster::intersect(ArbPolyM2, FossModpolM2)
areaPolygon(interADFM2) / 1e6 # 2397664
(2397664/3000761)*100 # 79.90186
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 619542
interTDAM2 <- raster::intersect(TerrPolyM2, ArbModpolM2)
areaPolygon(interTDAM2) / 1e6 # 445856.3
(445856.3/619542)*100 # 71.96547
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 619542
interTDCM2 <- raster::intersect(TerrPolyM2, CaveModpolM2)
areaPolygon(interTDCM2) / 1e6 # 48742.96
(48742.96/619542)*100 # 7.86758
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
interTDSM2 <- raster::intersect(TerrPolyM2, SaxModpolM2)
areaPolygon(interTDSM2) / 1e6 # 85453.15
(85453.15/744082.7)*100 # 11.48436
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2438405
interTDWM2 <- raster::intersect(TerrPolyM2, AquaModpolM2)
areaPolygon(interTDWM2) / 1e6 # 226003.9
(226003.9/2438405)*100 # 9.268514
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3000761
interTDFM2 <- raster::intersect(TerrPolyM2, FossModpolM2)
areaPolygon(interTDFM2) / 1e6 # 209156.6
(209156.6/3000761)*100 # 6.970119
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 5422425
interWDAM2 <- raster::intersect(AquaPolyM2, ArbModpolM2)
areaPolygon(interWDAM2) / 1e6 # 2120162
(2120162/5422425)*100 # 39.09989
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 619542
interWDCM2 <- raster::intersect(AquaPolyM2, CaveModpolM2)
areaPolygon(interWDCM2) / 1e6 # 482903.1
(482903.1/619542)*100 # 77.94518
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
interWDSM2 <- raster::intersect(AquaPolyM2, SaxModpolM2)
areaPolygon(interWDSM2) / 1e6 # 673854
(673854/744082.7)*100 # 
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolM2) / 1e6 # 1210240
interWDTM2 <- raster::intersect(AquaPolyM2, TerrModpolM2)
areaPolygon(interWDTM2) / 1e6 # 612603.4
(612603.4/1210240)*100 # 50.61834
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 #3000761
interWDFM2 <- raster::intersect(AquaPolyM2, FossModpolM2)
areaPolygon(interWDFM2) / 1e6 # 1906659
(1906659/3000761)*100 # 63.53918
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 5422425
interCDAM2 <- raster::intersect(CavePolyM2, ArbModpolM2)
areaPolygon(interCDAM2) / 1e6 # 468919.5
(468919.5/5422425)*100 # 8.647782
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2438405
interCDWM2 <- raster::intersect(CavePolyM2, AquaModpolM2)
areaPolygon(interCDWM2) / 1e6 # 424255.6
(424255.6/2438405)*100 # 17.3989
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
interCDSM2 <- raster::intersect(CavePolyM2, SaxModpolM2)
areaPolygon(interCDSM2) / 1e6 # 83977.9
(83977.9/744082.7)*100 # 11.2861
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolM2) / 1e6 #1210240
interCDTM2 <- raster::intersect(CavePolyM2, TerrModpolM2)
areaPolygon(interCDTM2) / 1e6 # 83018.22
(83018.22/1210240)*100 # 6.859649
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3000761
interCDFM2 <- raster::intersect(CavePolyM2, FossModpolM2)
areaPolygon(interCDFM2) / 1e6 # 2011769
(2011769/3000761)*100 # 67.04196
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 5422425
interFDAM2 <- raster::intersect(FossPolyM2, ArbModpolM2)
areaPolygon(interFDAM2) / 1e6 # 2011769
(2011769/5422425)*100 # 37.10091
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2438405
interFDWM2 <- raster::intersect(FossPolyM2, AquaModpolM2)
areaPolygon(interFDWM2) / 1e6 #  1574994
(1574994/2438405)*100 # 64.59116
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolM2) / 1e6 # 744082.7
interFDSM2 <- raster::intersect(FossPolyM2, SaxModpolM2)
areaPolygon(interFDSM2) / 1e6 # 537939.4
(537939.4/744082.7)*100 # 72.29565
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolM2) / 1e6 #  1210240
interFDTM2 <- raster::intersect(FossPolyM2, TerrModpolM2)
areaPolygon(interFDTM2) / 1e6 # 487918.9
(487918.9/1210240)*100 # 40.31588
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 619542
interFDCM2 <- raster::intersect(FossPolyM2, CaveModpolM2)
areaPolygon(interFDCM2) / 1e6 # 449362.8
(449362.8/619542)*100 # 72.53145
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolM2) / 1e6 # 5422425
interSDAM2 <- raster::intersect(SaxPolyM2, ArbModpolM2)
areaPolygon(interSDAM2) / 1e6 # 474380.7
(474380.7/5422425)*100 # 8.748497
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolM2) / 1e6 # 2438405
interSDWM2 <- raster::intersect(SaxPolyM2, AquaModpolM2)
areaPolygon(interSDWM2) / 1e6 # 428326.7
(428326.7/2438405)*100 # 17.56586
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolM2) / 1e6 # 3000761
interSDFM2 <- raster::intersect(SaxPolyM2, FossModpolM2)
areaPolygon(interSDFM2) / 1e6 # 428297.4
(428297.4/3000761)*100 # 14.27296
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolM2) / 1e6 # 1210240
interSDTM2 <- raster::intersect(SaxPolyM2, TerrModpolM2)
areaPolygon(interSDTM2) / 1e6 # 194748.8
(194748.8/1210240)*100 # 16.09175
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolM2) / 1e6 # 619542
interSDCM2 <- raster::intersect(SaxPolyM2, CaveModpolM2)
areaPolygon(interSDCM2) / 1e6 # 95429.33
(95429.33/619542)*100 # 15.40321
# this is sax spp present where cave can live

##################
# niche equivalency
##################

# climate data
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogetherFinalGTiff.gri')

# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors <- crop(x = ClimateData, y = geographic.extent)

# get point data ready
ArbPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_lenient/chull.shp")
TerrPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_lenient/chull.shp")
ArbPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_M1/chull.shp")
TerrPointsM1 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_M1/chull.shp")
ArbPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Arb_Points_M2/chull.shp")
TerrPointsM2 <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Len_M1_M2/Points/Terr_Points_M2/chull.shp")

# make point data frame
ArbLDF <- data.frame(ArbPointsL)
ArbLDF <- ArbLDF[,1:2]
TerrLDF <- data.frame(TerrPointsL)
TerrLDF <- TerrLDF[,1:2]

ArbM1DF <- data.frame(ArbPointsM1)
ArbM1DF <- ArbM1DF[,1:2]
TerrM1DF <- data.frame(TerrPointsM1)
TerrM1DF <- TerrM1DF[,1:2]

ArbM2DF <- data.frame(ArbPointsM2)
ArbM2DF <- ArbM2DF[,1:2]
TerrM2DF <- data.frame(TerrPointsM2)
TerrM2DF <- TerrM2DF[,1:2]

# leninet
IDTestL <- dismo::nicheEquivalency(sp1=ArbLDF, sp2=TerrLDF, predictors = predictors,
                                  n=100, model=maxent, verbose=T)
chars <- capture.output(print(IDTestL))
writeLines(chars, con = file("output_lenient.txt"))
OutputSimple <- rbind(IDTestL$statistic, IDTestL$null.distribution)
D_Rand <- OutputSimple[-1,1]
D_Obs <- OutputSimple[1,1]
(length(which(D_Rand < D_Obs))+1)/100 # p = 0.01
I_Rand <- OutputSimple[-1,2]
I_Obs <- OutputSimple[1,2]
(length(which(I_Rand < I_Obs))+1)/100 # p = 0.01
write.csv(OutputSimple, "IandDOutput_lenient.csv", row.names = F)
pdf("IandDSig_lenient.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar = c(4,4,2,1))
hist(D_Rand, col = "blue", xlim = c(0, 1), main = "Shoener's D", xlab = "D", ylab = "Frequency")
abline(v = D_Obs, col = "blue", lwd = 3)
par(mar=c(4,2,2,1))
hist(I_Rand, col = "red", xlim = c(0, 1), main = "Warren's I", xlab = "I")
abline(v = I_Obs, col = "red", lwd = 3)
dev.off()

# M1
IDTestM1 <- dismo::nicheEquivalency(sp1=ArbM1DF, sp2=TerrM1DF, predictors = predictors,
                                   n=100, model=maxent, verbose=T)
chars <- capture.output(print(IDTestM1))
writeLines(chars, con = file("output_M1.txt"))
OutputSimple <- rbind(IDTestM1$statistic, IDTestM1$null.distribution)
D_Rand <- OutputSimple[-1,1]
D_Obs <- OutputSimple[1,1]
(length(which(D_Rand < D_Obs))+1)/100 # p = 0.01
I_Rand <- OutputSimple[-1,2]
I_Obs <- OutputSimple[1,2]
(length(which(I_Rand < I_Obs))+1)/100 # p = 0.01
write.csv(OutputSimple, "IandDOutput_M1.csv", row.names = F)
pdf("IandDSig_M1.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar = c(4,4,2,1))
hist(D_Rand, col = "blue", xlim = c(0, 1), main = "Shoener's D", xlab = "D", ylab = "Frequency")
abline(v = D_Obs, col = "blue", lwd = 3)
par(mar=c(4,2,2,1))
hist(I_Rand, col = "red", xlim = c(0, 1), main = "Warren's I", xlab = "I")
abline(v = I_Obs, col = "red", lwd = 3)
dev.off()

# M2
IDTestM2 <- dismo::nicheEquivalency(sp1=ArbM2DF, sp2=TerrM2DF, predictors = predictors,
                                    n=100, model=maxent, verbose=T)
chars <- capture.output(print(IDTestM2))
writeLines(chars, con = file("output_M2.txt"))
OutputSimple <- rbind(IDTestM2$statistic, IDTestM2$null.distribution)
D_Rand <- OutputSimple[-1,1]
D_Obs <- OutputSimple[1,1]
(length(which(D_Rand < D_Obs))+1)/100 # p = 0.01
I_Rand <- OutputSimple[-1,2]
I_Obs <- OutputSimple[1,2]
(length(which(I_Rand < I_Obs))+1)/100 # p = 0.01
write.csv(OutputSimple, "IandDOutput_M2.csv", row.names = F)
pdf("IandDSig_M2.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar = c(4,4,2,1))
hist(D_Rand, col = "blue", xlim = c(0, 1), main = "Shoener's D", xlab = "D", ylab = "Frequency")
abline(v = D_Obs, col = "blue", lwd = 3)
par(mar=c(4,2,2,1))
hist(I_Rand, col = "red", xlim = c(0, 1), main = "Warren's I", xlab = "I")
abline(v = I_Obs, col = "red", lwd = 3)
dev.off()




###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###  for the microhabitat 7M and 7L    ############
##################################################

# packages
library(phyloclim); library(geosphere); library(raster)
library(rgdal); library(rgeos); library(dismo)

# load the maxent predictions
ArbMod7M <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/ArbMod_prediction_7M.grd")
TerrMod7M <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/TerrMod_prediction_7M.grd")
AquaMod7M <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/AquaMod_prediction_7M.grd")
CaveMod7M <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/CaveMod_prediction_7M.grd")
FossMod7M <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/FossMod_prediction_7M.grd")
SaxMod7M <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/SaxMod_prediction_7M.grd")
ArbMod7L <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/ArbMod_prediction_7L.grd")
TerrMod7L <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/TerrMod_prediction_7L.grd")
AquaMod7L <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/AquaMod_prediction_7L.grd")
CaveMod7L <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/CaveMod_prediction_7L.grd")
FossMod7L <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/FossMod_prediction_7L.grd")
SaxMod7L <- raster("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Predictions/SaxMod_prediction_7L.grd")

# load the polygons
ArbPoly7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/ArbPoly_7M/chull.shp")
TerrPoly7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/TerrPoly_7M/chull.shp")
AquaPoly7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/AquaPoly_7M/chull.shp")
CavePoly7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/CavePoly_7M/chull.shp")
FossPoly7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/FossPoly_7M/chull.shp")
SaxPoly7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/SaxPoly_7M/chull.shp")
ArbPoly7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/ArbPoly_7L/chull.shp")
TerrPoly7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/TerrPoly_7L/chull.shp")
AquaPoly7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/AquaPoly_7L/chull.shp")
CavePoly7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/CavePoly_7L/chull.shp")
FossPoly7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/FossPoly_7L/chull.shp")
SaxPoly7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Polygons/SaxPoly_7L/chull.shp")

# load the points
ArbPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Arb_Points_7M/chull.shp")
TerrPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Terr_Points_7M/chull.shp")
AquaPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Aqua_Points_7M/chull.shp")
CavePoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Cave_Points_7M/chull.shp")
FossPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Foss_Points_7M/chull.shp")
SaxPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Sax_Points_7M/chull.shp")
ArbPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Arb_Points_7L/chull.shp")
TerrPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Terr_Points_7L/chull.shp")
AquaPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Aqua_Points_7L/chull.shp")
CavePoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Cave_Points_7L/chull.shp")
FossPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Foss_Points_7L/chull.shp")
SaxPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/7M_7L/Points/Sax_Points_7L/chull.shp")

# give them a threshold suitability score
ArbMod7MSS <- ArbMod7M > 0.5
TerrMod7MSS <- TerrMod7M > 0.5
AquaMod7MSS <- AquaMod7M > 0.5
CaveMod7MSS <- CaveMod7M > 0.5
FossMod7MSS <- FossMod7M > 0.5
SaxMod7MSS <- SaxMod7M > 0.5
ArbMod7LSS <- ArbMod7L > 0.5
TerrMod7LSS <- TerrMod7L > 0.5
AquaMod7LSS <- AquaMod7L > 0.5
CaveMod7LSS <- CaveMod7L > 0.5
FossMod7LSS <- FossMod7L > 0.5
SaxMod7LSS <- SaxMod7L > 0.5

# turn it into a polygon
ArbModpol7M <- rasterToPolygons(ArbMod7MSS,function(x) x == 1,dissolve=T)
TerrModpol7M <- rasterToPolygons(TerrMod7MSS,function(x) x == 1,dissolve=T)
AquaModpol7M <- rasterToPolygons(AquaMod7MSS,function(x) x == 1,dissolve=T)
CaveModpol7M <- rasterToPolygons(CaveMod7MSS,function(x) x == 1,dissolve=T)
FossModpol7M <- rasterToPolygons(FossMod7MSS,function(x) x == 1,dissolve=T)
SaxModpol7M <- rasterToPolygons(SaxMod7MSS,function(x) x == 1,dissolve=T)
ArbModpol7L <- rasterToPolygons(ArbMod7LSS,function(x) x == 1,dissolve=T)
TerrModpol7L <- rasterToPolygons(TerrMod7LSS,function(x) x == 1,dissolve=T)
AquaModpol7L <- rasterToPolygons(AquaMod7LSS,function(x) x == 1,dissolve=T)
CaveModpol7L <- rasterToPolygons(CaveMod7LSS,function(x) x == 1,dissolve=T)
FossModpol7L <- rasterToPolygons(FossMod7LSS,function(x) x == 1,dissolve=T)
SaxModpol7L <- rasterToPolygons(SaxMod7LSS,function(x) x == 1,dissolve=T)

# get the area of the niche polygon with a 0.5 cutoff
areaPolygon(ArbModpol7M) / 1e6
#  1094396
areaPolygon(TerrModpol7M) / 1e6
#  5076573
areaPolygon(AquaModpol7M) / 1e6
# 
areaPolygon(CaveModpol7M) / 1e6
# 
areaPolygon(FossModpol7M) / 1e6
# 
areaPolygon(SaxModpol7M) / 1e6
# 
areaPolygon(ArbModpol7L) / 1e6
# 1283715
areaPolygon(TerrModpol7L) / 1e6
#  4590369
areaPolygon(AquaModpol7L) / 1e6
# 
areaPolygon(CaveModpol7L) / 1e6
# 
areaPolygon(FossModpol7L) / 1e6
# 
areaPolygon(SaxModpol7L) / 1e6
# 

######################################################################################################
# 7M reciprocal suitability 10/20/19

# How much habitat that dirt species live in is suitable for veg life 0.5 cut off veg niche
areaPolygon(ArbModpol7M) / 1e6 #  1094396
areaPolygon(TerrPoly7M) / 1e6 # 4547682
intersectionTA <- raster::intersect(ArbModpol7M, TerrPoly7M)
areaPolygon(intersectionTA) / 1e6 # 534436.7
# divide the intersection by the dirt polygon area
(534436.7/4547682)*100 # 11.75185

# How much habitat that veg species live in is suitable for dirt life 0.5 cut off dirt niche
areaPolygon(TerrModpol7M) / 1e6 #  5076573
areaPolygon(ArbPoly7M) / 1e6 # 593188.5
intersectionAT <- raster::intersect(TerrModpol7M, ArbPoly7M)
areaPolygon(intersectionAT) / 1e6 # 209912.3
# divide the intersection by the veg polygon area
(209912.3/593188.5)*100 # 35.38712

# veg polygon and veg niche 0.5 cut off veg niche
areaPolygon(ArbModpol7M) / 1e6 # 1094396
areaPolygon(ArbPoly7M) / 1e6 # 593188.5
intersectionAA <- raster::intersect(ArbModpol7M, ArbPoly7M)
areaPolygon(intersectionAA) / 1e6 # 438276.4
# divide the intersection by the veg polygon area
(438276.4/593188.5)*100 # 73.88484

# dirt polygon and dirt niche 0.5 cut off dirt niche
areaPolygon(TerrModpol7M) / 1e6 #  5076573
areaPolygon(TerrPoly7M) / 1e6 # 4547682
intersectionTT <- raster::intersect(TerrModpol7M, TerrPoly7M)
areaPolygon(intersectionTT) / 1e6 # 3728410
# divide the intersection by the dirt polygon area
(3728410/4547682)*100 # 81.98484

# 7L reciprocal suitability 10/20/19

# How much habitat that dirt species live in is suitable for veg life 0.5 cut off veg niche
areaPolygon(ArbModpol7L) / 1e6 #  1283715
areaPolygon(TerrPoly7L) / 1e6 # 3138294
intersectionTA <- raster::intersect(ArbModpol7L, TerrPoly7L)
areaPolygon(intersectionTA) / 1e6 # 398310.4
# divide the intersection by the dirt polygon area
(398310.4/3138294)*100 # 12.69194

# How much habitat that veg species live in is suitable for dirt life 0.5 cut off dirt niche
areaPolygon(TerrModpol7L) / 1e6 # 4590369
areaPolygon(ArbPoly7L) / 1e6 # 675927
intersectionAT <- raster::intersect(TerrModpol7L, ArbPoly7L)
areaPolygon(intersectionAT) / 1e6 #  239518.7
# divide the intersection by the veg polygon area
(239518.7/675927)*100 # 35.43559

# veg polygon and veg niche 0.5 cut off veg niche
areaPolygon(ArbModpol7L) / 1e6 #  1283715
areaPolygon(ArbPoly7L) / 1e6 #   675927
intersectionAA <- raster::intersect(ArbModpol7L, ArbPoly7L)
areaPolygon(intersectionAA) / 1e6 # 500257.3
# divide the intersection by the veg polygon area
(500257.3/675927)*100 # 74.01055

# dirt polygon and dirt niche 0.5 cut off dirt niche
areaPolygon(TerrModpol7L) / 1e6 #  4590369
areaPolygon(TerrPoly7L) / 1e6 # 3138294
intersectionTT <- raster::intersect(TerrModpol7L, TerrPoly7L)
areaPolygon(intersectionTT) / 1e6 # 2664304
# divide the intersection by the dirt polygon area
(2664304/3138294)*100 # 84.89657

######################################################################################################

# niche overlap 
# arb and terr
dismo::nicheOverlap(ArbMod7MSS, TerrMod7MSS, stat='I', mask=T, checkNegatives = T)
# 0.1909445
dismo::nicheOverlap(ArbMod7MSS, TerrMod7MSS, stat='D', mask=T, checkNegatives = T)
# 0.0837642
dismo::nicheOverlap(ArbMod7M, TerrMod7M, stat='I', mask=T, checkNegatives = T)
# 0.5937432
dismo::nicheOverlap(ArbMod7M, TerrMod7M, stat='D', mask=T, checkNegatives = T)
# 0.3233526

# arb and water
dismo::nicheOverlap(ArbMod7MSS, AquaMod7MSS, stat='I', mask=T, checkNegatives = T)
#
dismo::nicheOverlap(ArbMod7MSS, AquaMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, AquaMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, AquaMod7M, stat='D', mask=T, checkNegatives = T)
# 

# arb and cave
dismo::nicheOverlap(ArbMod7MSS, CaveMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, CaveMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, CaveMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, CaveMod7M, stat='D', mask=T, checkNegatives = T)
# 

# arb and foss
dismo::nicheOverlap(ArbMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# arb and sax
dismo::nicheOverlap(ArbMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and water
dismo::nicheOverlap(TerrMod7MSS, AquaMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, AquaMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, AquaMod7M, stat='I', mask=T, checkNegatives = T)
#
dismo::nicheOverlap(TerrMod7M, AquaMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and cave
dismo::nicheOverlap(TerrMod7MSS, CaveMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, CaveMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, CaveMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, CaveMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and foss 
dismo::nicheOverlap(TerrMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and sax
dismo::nicheOverlap(TerrMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# water and cave
dismo::nicheOverlap(AquaMod7MSS, CaveMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7MSS, CaveMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, CaveMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, CaveMod7M, stat='D', mask=T, checkNegatives = T)
# 

# water and foss
dismo::nicheOverlap(AquaMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# water and sax
dismo::nicheOverlap(AquaMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# cave and fos
dismo::nicheOverlap(CaveMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# cave and sax
dismo::nicheOverlap(CaveMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
#
dismo::nicheOverlap(CaveMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# foss and sax
dismo::nicheOverlap(FossMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(FossMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(FossMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(FossMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

###
# arb poly intersect with arb niche
# ArbPoly7M area
areaPolygon(ArbPoly7M) / 1e6 # 593188.5
# ArbModpol7M area
areaPolygon(ArbModpol7M) / 1e6 # 1094396
# intersection between them
AA7M <- raster::intersect(ArbPoly7M,ArbModpol7M)
areaPolygon(AA7M) / 1e6 # 438276.4
# percent of the polygon explained by the niche
(438276.4/593188.5)*100 # 73.88484

###
# terr poly intersect with terr niche
# TerrPoly7M area
areaPolygon(TerrPoly7M) / 1e6 # 4547682
# TerrModpol7M area
areaPolygon(TerrModpol7M) / 1e6 # 5076573
# intersection between them
TT7M <- raster::intersect(TerrPoly7M,TerrModpol7M)
areaPolygon(TT7M) / 1e6 # 3728410
# percent of the polygon explained by the niche
(3744265/4547682)*100 # 82.33348

###
# water poly intersect with water niche
# AquaPoly7M area
areaPolygon(AquaPoly7M) / 1e6 # 
# AquaModpol7M area
areaPolygon(AquaModpol7M) / 1e6 # 
# intersection between them
WW7M <- raster::intersect(AquaPoly7M,AquaModpol7M)
areaPolygon(WW7M) / 1e6 # 
# percent of the polygon explained by the niche
(2621246/2829823 )*100 #

###
# cave poly intersect with cave niche
# CavePoly7M area
areaPolygon(CavePoly7M) / 1e6 # 
# CaveModpol7M area
areaPolygon(CaveModpol7M) / 1e6 # 
# intersection between them
CC7M <- raster::intersect(CavePoly7M,CaveModpol7M)
areaPolygon(CC7M) / 1e6 # 
# percent of the polygon explained by the niche
(411796.6/492224)*100 # 

###
# foss poly intersect with foss niche
# FossPoly7M area
areaPolygon(FossPoly7M) / 1e6 # 
# FossModpol7M area
areaPolygon(FossModpol7M) / 1e6 # 
# intersection between them
FF7M <- raster::intersect(FossPoly7M,FossModpol7M)
areaPolygon(FF7M) / 1e6 # 
# percent of the polygon explained by the niche
(86868.56/110435)*100 # 

###
# sax poly intersect with sax niche
# SaxPoly7M area
areaPolygon(SaxPoly7M) / 1e6 # 
# SaxModpol7M area
areaPolygon(SaxModpol7M) / 1e6 # 
# intersection between them
SS7M <- raster::intersect(SaxPoly7M,SaxModpol7M)
areaPolygon(SS7M) / 1e6 # 
# percent of the polygon explained by the niche
(151507/180106.9)*100 # 

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 1094396
areaPolygon(TerrModpol7M) / 1e6 # 5076573
#inter bet Arb and Terr at 0.5 cutoff
interAT7M <- raster::intersect(ArbModpol7M, TerrModpol7M)
areaPolygon(interAT7M) / 1e6 # 418788
# percent of each niche that overlaps with the other by area
# inter/arb niche
(418788/1094396)*100 # 38.26659
# inter/terr niche
(418788/5076573)*100 # 8.249423

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(AquaModpol7M) / 1e6 # 
#inter bet Arb and water at 0.5 cutoff
interAW7M <- raster::intersect(ArbModpol7M, AquaModpol7M)
areaPolygon(interAW7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(3863.806/1051674)*100 # 
# inter/water niche
(3863.806/3182005)*100 # 

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(CaveModpol7M) / 1e6 # ]
#inter bet Arb and cave at 0.5 cutoff
interAC7M <- raster::intersect(ArbModpol7M, CaveModpol7M)
areaPolygon(interAC7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/1051674)*100 # 
# inter/cave niche
(0/607216.5)*100 # 

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet Arb and foss at 0.5 cutoff
interAF7M <- raster::intersect(ArbModpol7M, FossModpol7M)
areaPolygon(interAF7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(118243.9/1051674)*100 # 
# inter/foss niche
(118243.9/218512.1)*100 # 

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet Arb and sax at 0.5 cutoff
interAS7M <- raster::intersect(ArbModpol7M, SaxModpol7M)
areaPolygon(interAS7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/1051674)*100 # 0
# inter/sax niche
(0/350360.8)*100 # 0

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(AquaModpol7M) / 1e6 # 
#inter bet Terr and Water at 0.5 cutoff
interTW7M <- raster::intersect(TerrModpol7M, AquaModpol7M)
areaPolygon(interTW7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2989729/5144655)*100 # 
# inter/water niche
(2989729/3182005)*100 # 

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(CaveModpol7M) / 1e6 # 
#inter bet Terr and Cave at 0.5 cutoff
interTC7M <- raster::intersect(TerrModpol7M, CaveModpol7M)
areaPolygon(interTC7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(607216.5/5144655)*100 # 
# inter/cave niche
(607216.5/607216.5)*100 # 

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet Terr and Foss at 0.5 cutoff
interTF7M <- raster::intersect(TerrModpol7M, FossModpol7M)
areaPolygon(interTF7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(20840.96/5144655)*100 # 
# inter/foss niche
(20840.96/218512.1)*100 # 

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 #
#inter bet Terr and Sax at 0.5 cutoff
interTS7M <- raster::intersect(TerrModpol7M, SaxModpol7M)
areaPolygon(interTS7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(349552.1/5144655)*100 # 
# inter/sax niche
(349552.1/350360.8)*100 # 

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpol7M) / 1e6 # 
areaPolygon(CaveModpol7M) / 1e6 # 
#inter bet Water and Cave at 0.5 cutoff
interWCSN <- raster::intersect(AquaModpol7M, CaveModpol7M)
areaPolygon(interWCSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/water niche
(591103.6/3182005)*100 # 
# inter/cave niche
(591103.6/607216.5)*100 # 

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet Water and Foss at 0.5 cutoff
interWFSN <- raster::intersect(AquaModpol7M, FossModpol7M)
areaPolygon(interWFSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/water niche
(0/3182005)*100 # 
# inter/foss niche
(0/218512.1)*100 # 

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet Water and Sax at 0.5 cutoff
interWSSN <- raster::intersect(AquaModpol7M, SaxModpol7M)
areaPolygon(interWSSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/water niche
(339949.3/3182005)*100 # 
# inter/Sax niche
(339949.3/350360.8)*100 # 

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet cave and foss at 0.5 cutoff
interCFSN <- raster::intersect(CaveModpol7M, FossModpol7M)
areaPolygon(interCFSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/cave niche
(0/607216.5)*100 # 
# inter/foss niche
(0/218512.1)*100 # 

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet cave and sax at 0.5 cutoff
interCSSN <- raster::intersect(CaveModpol7M, SaxModpol7M)
areaPolygon(interCSSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/cave niche
(274959.1/607216.5)*100 # 
# inter/sax niche
(274959.1/350360.8)*100 # 

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet foss and sax at 0.5 cutoff
interFSSN <- raster::intersect(FossModpol7M, SaxModpol7M)
areaPolygon(interFSSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/foss niche
(0/218512.1)*100 # 
# inter/sax niche
(0/350360.8)*100 # 


#######################################
#### distribution inter with niche ###
#######################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 5076573
interADTS <- raster::intersect(ArbPoly7M, TerrModpol7M)
areaPolygon(interADTS) / 1e6 # 209912.3
(209912.3/5076573)*100 # 4.134921
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpol7M) / 1e6 #  
interADCS <- raster::intersect(ArbPoly7M, CaveModpol7M)
areaPolygon(interADCS) / 1e6 # 
(0/607216.5)*100 # 
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interADSS <- raster::intersect(ArbPoly7M, SaxModpol7M)
areaPolygon(interADSS) / 1e6 # 
(4.10787/350360.8)*100 #
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interADWS <- raster::intersect(ArbPoly7M, AquaModpol7M)
# this is zero
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 
interADFS <- raster::intersect(ArbPoly7M, FossModpol7M)
areaPolygon(interADFS) / 1e6 # 
(88711.5/218512.1)*100 # 
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 1094396
interTDAS <- raster::intersect(TerrPoly7M, ArbModpol7M)
areaPolygon(interTDAS) / 1e6 # 534436.7
(534436.7/1094396)*100 # 48.83394
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpol7M) / 1e6 # 
interTDCS <- raster::intersect(TerrPoly7M, CaveModpol7M)
areaPolygon(interTDCS) / 1e6 #
(602447.2/607216.5)*100 # 
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interTDSS <- raster::intersect(TerrPoly7M, SaxModpol7M)
areaPolygon(interTDSS) / 1e6 # 
(348090.3/350360.8)*100 # 
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interTDWS <- raster::intersect(TerrPoly7M, AquaModpol7M)
areaPolygon(interTDWS) / 1e6 # 2768780
(2768780/3182005)*100 # 87.01
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interTDFS <- raster::intersect(TerrPoly7M, FossModpol7M)
areaPolygon(interTDFS) / 1e6 # 
(60939.65/218512.1)*100 # 
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interWDAS <- raster::intersect(AquaPoly7M, ArbModpol7M)
areaPolygon(interWDAS) / 1e6 # 
(1180.498/1051674)*100 # 
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpol7M) / 1e6 #  
interWDCS <- raster::intersect(AquaPoly7M, CaveModpol7M)
areaPolygon(interWDCS) / 1e6 # 
(502203.4/607216.5)*100 # 
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interWDSS <- raster::intersect(AquaPoly7M, SaxModpol7M)
areaPolygon(interWDSS) / 1e6 # 
(346713.5/350360.8)*100 #
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interWDTS <- raster::intersect(AquaPoly7M, TerrModpol7M)
areaPolygon(interWDTS) / 1e6 # 
(2616476/5144655)*100 # 
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interWDFS <- raster::intersect(AquaPoly7M, FossModpol7M)
areaPolygon(interWDFS) / 1e6 # 
(0/218512.1)*100 # 
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interCDAS <- raster::intersect(CavePoly7M, ArbModpol7M)
areaPolygon(interCDAS) / 1e6 # 
(428.6043/1051674)*100 #
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interCDWS <- raster::intersect(CavePoly7M, AquaModpol7M)
areaPolygon(interCDWS) / 1e6 # 
(444352.2/3182005)*100 #
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interCDSS <- raster::intersect(CavePoly7M, SaxModpol7M)
areaPolygon(interCDSS) / 1e6 # 
(185022.1/350360.8)*100 # 
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interCDTS <- raster::intersect(CavePoly7M, TerrModpol7M)
areaPolygon(interCDTS) / 1e6 # 
(459171.9/5144655)*100 # 
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interCDFS <- raster::intersect(CavePoly7M, FossModpol7M)
areaPolygon(interCDFS) / 1e6 # 
(0/218512.1)*100 # 
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interFDAS <- raster::intersect(FossPoly7M, ArbModpol7M)
areaPolygon(interFDAS) / 1e6 # 
(58522.12/1051674)*100 # 
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interFDWS <- raster::intersect(FossPoly7M, AquaModpol7M)
# this is zero
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interFDSS <- raster::intersect(FossPoly7M, SaxModpol7M)
# this is zero
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interFDTS <- raster::intersect(FossPoly7M, TerrModpol7M)
areaPolygon(interFDTS) / 1e6 # 
(2350.88/5144655)*100 # 
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpol7M) / 1e6 # 
interFDCS <- raster::intersect(FossPoly7M, CaveModpol7M)
# this is zero
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interSDAS <- raster::intersect(SaxPoly7M, ArbModpol7M)
areaPolygon(interSDAS) / 1e6 # 899.1347
(899.1347/1051674)*100 # 0.08549557
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interSDWS <- raster::intersect(SaxPoly7M, AquaModpol7M)
areaPolygon(interSDWS) / 1e6 # 170665
(170665/3182005)*100 # 5.363442
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interSDFS <- raster::intersect(SaxPoly7M, FossModpol7M)
areaPolygon(interSDFS) / 1e6 # 173.1881
(173.1881/218512.1)*100 # 0.0792579
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpol7M) / 1e6 #
interSDTS <- raster::intersect(SaxPoly7M, TerrModpol7M)
areaPolygon(interSDTS) / 1e6 # 177273
(177273/5144655)*100 #  3.44577
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpol7M) / 1e6 # 
interSDCS <- raster::intersect(SaxPoly7M, CaveModpol7M)
areaPolygon(interSDCS) / 1e6 # 125484.7
(125484.7/607216.5)*100 # 20.66556
# this is sax spp present where cave can live

##################
# niche equivalency
##################

ArbPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_7M/chull.shp")
TerrPoints7M <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_7M/chull.shp")
ArbDF7M <- data.frame(ArbPoints7M)
ArbDF7M <- ArbDF7M[,1:2]
TerrDF7M <- data.frame(TerrPoints7M)
TerrDF7M <- TerrDF7M[,1:2]

ArbPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_7L/chull.shp")
TerrPoints7L <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_7L/chull.shp")
ArbDF7L <- data.frame(ArbPoints7L)
ArbDF7L <- ArbDF7L[,1:2]
TerrDF7L <- data.frame(TerrPoints7L)
TerrDF7L <- TerrDF7L[,1:2]

# climate data
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogetherFinalGTiff.gri')
# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors <- crop(x = ClimateData, y = geographic.extent)


IDTest7M <- dismo::nicheEquivalency(sp1=ArbDF7M, sp2=TerrDF7M, predictors = predictors,
                                  n=100, model=maxent, verbose=T)
chars7M <- capture.output(print(IDTest7M))
writeLines(chars7M, con = file("output_7M.txt"))

###

IDTest7L <- dismo::nicheEquivalency(sp1=ArbDF7L, sp2=TerrDF7L, predictors = predictors,
                                    n=100, model=maxent, verbose=T)
chars7L <- capture.output(print(IDTest7L))
writeLines(chars7L, con = file("output_7L.txt"))

# OVERLAP 7L
# change all M to L

# niche overlap 
# arb and terr
dismo::nicheOverlap(ArbMod7LSS, TerrMod7LSS, stat='I', mask=T, checkNegatives = T)
# 0.1987123
dismo::nicheOverlap(ArbMod7LSS, TerrMod7LSS, stat='D', mask=T, checkNegatives = T)
# 0.09864307
dismo::nicheOverlap(ArbMod7L, TerrMod7L, stat='I', mask=T, checkNegatives = T)
# 0.6103991
dismo::nicheOverlap(ArbMod7L, TerrMod7L, stat='D', mask=T, checkNegatives = T)
# 0.3451756

# arb and water
dismo::nicheOverlap(ArbMod7MSS, AquaMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, AquaMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, AquaMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, AquaMod7M, stat='D', mask=T, checkNegatives = T)
# 

# arb and cave
dismo::nicheOverlap(ArbMod7MSS, CaveMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, CaveMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, CaveMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, CaveMod7M, stat='D', mask=T, checkNegatives = T)
# 

# arb and foss
dismo::nicheOverlap(ArbMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# arb and sax
dismo::nicheOverlap(ArbMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(ArbMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and water
dismo::nicheOverlap(TerrMod7MSS, AquaMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, AquaMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, AquaMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, AquaMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and cave
dismo::nicheOverlap(TerrMod7MSS, CaveMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, CaveMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, CaveMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, CaveMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and foss 
dismo::nicheOverlap(TerrMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# terr and sax
dismo::nicheOverlap(TerrMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(TerrMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# water and cave
dismo::nicheOverlap(AquaMod7MSS, CaveMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7MSS, CaveMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, CaveMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, CaveMod7M, stat='D', mask=T, checkNegatives = T)
# 

# water and foss
dismo::nicheOverlap(AquaMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# water and sax
dismo::nicheOverlap(AquaMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(AquaMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# cave and fos
dismo::nicheOverlap(CaveMod7MSS, FossMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7MSS, FossMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, FossMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, FossMod7M, stat='D', mask=T, checkNegatives = T)
# 

# cave and sax
dismo::nicheOverlap(CaveMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(CaveMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

# foss and sax
dismo::nicheOverlap(FossMod7MSS, SaxMod7MSS, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(FossMod7MSS, SaxMod7MSS, stat='D', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(FossMod7M, SaxMod7M, stat='I', mask=T, checkNegatives = T)
# 
dismo::nicheOverlap(FossMod7M, SaxMod7M, stat='D', mask=T, checkNegatives = T)
# 

###
# arb poly intersect with arb niche
# ArbPoly7M area
areaPolygon(ArbPoly7M) / 1e6 # 593188.5
# ArbModpol7M area
areaPolygon(ArbModpol7M) / 1e6 # 1094396
# intersection between them
AA7M <- raster::intersect(ArbPoly7M,ArbModpol7M)
areaPolygon(AA7M) / 1e6 # 438276.4
# percent of the polygon explained by the niche
(438276.4/593188.5)*100 # 73.88484

###
# terr poly intersect with terr niche
# TerrPoly7M area
areaPolygon(TerrPoly7M) / 1e6 # 4547682
# TerrModpol7M area
areaPolygon(TerrModpol7M) / 1e6 # 5076573
# intersection between them
TT7M <- raster::intersect(TerrPoly7M,TerrModpol7M)
areaPolygon(TT7M) / 1e6 # 3728410
# percent of the polygon explained by the niche
(3728410/4547682)*100 # 81.98484

###
# water poly intersect with water niche
# AquaPoly7M area
areaPolygon(AquaPoly7M) / 1e6 # 
# AquaModpol7M area
areaPolygon(AquaModpol7M) / 1e6 # 
# intersection between them
WW7M <- raster::intersect(AquaPoly7M,AquaModpol7M)
areaPolygon(WW7M) / 1e6 # 
# percent of the polygon explained by the niche
(2621246/2829823 )*100 # 

###
# cave poly intersect with cave niche
# CavePoly7M area
areaPolygon(CavePoly7M) / 1e6 # 
# CaveModpol7M area
areaPolygon(CaveModpol7M) / 1e6 # 
# intersection between them
CC7M <- raster::intersect(CavePoly7M,CaveModpol7M)
areaPolygon(CC7M) / 1e6 # 
# percent of the polygon explained by the niche
(411796.6/492224)*100 # 

###
# foss poly intersect with foss niche
# FossPoly7M area
areaPolygon(FossPoly7M) / 1e6 # 
# FossModpol7M area
areaPolygon(FossModpol7M) / 1e6 # 
# intersection between them
FF7M <- raster::intersect(FossPoly7M,FossModpol7M)
areaPolygon(FF7M) / 1e6 # 
# percent of the polygon explained by the niche
(86868.56/110435)*100 # 

###
# sax poly intersect with sax niche
# SaxPoly7M area
areaPolygon(SaxPoly7M) / 1e6 # 
# SaxModpol7M area
areaPolygon(SaxModpol7M) / 1e6 # 
# intersection between them
SS7M <- raster::intersect(SaxPoly7M,SaxModpol7M)
areaPolygon(SS7M) / 1e6 # 
# percent of the polygon explained by the niche
(151507/180106.9)*100 # 

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 1094396
areaPolygon(TerrModpol7M) / 1e6 # 5076573
#inter bet Arb and Terr at 0.5 cutoff
interAT7M <- raster::intersect(ArbModpol7M, TerrModpol7M)
areaPolygon(interAT7M) / 1e6 # 418788
# percent of each niche that overlaps with the other by area
# inter/arb niche
(418788/1094396)*100 # 38.26659
# inter/terr niche
(418788/5076573)*100 # 8.249423

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(AquaModpol7M) / 1e6 # 
#inter bet Arb and water at 0.5 cutoff
interAW7M <- raster::intersect(ArbModpol7M, AquaModpol7M)
areaPolygon(interAW7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(3863.806/1051674)*100 # 
# inter/water niche
(3863.806/3182005)*100 # 

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(CaveModpol7M) / 1e6 # 
#inter bet Arb and cave at 0.5 cutoff
interAC7M <- raster::intersect(ArbModpol7M, CaveModpol7M)
areaPolygon(interAC7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/1051674)*100 # 
# inter/cave niche
(0/607216.5)*100 # 

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet Arb and foss at 0.5 cutoff
interAF7M <- raster::intersect(ArbModpol7M, FossModpol7M)
areaPolygon(interAF7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(118243.9/1051674)*100 # 
# inter/foss niche
(118243.9/218512.1)*100 # 

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet Arb and sax at 0.5 cutoff
interAS7M <- raster::intersect(ArbModpol7M, SaxModpol7M)
areaPolygon(interAS7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/1051674)*100 # 
# inter/sax niche
(0/350360.8)*100 # 

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(AquaModpol7M) / 1e6 # 
#inter bet Terr and Water at 0.5 cutoff
interTW7M <- raster::intersect(TerrModpol7M, AquaModpol7M)
areaPolygon(interTW7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2989729/5144655)*100 # 
# inter/water niche
(2989729/3182005)*100 # 

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(CaveModpol7M) / 1e6 # 
#inter bet Terr and Cave at 0.5 cutoff
interTC7M <- raster::intersect(TerrModpol7M, CaveModpol7M)
areaPolygon(interTC7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(607216.5/5144655)*100 # 
# inter/cave niche
(607216.5/607216.5)*100 # 

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet Terr and Foss at 0.5 cutoff
interTF7M <- raster::intersect(TerrModpol7M, FossModpol7M)
areaPolygon(interTF7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(20840.96/5144655)*100 # 
# inter/foss niche
(20840.96/218512.1)*100 # 

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet Terr and Sax at 0.5 cutoff
interTS7M <- raster::intersect(TerrModpol7M, SaxModpol7M)
areaPolygon(interTS7M) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/terr niche
(349552.1/5144655)*100 # 
# inter/sax niche
(349552.1/350360.8)*100 # 

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpol7M) / 1e6 # 
areaPolygon(CaveModpol7M) / 1e6 # 
#inter bet Water and Cave at 0.5 cutoff
interWCSN <- raster::intersect(AquaModpol7M, CaveModpol7M)
areaPolygon(interWCSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/water niche
(591103.6/3182005)*100 # 
# inter/cave niche
(591103.6/607216.5)*100 # 

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet Water and Foss at 0.5 cutoff
interWFSN <- raster::intersect(AquaModpol7M, FossModpol7M)
areaPolygon(interWFSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/water niche
(0/3182005)*100 # 
# inter/foss niche
(0/218512.1)*100 # 

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet Water and Sax at 0.5 cutoff
interWSSN <- raster::intersect(AquaModpol7M, SaxModpol7M)
areaPolygon(interWSSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/water niche
(339949.3/3182005)*100 # 
# inter/Sax niche
(339949.3/350360.8)*100 # 

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpol7M) / 1e6 # 
areaPolygon(FossModpol7M) / 1e6 # 
#inter bet cave and foss at 0.5 cutoff
interCFSN <- raster::intersect(CaveModpol7M, FossModpol7M)
areaPolygon(interCFSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/cave niche
(0/607216.5)*100 # 
# inter/foss niche
(0/218512.1)*100 # 

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet cave and sax at 0.5 cutoff
interCSSN <- raster::intersect(CaveModpol7M, SaxModpol7M)
areaPolygon(interCSSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/cave niche
(274959.1/607216.5)*100 # 
# inter/sax niche
(274959.1/350360.8)*100 # 

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpol7M) / 1e6 # 
areaPolygon(SaxModpol7M) / 1e6 # 
#inter bet foss and sax at 0.5 cutoff
interFSSN <- raster::intersect(FossModpol7M, SaxModpol7M)
areaPolygon(interFSSN) / 1e6 # 
# percent of each niche that overlaps with the other by area
# inter/foss niche
(0/218512.1)*100 # 
# inter/sax niche
(0/350360.8)*100 # 


#######################################
#### distribution inter with niche ###
#######################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpol7L) / 1e6 # 4590369
interADTS <- raster::intersect(ArbPoly7L, TerrModpol7L)
areaPolygon(interADTS) / 1e6 # 239518.7
(239518.7/4590369)*100 # 5.217853
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpol7M) / 1e6 #  
interADCS <- raster::intersect(ArbPoly7M, CaveModpol7M)
areaPolygon(interADCS) / 1e6 # 0
(0/607216.5)*100 # 0
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interADSS <- raster::intersect(ArbPoly7M, SaxModpol7M)
areaPolygon(interADSS) / 1e6 # 
(4.10787/350360.8)*100 # 
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interADWS <- raster::intersect(ArbPoly7M, AquaModpol7M)
# this is zero
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interADFS <- raster::intersect(ArbPoly7M, FossModpol7M)
areaPolygon(interADFS) / 1e6 # 
(88711.5/218512.1)*100 # 
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpol7L) / 1e6 # 1283715
interTDAS <- raster::intersect(TerrPoly7L, ArbModpol7L)
areaPolygon(interTDAS) / 1e6 # 398310.4
(398310.4/1283715)*100 # 31.02795
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpol7M) / 1e6 # 
interTDCS <- raster::intersect(TerrPoly7M, CaveModpol7M)
areaPolygon(interTDCS) / 1e6 # 
(602447.2/607216.5)*100 # 
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interTDSS <- raster::intersect(TerrPoly7M, SaxModpol7M)
areaPolygon(interTDSS) / 1e6 # 
(348090.3/350360.8)*100 # 
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interTDWS <- raster::intersect(TerrPoly7M, AquaModpol7M)
areaPolygon(interTDWS) / 1e6 # 
(2768780/3182005)*100 # 
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interTDFS <- raster::intersect(TerrPoly7M, FossModpol7M)
areaPolygon(interTDFS) / 1e6 #
(60939.65/218512.1)*100 # 
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interWDAS <- raster::intersect(AquaPoly7M, ArbModpol7M)
areaPolygon(interWDAS) / 1e6 # 
(1180.498/1051674)*100 # 
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpol7M) / 1e6 #  
interWDCS <- raster::intersect(AquaPoly7M, CaveModpol7M)
areaPolygon(interWDCS) / 1e6 # 
(502203.4/607216.5)*100 # 
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interWDSS <- raster::intersect(AquaPoly7M, SaxModpol7M)
areaPolygon(interWDSS) / 1e6 # 
(346713.5/350360.8)*100 # 
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interWDTS <- raster::intersect(AquaPoly7M, TerrModpol7M)
areaPolygon(interWDTS) / 1e6 # 
(2616476/5144655)*100 # 
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interWDFS <- raster::intersect(AquaPoly7M, FossModpol7M)
areaPolygon(interWDFS) / 1e6 # 
(0/218512.1)*100 # 
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interCDAS <- raster::intersect(CavePoly7M, ArbModpol7M)
areaPolygon(interCDAS) / 1e6 # 
(428.6043/1051674)*100 # 
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interCDWS <- raster::intersect(CavePoly7M, AquaModpol7M)
areaPolygon(interCDWS) / 1e6 # 
(444352.2/3182005)*100 # 
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interCDSS <- raster::intersect(CavePoly7M, SaxModpol7M)
areaPolygon(interCDSS) / 1e6 # 
(185022.1/350360.8)*100 # 
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interCDTS <- raster::intersect(CavePoly7M, TerrModpol7M)
areaPolygon(interCDTS) / 1e6 # 
(459171.9/5144655)*100 # 
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interCDFS <- raster::intersect(CavePoly7M, FossModpol7M)
areaPolygon(interCDFS) / 1e6 # 
(0/218512.1)*100 # 
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interFDAS <- raster::intersect(FossPoly7M, ArbModpol7M)
areaPolygon(interFDAS) / 1e6 # 
(58522.12/1051674)*100 # 
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interFDWS <- raster::intersect(FossPoly7M, AquaModpol7M)
# this is zero
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpol7M) / 1e6 # 
interFDSS <- raster::intersect(FossPoly7M, SaxModpol7M)
# this is zero
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interFDTS <- raster::intersect(FossPoly7M, TerrModpol7M)
areaPolygon(interFDTS) / 1e6 # 
(2350.88/5144655)*100 # 
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpol7M) / 1e6 # 
interFDCS <- raster::intersect(FossPoly7M, CaveModpol7M)
# this is zero
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpol7M) / 1e6 # 
interSDAS <- raster::intersect(SaxPoly7M, ArbModpol7M)
areaPolygon(interSDAS) / 1e6 # 
(899.1347/1051674)*100 # 
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpol7M) / 1e6 # 
interSDWS <- raster::intersect(SaxPoly7M, AquaModpol7M)
areaPolygon(interSDWS) / 1e6 # 
(170665/3182005)*100 # 
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpol7M) / 1e6 # 
interSDFS <- raster::intersect(SaxPoly7M, FossModpol7M)
areaPolygon(interSDFS) / 1e6 # 
(173.1881/218512.1)*100 # 
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpol7M) / 1e6 # 
interSDTS <- raster::intersect(SaxPoly7M, TerrModpol7M)
areaPolygon(interSDTS) / 1e6 # 
(177273/5144655)*100 #  
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpol7M) / 1e6 # 
interSDCS <- raster::intersect(SaxPoly7M, CaveModpol7M)
areaPolygon(interSDCS) / 1e6 # 
(125484.7/607216.5)*100 # 
# this is sax spp present where cave can live





###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###  for the microhabitat STRICT    ##############
###################################################

# USES OUTPUT OF THE MAXENT MODEL
# MAKES A POLYGON OF A CERTAIN SUITABILITY THRESHOLD
# ASKS HOW MUCH OF THE POLYGON OVERLAP IS IN THE SUITABILITY BY AREA

###############################

# packages
library(phyloclim); library(geosphere); library(raster)
library(rgdal); library(rgeos); library(dismo)

# load the maxent predictions
ArbModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Strict/Predictions/ArbMod_prediction_strict.grd")
TerrModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Strict/Predictions/TerrMod_prediction_strict.grd")
AquaModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Strict/Predictions/AquaMod_prediction_strict.grd")
CaveModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Strict/Predictions/CaveMod_prediction_strict.grd")
FossModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Strict/Predictions/FossMod_prediction_strict.grd")
SaxModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Strict/Predictions/SaxMod_prediction_strict.grd")

# load the polygons
ArbPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/ArbPoly_strict/chull.shp")
TerrPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/TerrPoly_strict/chull.shp")
AquaPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/AquaPoly_strict/chull.shp")
CavePolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/CavePoly_strict/chull.shp")
FossPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/FossPoly_strict/chull.shp")
SaxPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/SaxPoly_strict/chull.shp")

# load the points
ArbPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Arb_Points_strict_ressmall/chull.shp")
TerrPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Terr_Points_strict_ressmall/chull.shp")
AquaPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Aqua_Points_strict_ressmall/chull.shp")
CavePointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Cave_Points_strict_ressmall/chull.shp")
FossPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Foss_Points_strict_ressmall/chull.shp")
SaxPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Points/Sax_Points_strict_ressmall/chull.shp")

# give them a threshold suitability score
ArbModSSS <- ArbModS > 0.5
TerrModSSS <- TerrModS > 0.5
AquaModSSS <- AquaModS > 0.5
CaveModSSS <- CaveModS > 0.5
FossModSSS <- FossModS > 0.5
SaxModSSS <- SaxModS > 0.5

# turn it into a polygon
ArbModpolS <- rasterToPolygons(ArbModSSS,function(x) x == 1,dissolve=T)
TerrModpolS <- rasterToPolygons(TerrModSSS,function(x) x == 1,dissolve=T)
AquaModpolS <- rasterToPolygons(AquaModSSS,function(x) x == 1,dissolve=T)
CaveModpolS <- rasterToPolygons(CaveModSSS,function(x) x == 1,dissolve=T)
FossModpolS <- rasterToPolygons(FossModSSS,function(x) x == 1,dissolve=T)
SaxModpolS <- rasterToPolygons(SaxModSSS,function(x) x == 1,dissolve=T)

# get the area of the niche polygon with a 0.5 cutoff
areaPolygon(ArbModpolS) / 1e6
# 1051674
areaPolygon(TerrModpolS) / 1e6
# 5144655
areaPolygon(AquaModpolS) / 1e6
# 3182005
areaPolygon(CaveModpolS) / 1e6
# 607216.5
areaPolygon(FossModpolS) / 1e6
# 218512.1
areaPolygon(SaxModpolS) / 1e6
# 350360.8

# niche overlap 
# arb and terr
dismo::nicheOverlap(ArbModSSS, TerrModSSS, stat='I', mask=T, checkNegatives = T)
# 0.1933864 *
dismo::nicheOverlap(ArbModSSS, TerrModSSS, stat='D', mask=T, checkNegatives = T)
# 0.08340265 *
dismo::nicheOverlap(ArbModS, TerrModS, stat='I', mask=T, checkNegatives = T)
# 0.595809 *
dismo::nicheOverlap(ArbModS, TerrModS, stat='D', mask=T, checkNegatives = T)
# 0.3311615

# arb and water
dismo::nicheOverlap(ArbModSSS, AquaModSSS, stat='I', mask=T, checkNegatives = T)
# 0.002
dismo::nicheOverlap(ArbModSSS, AquaModSSS, stat='D', mask=T, checkNegatives = T)
# 0.001
dismo::nicheOverlap(ArbModS, AquaModS, stat='I', mask=T, checkNegatives = T)
# 0.259
dismo::nicheOverlap(ArbModS, AquaModS, stat='D', mask=T, checkNegatives = T)
# 0.106

# arb and cave
dismo::nicheOverlap(ArbModSSS, CaveModSSS, stat='I', mask=T, checkNegatives = T)
# 4.44e-16
dismo::nicheOverlap(ArbModSSS, CaveModSSS, stat='D', mask=T, checkNegatives = T)
# 4.44e-16
dismo::nicheOverlap(ArbModS, CaveModS, stat='I', mask=T, checkNegatives = T)
# 0.200
dismo::nicheOverlap(ArbModS, CaveModS, stat='D', mask=T, checkNegatives = T)
# 0.063

# arb and foss
dismo::nicheOverlap(ArbModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0.240
dismo::nicheOverlap(ArbModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0.104
dismo::nicheOverlap(ArbModS, FossModS, stat='I', mask=T, checkNegatives = T)
# 0.707
dismo::nicheOverlap(ArbModS, FossModS, stat='D', mask=T, checkNegatives = T)
# 0.397

# arb and sax
dismo::nicheOverlap(ArbModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0
dismo::nicheOverlap(ArbModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 3.33e-16
dismo::nicheOverlap(ArbModS, SaxModS, stat='I', mask=T, checkNegatives = T)
# 0.278
dismo::nicheOverlap(ArbModS, SaxModS, stat='D', mask=T, checkNegatives = T)
# 0.110

# terr and water
dismo::nicheOverlap(TerrModSSS, AquaModSSS, stat='I', mask=T, checkNegatives = T)
# 0.730
dismo::nicheOverlap(TerrModSSS, AquaModSSS, stat='D', mask=T, checkNegatives = T)
# 0.572
dismo::nicheOverlap(TerrModS, AquaModS, stat='I', mask=T, checkNegatives = T)
# 0.805
dismo::nicheOverlap(TerrModS, AquaModS, stat='D', mask=T, checkNegatives = T)
# 0.552

# terr and cave
dismo::nicheOverlap(TerrModSSS, CaveModSSS, stat='I', mask=T, checkNegatives = T)
# 0.335
dismo::nicheOverlap(TerrModSSS, CaveModSSS, stat='D', mask=T, checkNegatives = T)
# 0.112
dismo::nicheOverlap(TerrModS, CaveModS, stat='I', mask=T, checkNegatives = T)
# 0.555
dismo::nicheOverlap(TerrModS, CaveModS, stat='D', mask=T, checkNegatives = T)
# 0.250

# terr and foss 
dismo::nicheOverlap(TerrModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0.019
dismo::nicheOverlap(TerrModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0.003
dismo::nicheOverlap(TerrModS, FossModS, stat='I', mask=T, checkNegatives = T)
# 0.45
dismo::nicheOverlap(TerrModS, FossModS, stat='D', mask=T, checkNegatives = T)
# 0.187

# terr and sax
dismo::nicheOverlap(TerrModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.253
dismo::nicheOverlap(TerrModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.064
dismo::nicheOverlap(TerrModS, SaxModS, stat='I', mask=T, checkNegatives = T)
# 0.59
dismo::nicheOverlap(TerrModS, SaxModS, stat='D', mask=T, checkNegatives = T)
# 0.296

# water and cave
dismo::nicheOverlap(AquaModSSS, CaveModSSS, stat='I', mask=T, checkNegatives = T)
# 0.417
dismo::nicheOverlap(AquaModSSS, CaveModSSS, stat='D', mask=T, checkNegatives = T)
# 0.178
dismo::nicheOverlap(AquaModS, CaveModS, stat='I', mask=T, checkNegatives = T)
# 0.676
dismo::nicheOverlap(AquaModS, CaveModS, stat='D', mask=T, checkNegatives = T)
# 0.369

# water and foss
dismo::nicheOverlap(AquaModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 0
dismo::nicheOverlap(AquaModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 0
dismo::nicheOverlap(AquaModS, FossModS, stat='I', mask=T, checkNegatives = T)
# 0.175
dismo::nicheOverlap(AquaModS, FossModS, stat='D', mask=T, checkNegatives = T)
# 0.04

# water and sax
dismo::nicheOverlap(AquaModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.31
dismo::nicheOverlap(AquaModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.101
dismo::nicheOverlap(AquaModS, SaxModS, stat='I', mask=T, checkNegatives = T)
# 0.651
dismo::nicheOverlap(AquaModS, SaxModS, stat='D', mask=T, checkNegatives = T)
# 0.370

# cave and foss
dismo::nicheOverlap(CaveModSSS, FossModSSS, stat='I', mask=T, checkNegatives = T)
# 1.11e-16
dismo::nicheOverlap(CaveModSSS, FossModSSS, stat='D', mask=T, checkNegatives = T)
# 1.11e-16
dismo::nicheOverlap(CaveModS, FossModS, stat='I', mask=T, checkNegatives = T)
# 0.102
dismo::nicheOverlap(CaveModS, FossModS, stat='D', mask=T, checkNegatives = T)
# 0.022

# cave and sax
dismo::nicheOverlap(CaveModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0.592
dismo::nicheOverlap(CaveModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 0.447
dismo::nicheOverlap(CaveModS, SaxModS, stat='I', mask=T, checkNegatives = T)
# 0.883
dismo::nicheOverlap(CaveModS, SaxModS, stat='D', mask=T, checkNegatives = T)
# 0.644

# foss and sax
dismo::nicheOverlap(FossModSSS, SaxModSSS, stat='I', mask=T, checkNegatives = T)
# 0
dismo::nicheOverlap(FossModSSS, SaxModSSS, stat='D', mask=T, checkNegatives = T)
# 1.11e-16
dismo::nicheOverlap(FossModS, SaxModS, stat='I', mask=T, checkNegatives = T)
# 0.14
dismo::nicheOverlap(FossModS, SaxModS, stat='D', mask=T, checkNegatives = T)
# 0.038

###
# arb poly intersect with arb niche
# ArbPolyS area
areaPolygon(ArbPolyS) / 1e6 # 593188.5*
# ArbModpolS area
areaPolygon(ArbModpolS) / 1e6 # 1051674*
# intersection between them
AAS <- raster::intersect(ArbPolyS,ArbModpolS)
areaPolygon(AAS) / 1e6 # 429296.8
# percent of the polygon explained by the niche
(429296.8/593188.5)*100 # 72.37

###
# terr poly intersect with terr niche
# TerrPolyS area
areaPolygon(TerrPolyS) / 1e6 # 4547682
# TerrModpolS area
areaPolygon(TerrModpolS) / 1e6 # 5144655*
# intersection between them
TTS <- raster::intersect(TerrPolyS,TerrModpolS)
areaPolygon(TTS) / 1e6 # 3744265*
# percent of the polygon explained by the niche
(3744265/4547682)*100 # 82.33

###
# water poly intersect with water niche
# AquaPolyS area
areaPolygon(AquaPolyS) / 1e6 # 2829823 
# AquaModpolS area
areaPolygon(AquaModpolS) / 1e6 # 3182005
# intersection between them
WWS <- raster::intersect(AquaPolyS,AquaModpolS)
areaPolygon(WWS) / 1e6 # 2621246
# percent of the polygon explained by the niche
(2621246/2829823 )*100 # 92.63

###
# cave poly intersect with cave niche
# CavePolyS area
areaPolygon(CavePolyS) / 1e6 # 492224
# CaveModpolS area
areaPolygon(CaveModpolS) / 1e6 # 607216.5
# intersection between them
CCS <- raster::intersect(CavePolyS,CaveModpolS)
areaPolygon(CCS) / 1e6 # 411796.6
# percent of the polygon explained by the niche
(411796.6/492224)*100 # 83.66

###
# foss poly intersect with foss niche
# FossPolyS area
areaPolygon(FossPolyS) / 1e6 # 110435
# FossModpolS area
areaPolygon(FossModpolS) / 1e6 # 218512.1
# intersection between them
FFS <- raster::intersect(FossPolyS,FossModpolS)
areaPolygon(FFS) / 1e6 # 86868.56
# percent of the polygon explained by the niche
(86868.56/110435)*100 # 78.66

###
# sax poly intersect with sax niche
# SaxPolyS area
areaPolygon(SaxPolyS) / 1e6 # 180106.9
# SaxModpolS area
areaPolygon(SaxModpolS) / 1e6 # 350360.8
# intersection between them
SSS <- raster::intersect(SaxPolyS,SaxModpolS)
areaPolygon(SSS) / 1e6 # 151507
# percent of the polygon explained by the niche
(151507/180106.9)*100 # 84.12

###
# intersection area arb-terr niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1051674
areaPolygon(TerrModpolS) / 1e6 # 5144655
#inter bet Arb and Terr at 0.5 cutoff
interATSN <- raster::intersect(ArbModpolS, TerrModpolS)
areaPolygon(interATSN) / 1e6 # 421223.1
# percent of each niche that overlaps with the other by area
# inter/arb niche
(421223.1/1051674)*100 # 40.05
# inter/terr niche
(421223.1/5144655)*100 # 8.18

###
# intersection area arb-water niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1051674
areaPolygon(AquaModpolS) / 1e6 # 3182005
#inter bet Arb and water at 0.5 cutoff
interAWSN <- raster::intersect(ArbModpolS, AquaModpolS)
areaPolygon(interAWSN) / 1e6 # 3863.806
# percent of each niche that overlaps with the other by area
# inter/arb niche
(3863.806/1051674)*100 # 0.3
# inter/water niche
(3863.806/3182005)*100 # 0.12

###
# intersection area arb-cave niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1051674
areaPolygon(CaveModpolS) / 1e6 # 607216.5
#inter bet Arb and cave at 0.5 cutoff
interACSN <- raster::intersect(ArbModpolS, CaveModpolS)
areaPolygon(interACSN) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/1051674)*100 # 0
# inter/cave niche
(0/607216.5)*100 # 0

###
# intersection area arb-foss niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1051674
areaPolygon(FossModpolS) / 1e6 # 218512.1
#inter bet Arb and foss at 0.5 cutoff
interAFSN <- raster::intersect(ArbModpolS, FossModpolS)
areaPolygon(interAFSN) / 1e6 # 118243.9
# percent of each niche that overlaps with the other by area
# inter/arb niche
(118243.9/1051674)*100 # 11.24
# inter/foss niche
(118243.9/218512.1)*100 # 54.11

###
# intersection area arb-sax niches
# Area for the niches
areaPolygon(ArbModpolS) / 1e6 # 1051674
areaPolygon(SaxModpolS) / 1e6 # 350360.8
#inter bet Arb and sax at 0.5 cutoff
interASSN <- raster::intersect(ArbModpolS, SaxModpolS)
areaPolygon(interASSN) / 1e6 # 0 
# percent of each niche that overlaps with the other by area
# inter/arb niche
(0/1051674)*100 # 0
# inter/sax niche
(0/350360.8)*100 # 0

###
# intersection area terr-water niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5144655
areaPolygon(AquaModpolS) / 1e6 # 3182005
#inter bet Terr and Water at 0.5 cutoff
interTWSN <- raster::intersect(TerrModpolS, AquaModpolS)
areaPolygon(interTWSN) / 1e6 # 2989729
# percent of each niche that overlaps with the other by area
# inter/terr niche
(2989729/5144655)*100 # 58.1133
# inter/water niche
(2989729/3182005)*100 # 93.95

###
# intersection area terr-cave niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5144655
areaPolygon(CaveModpolS) / 1e6 # 607216.5
#inter bet Terr and Cave at 0.5 cutoff
interTCSN <- raster::intersect(TerrModpolS, CaveModpolS)
areaPolygon(interTCSN) / 1e6 # 607216.5
# percent of each niche that overlaps with the other by area
# inter/terr niche
(607216.5/5144655)*100 # 11.80
# inter/cave niche
(607216.5/607216.5)*100 # 100

###
# intersection area terr-foss niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5144655
areaPolygon(FossModpolS) / 1e6 # 218512.1
#inter bet Terr and Foss at 0.5 cutoff
interTFSN <- raster::intersect(TerrModpolS, FossModpolS)
areaPolygon(interTFSN) / 1e6 # 20840.96
# percent of each niche that overlaps with the other by area
# inter/terr niche
(20840.96/5144655)*100 # 0.4
# inter/foss niche
(20840.96/218512.1)*100 # 9.53

###
# intersection area terr-Sax niches
# Area for the niches
areaPolygon(TerrModpolS) / 1e6 # 5144655
areaPolygon(SaxModpolS) / 1e6 # 350360.8
#inter bet Terr and Sax at 0.5 cutoff
interTSSN <- raster::intersect(TerrModpolS, SaxModpolS)
areaPolygon(interTSSN) / 1e6 # 349552.1
# percent of each niche that overlaps with the other by area
# inter/terr niche
(349552.1/5144655)*100 # 6.794471
# inter/sax niche
(349552.1/350360.8)*100 # 99.76918

###
# intersection area water-cave niches
# Area for the niches
areaPolygon(AquaModpolS) / 1e6 # 3182005
areaPolygon(CaveModpolS) / 1e6 # 607216.5
#inter bet Water and Cave at 0.5 cutoff
interWCSN <- raster::intersect(AquaModpolS, CaveModpolS)
areaPolygon(interWCSN) / 1e6 # 591103.6
# percent of each niche that overlaps with the other by area
# inter/water niche
(591103.6/3182005)*100 # 18.57
# inter/cave niche
(591103.6/607216.5)*100 # 97.34

###
# intersection area water-foss niches
# Area for the niches
areaPolygon(AquaModpolS) / 1e6 # 3182005
areaPolygon(FossModpolS) / 1e6 # 218512.1
#inter bet Water and Foss at 0.5 cutoff
interWFSN <- raster::intersect(AquaModpolS, FossModpolS)
areaPolygon(interWFSN) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/water niche
(0/3182005)*100 # 0
# inter/foss niche
(0/218512.1)*100 # 0

###
# intersection area water-Sax niches
# Area for the niches
areaPolygon(AquaModpolS) / 1e6 # 3182005
areaPolygon(SaxModpolS) / 1e6 # 350360.8
#inter bet Water and Sax at 0.5 cutoff
interWSSN <- raster::intersect(AquaModpolS, SaxModpolS)
areaPolygon(interWSSN) / 1e6 # 339949.3
# percent of each niche that overlaps with the other by area
# inter/water niche
(339949.3/3182005)*100 # 10.68349
# inter/Sax niche
(339949.3/350360.8)*100 # 97.02835

###
# intersection area cave-foss niches
# Area for the niches
areaPolygon(CaveModpolS) / 1e6 # 607216.5
areaPolygon(FossModpolS) / 1e6 # 218512.1
#inter bet cave and foss at 0.5 cutoff
interCFSN <- raster::intersect(CaveModpolS, FossModpolS)
areaPolygon(interCFSN) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/cave niche
(0/607216.5)*100 # 0
# inter/foss niche
(0/218512.1)*100 # 0

###
# intersection area cave-Sax niches
# Area for the niches
areaPolygon(CaveModpolS) / 1e6 # 607216.5
areaPolygon(SaxModpolS) / 1e6 # 350360.8
#inter bet cave and sax at 0.5 cutoff
interCSSN <- raster::intersect(CaveModpolS, SaxModpolS)
areaPolygon(interCSSN) / 1e6 # 274959.1
# percent of each niche that overlaps with the other by area
# inter/cave niche
(274959.1/607216.5)*100 # 45.28189
# inter/sax niche
(274959.1/350360.8)*100 # 78.47884

###
# intersection area Foss-Sax niches
# Area for the niches
areaPolygon(FossModpolS) / 1e6 # 218512.1
areaPolygon(SaxModpolS) / 1e6 # 350360.8
#inter bet foss and sax at 0.5 cutoff
interFSSN <- raster::intersect(FossModpolS, SaxModpolS)
areaPolygon(interFSSN) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/foss niche
(0/218512.1)*100 # 0
# inter/sax niche
(0/350360.8)*100 # 0


#######################################
#### distribution inter with niche ###
#######################################

# intersect arb polygon with terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5144655*
interADTS <- raster::intersect(ArbPolyS, TerrModpolS)
areaPolygon(interADTS) / 1e6 # 211004.3
(211004.3/5144655)*100 # 4.10
# this is arb spp present where terr can live

# intersect arb polygon with Cave niche model
areaPolygon(CaveModpolS) / 1e6 #  607216.5
interADCS <- raster::intersect(ArbPolyS, CaveModpolS)
areaPolygon(interADCS) / 1e6 # 0
(0/607216.5)*100 # 0
# this is arb spp present where cave can live

# intersect arb polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 350360.8
interADSS <- raster::intersect(ArbPolyS, SaxModpolS)
areaPolygon(interADSS) / 1e6 # 4.10787
(4.10787/350360.8)*100 # 0.001
# this is arb spp present where sax can live

# intersect arb polygon with Water niche model
areaPolygon(AquaModpolS) / 1e6 # 3182005
interADWS <- raster::intersect(ArbPolyS, AquaModpolS)
# this is zero
# this is arb spp present where water can live

# intersect arb polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 218512.1
interADFS <- raster::intersect(ArbPolyS, FossModpolS)
areaPolygon(interADFS) / 1e6 # 88711.5
(88711.5/218512.1)*100 # 40.59798
# this is arb spp present where foss can live

# intersect terr polygon with arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1051674*
interTDAS <- raster::intersect(TerrPolyS, ArbModpolS)
areaPolygon(interTDAS) / 1e6 # 533192.7*
(533192.7/1051674)*100 # 50.699
# this is terr spp present where arb can live

# intersect terr polygon with Cave niche model
areaPolygon(CaveModpolS) / 1e6 # 607216.5
interTDCS <- raster::intersect(TerrPolyS, CaveModpolS)
areaPolygon(interTDCS) / 1e6 # 602447.2
(602447.2/607216.5)*100 # 99.21456
# this is terr spp present where cave can live

# intersect terr polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 350360.8
interTDSS <- raster::intersect(TerrPolyS, SaxModpolS)
areaPolygon(interTDSS) / 1e6 # 348090.3
(348090.3/350360.8)*100 # 99.35195
# this is terr spp present where sax can live

# intersect terr polygon with Water niche model
areaPolygon(AquaModpolS) / 1e6 # 3182005
interTDWS <- raster::intersect(TerrPolyS, AquaModpolS)
areaPolygon(interTDWS) / 1e6 # 2768780
(2768780/3182005)*100 # 87.01
# this is terr spp present where water can live

# intersect terr polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 218512.1
interTDFS <- raster::intersect(TerrPolyS, FossModpolS)
areaPolygon(interTDFS) / 1e6 # 60939.65
(60939.65/218512.1)*100 # 27.88846
# this is terr spp present where foss can live

# intersect Water polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1051674
interWDAS <- raster::intersect(AquaPolyS, ArbModpolS)
areaPolygon(interWDAS) / 1e6 # 1180.498
(1180.498/1051674)*100 # 0.1122494
# this is water spp present where arb can live

# intersect Water polygon with cave niche model
areaPolygon(CaveModpolS) / 1e6 #  607216.5
interWDCS <- raster::intersect(AquaPolyS, CaveModpolS)
areaPolygon(interWDCS) / 1e6 # 502203.4
(502203.4/607216.5)*100 # 82.70582
# this is water spp present where cave can live

# intersect Water polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 350360.8
interWDSS <- raster::intersect(AquaPolyS, SaxModpolS)
areaPolygon(interWDSS) / 1e6 # 346713.5
(346713.5/350360.8)*100 # 98.95899
# this is water spp present where sax can live

# intersect Water polygon with Terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5144655
interWDTS <- raster::intersect(AquaPolyS, TerrModpolS)
areaPolygon(interWDTS) / 1e6 # 2616476
(2616476/5144655)*100 # 50.85814
# this is water spp present where terr can live

# intersect Water polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 218512.1
interWDFS <- raster::intersect(AquaPolyS, FossModpolS)
areaPolygon(interWDFS) / 1e6 # 0
(0/218512.1)*100 # 0
# this is water spp present where foss can live

# intersect Cave polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1051674
interCDAS <- raster::intersect(CavePolyS, ArbModpolS)
areaPolygon(interCDAS) / 1e6 # 428.6043
(428.6043/1051674)*100 # 0.04075448
# this is cave spp present where arb can live

# intersect Cave polygon with water niche model
areaPolygon(AquaModpolS) / 1e6 # 3182005
interCDWS <- raster::intersect(CavePolyS, AquaModpolS)
areaPolygon(interCDWS) / 1e6 # 444352.2
(444352.2/3182005)*100 # 13.96
# this is cave spp present where water can live

# intersect Cave polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 350360.8
interCDSS <- raster::intersect(CavePolyS, SaxModpolS)
areaPolygon(interCDSS) / 1e6 # 185022.1
(185022.1/350360.8)*100 # 52.80902
# this is cave spp present where sax can live

# intersect Cave polygon with terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5144655
interCDTS <- raster::intersect(CavePolyS, TerrModpolS)
areaPolygon(interCDTS) / 1e6 # 459171.9
(459171.9/5144655)*100 # 8.925222
# this is cave spp present where terr can live

# intersect Cave polygon with foss niche model
areaPolygon(FossModpolS) / 1e6 # 218512.1
interCDFS <- raster::intersect(CavePolyS, FossModpolS)
areaPolygon(interCDFS) / 1e6 # 0
(0/218512.1)*100 # 0
# this is cave spp present where foss can live

# intersect Foss polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1051674
interFDAS <- raster::intersect(FossPolyS, ArbModpolS)
areaPolygon(interFDAS) / 1e6 # 58522.12
(58522.12/1051674)*100 # 5.564664
# this is foss spp present where arb can live

# intersect Foss polygon with water niche model
areaPolygon(AquaModpolS) / 1e6 # 3182005
interFDWS <- raster::intersect(FossPolyS, AquaModpolS)
# this is zero
# this is foss spp present where water can live

# intersect Foss polygon with Sax niche model
areaPolygon(SaxModpolS) / 1e6 # 350360.8
interFDSS <- raster::intersect(FossPolyS, SaxModpolS)
# this is zero
# this is foss spp present where sax can live

# intersect Foss polygon with Terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5144655
interFDTS <- raster::intersect(FossPolyS, TerrModpolS)
areaPolygon(interFDTS) / 1e6 # 2350.88
(2350.88/5144655)*100 # 0.04569558
# this is foss spp present where terr can live

# intersect Foss polygon with Cave niche model
areaPolygon(CaveModpolS) / 1e6 # 607216.5
interFDCS <- raster::intersect(FossPolyS, CaveModpolS)
# this is zero
# this is foss spp present where cave can live

# intersect Sax polygon with Arb niche model
areaPolygon(ArbModpolS) / 1e6 # 1051674
interSDAS <- raster::intersect(SaxPolyS, ArbModpolS)
areaPolygon(interSDAS) / 1e6 # 899.1347
(899.1347/1051674)*100 # 0.08549557
# this is sax spp present where arb can live

# intersect Sax polygon with water niche model
areaPolygon(AquaModpolS) / 1e6 # 3182005
interSDWS <- raster::intersect(SaxPolyS, AquaModpolS)
areaPolygon(interSDWS) / 1e6 # 170665
(170665/3182005)*100 # 5.363442
# this is sax spp present where water can live

# intersect Sax polygon with Foss niche model
areaPolygon(FossModpolS) / 1e6 # 218512.1
interSDFS <- raster::intersect(SaxPolyS, FossModpolS)
areaPolygon(interSDFS) / 1e6 # 173.1881
(173.1881/218512.1)*100 # 0.0792579
# this is sax spp present where foss can live

# intersect Sax polygon with terr niche model
areaPolygon(TerrModpolS) / 1e6 # 5144655
interSDTS <- raster::intersect(SaxPolyS, TerrModpolS)
areaPolygon(interSDTS) / 1e6 # 177273
(177273/5144655)*100 #  3.44577
# this is sax spp present where terr can live

# intersect Sax polygon with cave niche model
areaPolygon(CaveModpolS) / 1e6 # 607216.5
interSDCS <- raster::intersect(SaxPolyS, CaveModpolS)
areaPolygon(interSDCS) / 1e6 # 125484.7
(125484.7/607216.5)*100 # 20.66556
# this is sax spp present where cave can live

##################
# niche equivalency
##################

# just do the ID for arb and terr for strict/lenient/m1/m2/subS/subL

ArbPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_strict_ressmall/chull.shp")
TerrPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_strict_ressmall/chull.shp")
ArbDF <- data.frame(ArbPointsS)
ArbDF <- ArbDF[,1:2]
TerrDF <- data.frame(TerrPointsS)
TerrDF <- TerrDF[,1:2]

# climate data
ClimateData <- stack('./Analysis_Scripts/Chapter3/Climate Data/SDM/AllDataTogetherFinalGTiff.gri')
# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors <- crop(x = ClimateData, y = geographic.extent)


IDTest <- dismo::nicheEquivalency(sp1=ArbDF, sp2=TerrDF, predictors = predictors,
                        n=100, model=maxent, verbose=T)
chars <- capture.output(print(IDTest))
writeLines(chars, con = file("output.txt"))





# nah bitch dont do this below
######################################
# background test
?bg.similarity.test

# long is x and lat is y

# ArbDF in data.frame
ArbDF
class(ArbDF)
colnames(ArbDF)[1] <- "long"
colnames(ArbDF)[2] <- "lat"
ArbDF
ArbDF$spec <- "Arb"
ArbDF
# TerrDF
colnames(TerrDF)[1] <- "long"
colnames(TerrDF)[2] <- "lat"
TerrDF$spec <- "Terr"
TerrDF

All <- rbind(ArbDF,TerrDF)
All
432+3664 # check
All2 <-All[,c("spec","long","lat")]
All2

maxent.exe <- paste(system.file(package="dismo"), 
                    "/java/maxent.jar", sep = "")

species <- c("Arb", "Terr")
sites <- All2
samples <- sites[grep(paste(species, collapse = "|"), sites$spec), ]
data.path <- system.file("extdata", package = "phyloclim")
preds <- stack(predictors)
class(preds)
reps <- 99

if (file.exists(maxent.exe)){
  net <- niche.equivalency.test(samples, preds, reps, maxent.exe)
  net; plot(net)
  bst <- bg.similarity.test(samples, preds, reps, app = maxent.exe)
  bst; plot(bst)
} else {
  message("get a copy of MAXENT (see Details)")
}

open <- raster('R.phyloclim.temp/out/_proj.asc')
open
test <- stack(c(list.files('R.phyloclim.temp/out', full.names = T, pattern = '_proj.asc')))
test
file(fname, "r")



###############################
# path to MAXENT
# --------------
maxent.exe <- paste(system.file(package="dismo"), 
                    "/java/maxent.jar", sep = "")

# a data frame of coordinates where two species 
# have been detected ('presence points') and
# a raster stack of environmental covariables
# --------------------------------------
species <- c("enneaphylla", "laciniata")
data(sites)
samples <- sites[grep(paste(species, collapse = "|"), sites$spec), ]
data.path <- system.file("extdata", package = "phyloclim")
preds <- list.files(path = data.path, pattern = "[.]asc")
preds <- paste(data.path, preds, sep = "/")
preds <- stack(lapply(X = preds, FUN = raster))


# testing against 9 permutations of the data
# -------------------------------------------
reps <- 9

# run hypothesis tests
# --------------------
if (file.exists(maxent.exe)){
  net <- niche.equivalency.test(samples, preds, reps, maxent.exe)
  net; plot(net)
  bst <- bg.similarity.test(samples, preds, reps, app = maxent.exe)
  bst; plot(bst)
} else {
  message("get a copy of MAXENT (see Details)")
}














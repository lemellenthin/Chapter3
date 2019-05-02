###################################################
### RECIPROCOL SUITABILITY ANALYSIS ###############
###  for the substrate strict n lenient###########
###################################################

# USES OUTPUT OF THE MAXENT MODEL
# MAKES A POLYGON OF A CERTAIN SUITABILITY THRESHOLD
# ASKS HOW MUCH OF THE POLYGON OVERLAP IS IN THE SUITABILITY BY AREA

###############################

# packages
library(phyloclim); library(geosphere); library(raster)
library(rgdal); library(rgeos)

# load the maxent predictions
DirtModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/DirtMod_prediction_strict.grd")
VegModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/VegMod_prediction_strict.grd")
RockModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/RockMod_prediction_strict.grd")
WaterModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/WaterMod_prediction_strict.grd")
DirtModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/DirtMod_prediction_lenient.grd")
VegModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/VegMod_prediction_lenient.grd")
RockModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/RockMod_prediction_lenient.grd")
WaterModL <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/WaterMod_prediction_lenient.grd")

# load the polygons
DirtPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/DirtPoly_strict/chull.shp")
VegPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/VegPoly_strict/chull.shp")
RockPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/RockPoly_strict/chull.shp")
WaterPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/WaterPoly_strict/chull.shp")
DirtPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/DirtPoly_lenient/chull.shp")
VegPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/VegPoly_lenient/chull.shp")
RockPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/RockPoly_lenient/chull.shp")
WaterPolyL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Polygons/WaterPoly_lenient/chull.shp")

# load the points
DirtPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Dirt_Points_strict/chull.shp")
VegPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Veg_Points_strict/chull.shp")
RockPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Rock_Points_strict/chull.shp")
WaterPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Water_Points_strict/chull.shp")
DirtPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Dirt_Points_lenient/chull.shp")
VegPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Veg_Points_lenient/chull.shp")
RockPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Rock_Points_lenient/chull.shp")
WaterPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Water_Points_lenient/chull.shp")

# give them a threshold suitability score
DirtModSSS <- DirtModS > 0.5
VegModSSS <- VegModS > 0.5
RockModSSS <- RockModS > 0.5
WaterModSSS <- WaterModS > 0.5
DirtModLSS <- DirtModL > 0.5
VegModLSS <- VegModL > 0.5
RockModLSS <- RockModL > 0.5
WaterModLSS <- WaterModL > 0.5

# turn it into a polygon
DirtModpolS <- rasterToPolygons(DirtModSSS,function(x) x == 1,dissolve=T)
VegModpolS <- rasterToPolygons(VegModSSS,function(x) x == 1,dissolve=T)
RockModpolS <- rasterToPolygons(RockModSSS,function(x) x == 1,dissolve=T)
WaterModpolS <- rasterToPolygons(WaterModSSS,function(x) x == 1,dissolve=T)
DirtModpolL <- rasterToPolygons(DirtModLSS,function(x) x == 1,dissolve=T)
VegModpolL <- rasterToPolygons(VegModLSS,function(x) x == 1,dissolve=T)
RockModpolL <- rasterToPolygons(RockModLSS,function(x) x == 1,dissolve=T)
WaterModpolL <- rasterToPolygons(WaterModLSS,function(x) x == 1,dissolve=T)

#
#maybe <- extract(ArbSS, ArbPoly)
#summary(maybe[[1]])

# get the area of the niche polygon
areaPolygon(DirtModpolS) / 1e6
# 5987066
areaPolygon(VegModpolS) / 1e6
# 3141018
areaPolygon(RockModpolS) / 1e6
#  1517493
areaPolygon(WaterModpolS) / 1e6
# 3602891

areaPolygon(DirtModpolL) / 1e6
# 6250365
areaPolygon(VegModpolL) / 1e6
# 3013852
areaPolygon(RockModpolL) / 1e6
# 2510470
areaPolygon(WaterModpolL) / 1e6
# 3717400


# niche overlap 
# dirt and veg
dismo::nicheOverlap(DirtModSSS, VegModSSS, stat='I', mask=T, checkNegatives = T)
# 0.2681263
dismo::nicheOverlap(DirtModSSS, VegModSSS, stat='D', mask=T, checkNegatives = T)
# 0.1807469
dismo::nicheOverlap(DirtModS, VegModS, stat='I', mask=T, checkNegatives = T)
# 0.7227302
dismo::nicheOverlap(DirtModS, VegModS, stat='D', mask=T, checkNegatives = T)
# 0.4342465

# veg and water
dismo::nicheOverlap(VegModSSS, WaterModSSS, stat='I', mask=T, checkNegatives = T)
# 0.07066181
dismo::nicheOverlap(VegModSSS, WaterModSSS, stat='D', mask=T, checkNegatives = T)
# 0.06013196
dismo::nicheOverlap(VegModS, WaterModS, stat='I', mask=T, checkNegatives = T)
# 0.3293571
dismo::nicheOverlap(VegModS, WaterModS, stat='D', mask=T, checkNegatives = T)
# 0.1302702

# veg and rock
dismo::nicheOverlap(VegModSSS, RockModSSS, stat='I', mask=T, checkNegatives = T)
#  0.1315312
dismo::nicheOverlap(VegModSSS, RockModSSS, stat='D', mask=T, checkNegatives = T)
#  0.0965914
dismo::nicheOverlap(VegModS, RockModS, stat='I', mask=T, checkNegatives = T)
# 0.5342483
dismo::nicheOverlap(VegModS, RockModS, stat='D', mask=T, checkNegatives = T)
# 0.2879063

# dirt and water
dismo::nicheOverlap(DirtModSSS, WaterModSSS, stat='I', mask=T, checkNegatives = T)
# 0.6279398
dismo::nicheOverlap(DirtModSSS, WaterModSSS, stat='D', mask=T, checkNegatives = T)
# 0.4974263
dismo::nicheOverlap(DirtModS, WaterModS, stat='I', mask=T, checkNegatives = T)
# 0.7539265
dismo::nicheOverlap(DirtModS, WaterModS, stat='D', mask=T, checkNegatives = T)
# 0.4935264

# dirt and rock
dismo::nicheOverlap(DirtModSSS, RockModSSS, stat='I', mask=T, checkNegatives = T)
# 0.4840399
dismo::nicheOverlap(DirtModSSS, RockModSSS, stat='D', mask=T, checkNegatives = T)
# 0.2396195
dismo::nicheOverlap(DirtModS, RockModS, stat='I', mask=T, checkNegatives = T)
# 0.7847506
dismo::nicheOverlap(DirtModS, RockModS, stat='D', mask=T, checkNegatives = T)
# 0.5041729

# water and rock
dismo::nicheOverlap(WaterModSSS, RockModSSS, stat='I', mask=T, checkNegatives = T)
# 0.4060255
dismo::nicheOverlap(WaterModSSS, RockModSSS, stat='D', mask=T, checkNegatives = T)
# 0.2537369
dismo::nicheOverlap(WaterModS, RockModS, stat='I', mask=T, checkNegatives = T)
# 0.6051642
dismo::nicheOverlap(WaterModS, RockModS, stat='D', mask=T, checkNegatives = T)
# 0.4081347


######################################################################################

# dirt and veg
dismo::nicheOverlap(DirtModLSS, VegModLSS, stat='I', mask=T, checkNegatives = T)
#  0.2954906
dismo::nicheOverlap(DirtModLSS, VegModLSS, stat='D', mask=T, checkNegatives = T)
#  0.189663
dismo::nicheOverlap(DirtModL, VegModL, stat='I', mask=T, checkNegatives = T)
# 0.7262059
dismo::nicheOverlap(DirtModL, VegModL, stat='D', mask=T, checkNegatives = T)
# 0.4451553

# veg and water
dismo::nicheOverlap(VegModLSS, WaterModLSS, stat='I', mask=T, checkNegatives = T)
# 0.05567155
dismo::nicheOverlap(VegModLSS, WaterModLSS, stat='D', mask=T, checkNegatives = T)
# 0.04572444
dismo::nicheOverlap(VegModL, WaterModL, stat='I', mask=T, checkNegatives = T)
# 0.3430431
dismo::nicheOverlap(VegModL, WaterModL, stat='D', mask=T, checkNegatives = T)
# 0.135447

# veg and rock
dismo::nicheOverlap(VegModLSS, RockModLSS, stat='I', mask=T, checkNegatives = T)
#  0.1536091
dismo::nicheOverlap(VegModLSS, RockModLSS, stat='D', mask=T, checkNegatives = T)
# 0.1478229
dismo::nicheOverlap(VegModL, RockModL, stat='I', mask=T, checkNegatives = T)
# 0.5272911
dismo::nicheOverlap(VegModL, RockModL, stat='D', mask=T, checkNegatives = T)
# 0.2857815

# dirt and water
dismo::nicheOverlap(DirtModLSS, WaterModLSS, stat='I', mask=T, checkNegatives = T)
# 0.6650475
dismo::nicheOverlap(DirtModLSS, WaterModLSS, stat='D', mask=T, checkNegatives = T)
# 0.5197283
dismo::nicheOverlap(DirtModL, WaterModL, stat='I', mask=T, checkNegatives = T)
# 0.7842509
dismo::nicheOverlap(DirtModL, WaterModL, stat='D', mask=T, checkNegatives = T)
# 0.5173696

# dirt and rock
dismo::nicheOverlap(DirtModLSS, RockModLSS, stat='I', mask=T, checkNegatives = T)
#  0.5857643
dismo::nicheOverlap(DirtModLSS, RockModLSS, stat='D', mask=T, checkNegatives = T)
#  0.7936531
dismo::nicheOverlap(DirtModL, RockModL, stat='I', mask=T, checkNegatives = T)
# 0.7936531
dismo::nicheOverlap(DirtModL, RockModL, stat='D', mask=T, checkNegatives = T)
# 0.5086255

# water and rock
dismo::nicheOverlap(WaterModLSS, RockModLSS, stat='I', mask=T, checkNegatives = T)
# 0.626755
dismo::nicheOverlap(WaterModLSS, RockModLSS, stat='D', mask=T, checkNegatives = T)
#  0.4953791
dismo::nicheOverlap(WaterModL, RockModL, stat='I', mask=T, checkNegatives = T)
# 0.7158928
dismo::nicheOverlap(WaterModL, RockModL, stat='D', mask=T, checkNegatives = T)
# 0.5431892


###
# dirt poly intersect with dirt niche
# DirtPolyS area
areaPolygon(DirtPolyS) / 1e6 # 4543085
# DirtModpolS area
areaPolygon(DirtModpolS) / 1e6 # 5987066
# intersection between them
DDS <- raster::intersect(DirtPolyS,DirtModpolS)
areaPolygon(DDS) / 1e6 # 3808467
# percent of the polygon explained by the niche
(3808467/4543085)*100 # 83.82997

###
# Veg poly intersect with Veg niche
# VegPolyS area
areaPolygon(VegPolyS) / 1e6 # 671261.5
# VegModpolS area
areaPolygon(VegModpolS) / 1e6 # 3141018
# intersection between them
VVS <- raster::intersect(VegPolyS,VegModpolS)
areaPolygon(VVS)/ 1e6 # 473775
# percent of the polygon explained by the niche
(473775/671261.5)*100 # 70.5798

###
# rock poly intersect with rock niche
# RockPolyS area
areaPolygon(RockPolyS) / 1e6 # 592799
# RockModpolS area
areaPolygon(RockModpolS) / 1e6 # 1517493
# intersection between them
RRS <- raster::intersect(RockPolyS,RockModpolS)
areaPolygon(RRS) / 1e6 # 536032.3
# percent of the polygon explained by the niche
(536032.3/592799)*100 # 90.42395

###
# water poly intersect with water niche
# WaterPolyS area
areaPolygon(WaterPolyS) / 1e6 # 2829823
# WaterModpolS area
areaPolygon(WaterModpolS) / 1e6 # 3602891
# intersection between them
WWS <- raster::intersect(WaterPolyS,WaterModpolS)
areaPolygon(WWS) / 1e6 # 2523656
# percent of the polygon explained by the niche
(2523656/2829823)*100 # 89.1807


###
# intersection area Dirt-Veg niches
# Area for the niches
areaPolygon(DirtModpolS) / 1e6 # 5987066
areaPolygon(VegModpolS) / 1e6 # 3141018
#inter bet dirt and veg at 0.5 cutoff
interDVSN <- raster::intersect(DirtModpolS, VegModpolS)
areaPolygon(interDVSN) / 1e6 # 1166246
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(1166246/5987066)*100 # 19.47942
# inter/veg niche
(1166246/3141018)*100 #  37.12955

###
# intersection area Dirt-Rock niches
# Area for the niches
areaPolygon(DirtModpolS) / 1e6 # 5987066
areaPolygon(RockModpolS) / 1e6 # 1517493
#inter bet dirt and rock at 0.5 cutoff
interDRSN <- raster::intersect(DirtModpolS, RockModpolS)
areaPolygon(interDRSN) / 1e6 # 1477906
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(1477906/5987066)*100 # 24.68498
# inter/rock niche
(1477906/1517493)*100 #  97.39129

###
# intersection area Dirt-Water niches
# Area for the niches
areaPolygon(DirtModpolS) / 1e6 # 5987066
areaPolygon(WaterModpolS) / 1e6 # 3602891
#inter bet dirt and water at 0.5 cutoff
interDWSN <- raster::intersect(DirtModpolS, WaterModpolS)
areaPolygon(interDWSN) / 1e6 # 2955953
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(2955953/5987066)*100 # 49.37231
# inter/water niche
(2955953/3602891)*100 # 82.04392

###
# intersection area Veg-Rock niches
# Area for the niches
areaPolygon(VegModpolS) / 1e6 # 3141018
areaPolygon(RockModpolS) / 1e6 # 1517493
#inter bet veg and rock at 0.5 cutoff
interVRSN <- raster::intersect(VegModpolS, RockModpolS)
areaPolygon(interVRSN) / 1e6 # 279609.3
# percent of each niche that overlaps with the other by area
# inter/veg niche
(279609.3/3141018)*100 # 8.901869
# inter/rock niche
(279609.3/1517493)*100 # 18.42574

###
# intersection area Veg-Water niches
# Area for the niches
areaPolygon(VegModpolS) / 1e6 # 3141018
areaPolygon(WaterModpolS) / 1e6 # 3602891
#inter bet Veg and Water at 0.5 cutoff
interVWSN <- raster::intersect(VegModpolS, WaterModpolS)
areaPolygon(interVWSN) / 1e6 # 244454.7
# percent of each niche that overlaps with the other by area
# inter/veg niche
(244454.7/3141018)*100 # 7.782658
# inter/water niche
(244454.7/3602891)*100 # 6.78496

###
# intersection area Rock-Water niches
# Area for the niches
areaPolygon(RockModpolS) / 1e6 # 1517493
areaPolygon(WaterModpolS) / 1e6 # 3602891
#inter bet rock and Water at 0.5 cutoff
interRWSN <- raster::intersect(RockModpolS, WaterModpolS)
areaPolygon(interRWSN) / 1e6 # 975516.2
# percent of each niche that overlaps with the other by area
# inter/rock niche
(975516.2/1517493)*100 # 64.28472
# inter/water niche
(975516.2/3602891)*100 # 27.07593


######################################################################################

# dirt poly intersect with dirt niche
# DirtPolyL area
areaPolygon(DirtPolyL) / 1e6 # 3920842
# DirtModpolL area
areaPolygon(DirtModpolL) / 1e6 # 6250365
# intersection between them
DDL <- raster::intersect(DirtPolyL,DirtModpolL)
areaPolygon(DDL) / 1e6 # 3405375
# percent of the polygon explained by the niche
(3405375/3920842)*100 # 86.85316

###
# Veg poly intersect with Veg niche
# VegPolyL area
areaPolygon(VegPolyL) / 1e6 # 971408.6
# VegModpolL area
areaPolygon(VegModpolL) / 1e6 # 3013852
# intersection between them
VVL <- raster::intersect(VegPolyL,VegModpolL)
areaPolygon(VVL) / 1e6 # 525718.9
# percent of the polygon explained by the niche
(525718.9/971408.6)*100 # 54.11923

###
# rock poly intersect with rock niche
# RockPolyL area
areaPolygon(RockPolyL) / 1e6 # 1976929
# RockModpolL area
areaPolygon(RockModpolL) / 1e6 # 2510470
# intersection between them
RRL <- raster::intersect(RockPolyL,RockModpolL)
areaPolygon(RRL) / 1e6 # 1617842
# percent of the polygon explained by the niche
(1617842/1976929)*100 # 81.83612

###
# water poly intersect with water niche
# WaterPolyL area
areaPolygon(WaterPolyL) / 1e6 # 2829823
# WaterModpolL area
areaPolygon(WaterModpolL) / 1e6 # 3717400
# intersection between them
WWL <- raster::intersect(WaterPolyL,WaterModpolL)
areaPolygon(WWL) / 1e6 # 2582439
# percent of the polygon explained by the niche
(2582439/2829823)*100 # 91.25797


###
# intersection area Dirt-Veg niches
# Area for the niches
areaPolygon(DirtModpolL) / 1e6 # 6250365
areaPolygon(VegModpolL) / 1e6 # 3013852
#inter bet dirt and veg at 0.5 cutoff
interDVLN <- raster::intersect(DirtModpolL, VegModpolL)
areaPolygon(interDVLN) / 1e6 # 1252341
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(1252341/6250365)*100 # 20.03629
# inter/veg niche
(1252341/3013852)*100 # 41.55284

###
# intersection area Dirt-Rock niches
# Area for the niches
areaPolygon(DirtModpolL) / 1e6 # 6250365
areaPolygon(RockModpolL) / 1e6 # 2510470
#inter bet dirt and rock at 0.5 cutoff
interDRLN <- raster::intersect(DirtModpolL, RockModpolL)
areaPolygon(interDRLN) / 1e6 # 2375088
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(2375088/6250365)*100 # 37.99919
# inter/rock niche
(2375088/2510470)*100 #  94.6073

###
# intersection area Dirt-Water niches
# Area for the niches
areaPolygon(DirtModpolL) / 1e6 # 6250365
areaPolygon(WaterModpolL) / 1e6 # 3717400
#inter bet dirt and water at 0.5 cutoff
interDWLN <- raster::intersect(DirtModpolL, WaterModpolL)
areaPolygon(interDWLN) / 1e6 # 3225941
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(3225941/6250365)*100 # 51.61204
# inter/water niche
(3225941/3717400)*100 # 86.7795

###
# intersection area Veg-Rock niches
# Area for the niches
areaPolygon(VegModpolL) / 1e6 # 3013852
areaPolygon(RockModpolL) / 1e6 # 2510470
#inter bet veg and rock at 0.5 cutoff
interVRLN <- raster::intersect(VegModpolL, RockModpolL)
areaPolygon(interVRLN) / 1e6 # 408441
# percent of each niche that overlaps with the other by area
# inter/veg niche
(408441/3013852)*100 # 13.55213
# inter/rock niche
(408441/2510470)*100 # 16.2695

###
# intersection area Veg-Water niches
# Area for the niches
areaPolygon(VegModpolL) / 1e6 # 3013852
areaPolygon(WaterModpolL) / 1e6 # 3717400
#inter bet Veg and Water at 0.5 cutoff
interVWLN <- raster::intersect(VegModpolL, WaterModpolL)
areaPolygon(interVWLN) / 1e6 # 191767.1
# percent of each niche that overlaps with the other by area
# inter/veg niche
(191767.1/3013852)*100 # 6.362857
# inter/water niche
(191767.1/3717400)*100 # 5.158635

###
# intersection area Rock-Water niches
# Area for the niches
areaPolygon(RockModpolL) / 1e6 # 2510470
areaPolygon(WaterModpolL) / 1e6 # 3717400
#inter bet rock and Water at 0.5 cutoff
interRWLN <- raster::intersect(RockModpolL, WaterModpolL)
areaPolygon(interRWLN) / 1e6 # 2002517
# percent of each niche that overlaps with the other by area
# inter/rock niche
(2002517/2510470)*100 # 79.76662
# inter/water niche
(2002517/3717400)*100 # 53.86875


#######################################
#### distribution inter with niche ###
#######################################

# intersect dirt polygon with veg niche model
areaPolygon(VegModpolS) / 1e6 # 3141018
interDSVN <- raster::intersect(DirtPolyS, VegModpolS)
areaPolygon(interDSVN) / 1e6 # 915277
(915277/3141018)*100 # 29.1395
# this is dirt spp present where veg can live

# intersect dirt polygon with rock niche model
areaPolygon(RockModpolS) / 1e6 # 1517493
interDSRN <- raster::intersect(DirtPolyS, RockModpolS)
areaPolygon(interDSRN) / 1e6 # 1197684
(1197684/1517493)*100 # 78.92517
# this is dirt spp present where rock can live

# intersect dirt polygon with water niche model
areaPolygon(WaterModpolS) / 1e6 # 3602891
interDSWN <- raster::intersect(DirtPolyS, WaterModpolS)
areaPolygon(interDSWN) / 1e6 # 2670004
(2670004/3602891)*100 # 74.10727
# this is dirt spp present where water can live

# intersect veg polygon with dirt niche model
areaPolygon(DirtModpolS) / 1e6 # 5987066
interVSDN <- raster::intersect(VegPolyS, DirtModpolS)
areaPolygon(interVSDN) / 1e6 # 284077.2
(284077.2/5987066)*100 #  4.744848
# this is veg spp present where dirt can live

# intersect veg polygon with rock niche model
areaPolygon(RockModpolS) / 1e6 # 1517493
interVSRN <- raster::intersect(VegPolyS, RockModpolS)
areaPolygon(interVSRN) / 1e6 # 78549.13
(78549.13/1517493)*100 # 5.176243
# this is veg spp present where rock can live

# intersect veg polygon with water niche model
areaPolygon(WaterModpolS) / 1e6 # 3602891
interVSWN <- raster::intersect(VegPolyS, WaterModpolS)
# this is zero, no intersection
# this is terr spp present where arb can live

# intersect rock polygon with dirt niche model
areaPolygon(DirtModpolS) / 1e6 # 5987066
interRSDN <- raster::intersect(RockPolyS, DirtModpolS)
areaPolygon(interRSDN) / 1e6 # 558187.9
(558187.9/5987066)*100 #  9.323229
# this is rock spp present where dirt can live

# intersect rock polygon with veg niche model
areaPolygon(VegModpolS) / 1e6 # 3141018
interRSVN <- raster::intersect(RockPolyS, VegModpolS)
areaPolygon(interRSVN) / 1e6 # 60173.52
(60173.52/3141018)*100 # 1.915733
# this is rock spp present where veg can live

# intersect rock polygon with Water niche model
areaPolygon(WaterModpolS) / 1e6 # 3602891
interRSWN <- raster::intersect(RockPolyS, WaterModpolS)
areaPolygon(interRSWN) / 1e6 # 468393.9
(468393.9/3602891)*100 #  13.0005
# this is rock spp present where water can live

# intersect water polygon with dirt niche model
areaPolygon(DirtModpolS) / 1e6 # 5987066
interWSDN <- raster::intersect(WaterPolyS, DirtModpolS)
areaPolygon(interWSDN) / 1e6 # 2555447
(2555447/5987066)*100 # 42.68279
# this is water spp present where dirt can live

# intersect Water polygon with veg niche model
areaPolygon(VegModpolS) / 1e6 # 3141018
interWSVN <- raster::intersect(WaterPolyS, VegModpolS)
areaPolygon(interWSVN) / 1e6 # 256274.9
(256274.9/3141018)*100 # 8.158976
# this is water spp present where veg can live

# intersect Water polygon with rock niche model
areaPolygon(RockModpolS) / 1e6 # 1517493
interWSRN <- raster::intersect(WaterPolyS, RockModpolS)
areaPolygon(interWSRN) / 1e6 # 927961.8
(927961.8/1517493)*100 # 61.15098
# this is water spp present where rock can live


##############################################################################################

# intersect dirt polygon with veg niche model
areaPolygon(VegModpolL) / 1e6 # 3013852
interDLVN <- raster::intersect(DirtPolyL, VegModpolL)
areaPolygon(interDLVN) / 1e6 # 765985.7
(765985.7/3013852)*100 # 25.4155
# this is dirt spp present where veg can live

# intersect dirt polygon with rock niche model
areaPolygon(RockModpolL) / 1e6 # 2510470
interDLRN <- raster::intersect(DirtPolyL, RockModpolL)
areaPolygon(interDLRN) / 1e6 # 1505893
(1505893/2510470)*100 # 59.9845
# this is dirt spp present where rock can live

# intersect dirt polygon with water niche model
areaPolygon(WaterModpolL) / 1e6 # 3717400
interDLWN <- raster::intersect(DirtPolyL, WaterModpolL)
areaPolygon(interDLWN) / 1e6 # 2374315
(2374315/3717400)*100 # 63.87031
# this is dirt spp present where water can live

# intersect veg polygon with dirt niche model
areaPolygon(DirtModpolL) / 1e6 # 6250365
interVLDN <- raster::intersect(VegPolyL, DirtModpolL)
areaPolygon(interVLDN) / 1e6 # 529981.3
(529981.3/6250365)*100 # 8.479206
# this is veg spp present where dirt can live

# intersect veg polygon with rock niche model
areaPolygon(RockModpolL) / 1e6 # 2510470
interVSRN <- raster::intersect(VegPolyL, RockModpolL)
areaPolygon(interVSRN) / 1e6 # 287785.9
(287785.9/2510470)*100 #  11.46343
# this is veg spp present where rock can live

# intersect veg polygon with water niche model
areaPolygon(WaterModpolL) / 1e6 # 3717400
interVLWN <- raster::intersect(VegPolyL, WaterModpolL)
areaPolygon(interVLWN) / 1e6 # 172945.9
(172945.9/3717400)*100 # 4.652335
# this is veg spp present where water can live

# intersect rock polygon with dirt niche model
areaPolygon(DirtModpolL) / 1e6 # 6250365
interRLDN <- raster::intersect(RockPolyL, DirtModpolL)
areaPolygon(interRLDN) / 1e6 # 1817674
(1817674/6250365)*100 #  29.08109
# this is rock spp present where dirt can live

# intersect rock polygon with veg niche model
areaPolygon(VegModpolL) / 1e6 # 3013852
interRLVN <- raster::intersect(RockPolyL, VegModpolL)
areaPolygon(interRLVN) / 1e6 # 186352.9
(186352.9/3013852)*100 #  6.183213
# this is rock spp present where veg can live

# intersect rock polygon with Water niche model
areaPolygon(WaterModpolL) / 1e6 # 3717400
interRLWN <- raster::intersect(RockPolyL, WaterModpolL)
areaPolygon(interRLWN) / 1e6 # 1724387
(1724387/3717400)*100 # 46.38691
# this is rock spp present where water can live

# intersect water polygon with dirt niche model
areaPolygon(DirtModpolL) / 1e6 # 6250365
interWLDN <- raster::intersect(WaterPolyL, DirtModpolL)
areaPolygon(interWLDN) / 1e6 # 2601382
(2601382/6250365)*100 # 41.61968
# this is water spp present where dirt can live

# intersect Water polygon with veg niche model
areaPolygon(VegModpolL) / 1e6 # 3013852
interWLVN <- raster::intersect(WaterPolyL, VegModpolL)
areaPolygon(interWLVN) / 1e6 # 189114.6
(189114.6/3013852)*100 # 6.274847
# this is water spp present where veg can live

# intersect Water polygon with rock niche model
areaPolygon(RockModpolL) / 1e6 # 2510470
interWLRN <- raster::intersect(WaterPolyL, RockModpolL)
areaPolygon(interWLRN) / 1e6 # 1661754
(1661754/2510470)*100 # 66.19294
# this is water spp present where rock can live

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
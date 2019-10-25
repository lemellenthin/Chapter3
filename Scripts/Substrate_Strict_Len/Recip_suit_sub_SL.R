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
library(rgdal); library(rgeos); library(maxent); library(dismo)

# load the maxent predictions
DirtModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/DirtMod_prediction_strict.grd")
VegModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/VegMod_prediction_strict.grd")
RockModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/RockMod_prediction_strict.grd")
WaterModS <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/WaterMod_prediction_strict.grd")
DirtModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/DirtMod_prediction_lenient.grd")
VegModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/VegMod_prediction_lenient.grd")
RockModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/RockMod_prediction_lenient.grd")
WaterModL <- raster("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Predictions/WaterMod_prediction_lenient.grd")

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

# get the area of the niche polygon
areaPolygon(DirtModpolS) / 1e6
# 5152820
areaPolygon(VegModpolS) / 1e6
# 1308248
areaPolygon(RockModpolS) / 1e6
# 686940.1
areaPolygon(WaterModpolS) / 1e6
# 3225799

areaPolygon(DirtModpolL) / 1e6
# 4847568
areaPolygon(VegModpolL) / 1e6
# 2401722
areaPolygon(RockModpolL) / 1e6
# 2313223
areaPolygon(WaterModpolL) / 1e6
# 3251389

######################################################################################################
# strict reciprocal suitability 10/20/19

# How much habitat that dirt species live in is suitable for veg life 0.5 cut off veg niche
areaPolygon(VegModpolS) / 1e6 #  1308248
areaPolygon(DirtPolyS) / 1e6 # 4543085
intersectionTA <- raster::intersect(VegModpolS, DirtPolyS)
areaPolygon(intersectionTA) / 1e6 # 593912.5
# divide the intersection by the dirt polygon area
(593912.5/4543085)*100 # 13.07289

# How much habitat that veg species live in is suitable for dirt life 0.5 cut off dirt niche
areaPolygon(DirtModpolS) / 1e6 #  5152820
areaPolygon(VegPolyS) / 1e6 # 671261.5
intersectionAT <- raster::intersect(DirtModpolS, VegPolyS)
areaPolygon(intersectionAT) / 1e6 # 236260.3
# divide the intersection by the veg polygon area
(236260.3/671261.5)*100 # 35.19646

# veg polygon and veg niche 0.5 cut off veg niche
areaPolygon(VegModpolS) / 1e6 #  1308248
areaPolygon(VegPolyS) / 1e6 # 671261.5
intersectionAA <- raster::intersect(VegModpolS, VegPolyS)
areaPolygon(intersectionAA) / 1e6 # 493240.1
# divide the intersection by the veg polygon area
(493240.1/671261.5)*100 # 73.47958

# dirt polygon and dirt niche 0.5 cut off dirt niche
areaPolygon(DirtModpolS) / 1e6 #  5152820
areaPolygon(DirtPolyS) / 1e6 # 4543085
intersectionTT <- raster::intersect(DirtModpolS, DirtPolyS)
areaPolygon(intersectionTT) / 1e6 # 3771700
# divide the intersection by the dirt polygon area
(3771700/4543085)*100 # 83.02068

# lenient reciprocal suitability 10/20/19

# How much habitat that dirt species live in is suitable for veg life 0.5 cut off veg niche
areaPolygon(VegModpolL) / 1e6 #  2401722
areaPolygon(DirtPolyL) / 1e6 # 3920842
intersectionTA <- raster::intersect(VegModpolL, DirtPolyL)
areaPolygon(intersectionTA) / 1e6 # 1192061
# divide the intersection by the dirt polygon area
(1192061/3920842)*100 # 30.40319

# How much habitat that veg species live in is suitable for dirt life 0.5 cut off dirt niche
areaPolygon(DirtModpolL) / 1e6 # 4847568
areaPolygon(VegPolyL) / 1e6 # 971408.6
intersectionAT <- raster::intersect(DirtModpolL, VegPolyL)
areaPolygon(intersectionAT) / 1e6 # 478055.7
# divide the intersection by the veg polygon area
(478055.7/971408.6)*100 # 49.21263

# veg polygon and veg niche 0.5 cut off veg niche
areaPolygon(VegModpolL) / 1e6 #  2401722
areaPolygon(VegPolyL) / 1e6 #  971408.6
intersectionAA <- raster::intersect(VegModpolL, VegPolyL)
areaPolygon(intersectionAA) / 1e6 # 721217.6
# divide the intersection by the veg polygon area
(721217.6/971408.6)*100 # 74.24451

# dirt polygon and dirt niche 0.5 cut off dirt niche
areaPolygon(DirtModpolL) / 1e6 #  4847568
areaPolygon(DirtPolyL) / 1e6 # 3920842
intersectionTT <- raster::intersect(DirtModpolL, DirtPolyL)
areaPolygon(intersectionTT) / 1e6 # 3310788
# divide the intersection by the dirt polygon area
(3310788/3920842)*100 # 84.44074

######################################################################################################

# niche overlap 
# dirt and veg
dismo::nicheOverlap(DirtModSSS, VegModSSS, stat='I', mask=T, checkNegatives = T)
# 0.1988947
dismo::nicheOverlap(DirtModSSS, VegModSSS, stat='D', mask=T, checkNegatives = T)
# 0.09399391
dismo::nicheOverlap(DirtModS, VegModS, stat='I', mask=T, checkNegatives = T)
# 0.5828209
dismo::nicheOverlap(DirtModS, VegModS, stat='D', mask=T, checkNegatives = T)
# 0.319821

# veg and water
dismo::nicheOverlap(VegModSSS, WaterModSSS, stat='I', mask=T, checkNegatives = T)
# 0.01068365
dismo::nicheOverlap(VegModSSS, WaterModSSS, stat='D', mask=T, checkNegatives = T)
# 0.006396512
dismo::nicheOverlap(VegModS, WaterModS, stat='I', mask=T, checkNegatives = T)
# 0.2463302
dismo::nicheOverlap(VegModS, WaterModS, stat='D', mask=T, checkNegatives = T)
# 0.09719971

# veg and rock
dismo::nicheOverlap(VegModSSS, RockModSSS, stat='I', mask=T, checkNegatives = T)
#  -8.881784e-16 - ie 0
dismo::nicheOverlap(VegModSSS, RockModSSS, stat='D', mask=T, checkNegatives = T)
# 1.554312e-15 - ie 0
dismo::nicheOverlap(VegModS, RockModS, stat='I', mask=T, checkNegatives = T)
#0.2002149
dismo::nicheOverlap(VegModS, RockModS, stat='D', mask=T, checkNegatives = T)
# 0.06324667

# dirt and water
dismo::nicheOverlap(DirtModSSS, WaterModSSS, stat='I', mask=T, checkNegatives = T)
# 0.724615
dismo::nicheOverlap(DirtModSSS, WaterModSSS, stat='D', mask=T, checkNegatives = T)
# 0.5719529
dismo::nicheOverlap(DirtModS, WaterModS, stat='I', mask=T, checkNegatives = T)
# 0.8046466
dismo::nicheOverlap(DirtModS, WaterModS, stat='D', mask=T, checkNegatives = T)
# 0.5569002

# dirt and rock
dismo::nicheOverlap(DirtModSSS, RockModSSS, stat='I', mask=T, checkNegatives = T)
# 0.3569788
dismo::nicheOverlap(DirtModSSS, RockModSSS, stat='D', mask=T, checkNegatives = T)
# 0.1274339
dismo::nicheOverlap(DirtModS, RockModS, stat='I', mask=T, checkNegatives = T)
# 0.5607037
dismo::nicheOverlap(DirtModS, RockModS, stat='D', mask=T, checkNegatives = T)
# 0.2580245

# water and rock
dismo::nicheOverlap(WaterModSSS, RockModSSS, stat='I', mask=T, checkNegatives = T)
# 0.4384002
dismo::nicheOverlap(WaterModSSS, RockModSSS, stat='D', mask=T, checkNegatives = T)
# 0.1982715
dismo::nicheOverlap(WaterModS, RockModS, stat='I', mask=T, checkNegatives = T)
#0.6721021
dismo::nicheOverlap(WaterModS, RockModS, stat='D', mask=T, checkNegatives = T)
# 0.375371


######################################################################################

# dirt and veg
dismo::nicheOverlap(DirtModLSS, VegModLSS, stat='I', mask=T, checkNegatives = T)
#  0.4057241
dismo::nicheOverlap(DirtModLSS, VegModLSS, stat='D', mask=T, checkNegatives = T)
#  0.2709151
dismo::nicheOverlap(DirtModL, VegModL, stat='I', mask=T, checkNegatives = T)
# 0.734848
dismo::nicheOverlap(DirtModL, VegModL, stat='D', mask=T, checkNegatives = T)
#  0.4862683

# veg and water
dismo::nicheOverlap(VegModLSS, WaterModLSS, stat='I', mask=T, checkNegatives = T)
#  0.2945311
dismo::nicheOverlap(VegModLSS, WaterModLSS, stat='D', mask=T, checkNegatives = T)
# 0.241477
dismo::nicheOverlap(VegModL, WaterModL, stat='I', mask=T, checkNegatives = T)
# 0.5065009
dismo::nicheOverlap(VegModL, WaterModL, stat='D', mask=T, checkNegatives = T)
# 0.3007666

# veg and rock
dismo::nicheOverlap(VegModLSS, RockModLSS, stat='I', mask=T, checkNegatives = T)
#  0.3599248
dismo::nicheOverlap(VegModLSS, RockModLSS, stat='D', mask=T, checkNegatives = T)
# 0.3588846
dismo::nicheOverlap(VegModL, RockModL, stat='I', mask=T, checkNegatives = T)
#0.6305382
dismo::nicheOverlap(VegModL, RockModL, stat='D', mask=T, checkNegatives = T)
#  0.3523457

# dirt and water
dismo::nicheOverlap(DirtModLSS, WaterModLSS, stat='I', mask=T, checkNegatives = T)
# 0.7343884
dismo::nicheOverlap(DirtModLSS, WaterModLSS, stat='D', mask=T, checkNegatives = T)
# 0.5981135
dismo::nicheOverlap(DirtModL, WaterModL, stat='I', mask=T, checkNegatives = T)
# 0.8037039
dismo::nicheOverlap(DirtModL, WaterModL, stat='D', mask=T, checkNegatives = T)
# 0.5622735

# dirt and rock
dismo::nicheOverlap(DirtModLSS, RockModLSS, stat='I', mask=T, checkNegatives = T)
#  0.6347148
dismo::nicheOverlap(DirtModLSS, RockModLSS, stat='D', mask=T, checkNegatives = T)
#  0.425048
dismo::nicheOverlap(DirtModL, RockModL, stat='I', mask=T, checkNegatives = T)
# 0.7661032
dismo::nicheOverlap(DirtModL, RockModL, stat='D', mask=T, checkNegatives = T)
# 0.4584403

# water and rock
dismo::nicheOverlap(WaterModLSS, RockModLSS, stat='I', mask=T, checkNegatives = T)
# 0.765948
dismo::nicheOverlap(WaterModLSS, RockModLSS, stat='D', mask=T, checkNegatives = T)
# 0.6297974
dismo::nicheOverlap(WaterModL, RockModL, stat='I', mask=T, checkNegatives = T)
# 0.8175637
dismo::nicheOverlap(WaterModL, RockModL, stat='D', mask=T, checkNegatives = T)
# 0.6245364


###
# dirt poly intersect with dirt niche
# DirtPolyS area
areaPolygon(DirtPolyS) / 1e6 # 4543085
# DirtModpolS area
areaPolygon(DirtModpolS) / 1e6 # 5152820
# intersection between them
DDS <- raster::intersect(DirtPolyS,DirtModpolS)
areaPolygon(DDS) / 1e6 # 3771700
# percent of the polygon explained by the niche
(3771700/4543085)*100 # 83.02068

###
# Veg poly intersect with Veg niche
# VegPolyS area
areaPolygon(VegPolyS) / 1e6 # 671261.5
# VegModpolS area
areaPolygon(VegModpolS) / 1e6 # 1308248
# intersection between them
VVS <- raster::intersect(VegPolyS,VegModpolS)
areaPolygon(VVS)/ 1e6 # 493240.1
# percent of the polygon explained by the niche
(493240.1/671261.5)*100 # 73.47958

###
# rock poly intersect with rock niche
# RockPolyS area
areaPolygon(RockPolyS) / 1e6 # 592799
# RockModpolS area
areaPolygon(RockModpolS) / 1e6 # 686940.1
# intersection between them
RRS <- raster::intersect(RockPolyS,RockModpolS)
areaPolygon(RRS) / 1e6 # 489027.2
# percent of the polygon explained by the niche
(489027.2/592799)*100 # 82.49461

###
# water poly intersect with water niche
# WaterPolyS area
areaPolygon(WaterPolyS) / 1e6 # 2829823
# WaterModpolS area
areaPolygon(WaterModpolS) / 1e6 # 3225799
# intersection between them
WWS <- raster::intersect(WaterPolyS,WaterModpolS)
areaPolygon(WWS) / 1e6 # 2647729
# percent of the polygon explained by the niche
(2647729/2829823)*100 # 93.56518


###
# intersection area Dirt-Veg niches
# Area for the niches
areaPolygon(DirtModpolS) / 1e6 # 5152820
areaPolygon(VegModpolS) / 1e6 # 1308248
#inter bet dirt and veg at 0.5 cutoff
interDVSN <- raster::intersect(DirtModpolS, VegModpolS)
areaPolygon(interDVSN) / 1e6 # 484382.7
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(484382.7/5152820)*100 # 9.400342
# inter/veg niche
(484382.7/1308248)*100 # 37.0253

###
# intersection area Dirt-Rock niches
# Area for the niches
areaPolygon(DirtModpolS) / 1e6 # 5152820
areaPolygon(RockModpolS) / 1e6 # 686940.1
#inter bet dirt and rock at 0.5 cutoff
interDRSN <- raster::intersect(DirtModpolS, RockModpolS)
areaPolygon(interDRSN) / 1e6 # 686940.1
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(686940.1/5152820)*100 # 24.68498
# inter/rock niche
(686940.1/686940.1)*100 # 100

###
# intersection area Dirt-Water niches
# Area for the niches
areaPolygon(DirtModpolS) / 1e6 # 5152820
areaPolygon(WaterModpolS) / 1e6 # 3225799
#inter bet dirt and water at 0.5 cutoff
interDWSN <- raster::intersect(DirtModpolS, WaterModpolS)
areaPolygon(interDWSN) / 1e6 # 2993052
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(2993052/5152820)*100 # 58.08571
# inter/water niche
(2993052/3225799)*100 # 92.78483

###
# intersection area Veg-Rock niches
# Area for the niches
areaPolygon(VegModpolS) / 1e6 # 1308248
areaPolygon(RockModpolS) / 1e6 # 686940.1
#inter bet veg and rock at 0.5 cutoff
interVRSN <- raster::intersect(VegModpolS, RockModpolS)
areaPolygon(interVRSN) / 1e6 # 0
# percent of each niche that overlaps with the other by area
# inter/veg niche
(0/1308248)*100 # 0
# inter/rock niche
(0/686940.1)*100 # 0

###
# intersection area Veg-Water niches
# Area for the niches
areaPolygon(VegModpolS) / 1e6 # 1308248
areaPolygon(WaterModpolS) / 1e6 # 3225799
#inter bet Veg and Water at 0.5 cutoff
interVWSN <- raster::intersect(VegModpolS, WaterModpolS)
areaPolygon(interVWSN) / 1e6 # 22895.57
# percent of each niche that overlaps with the other by area
# inter/veg niche
(22895.57/1308248)*100 # 1.750094
# inter/water niche
(22895.57/3225799)*100 # 0.7097643

###
# intersection area Rock-Water niches
# Area for the niches
areaPolygon(RockModpolS) / 1e6 # 686940.1
areaPolygon(WaterModpolS) / 1e6 # 3225799
#inter bet rock and Water at 0.5 cutoff
interRWSN <- raster::intersect(RockModpolS, WaterModpolS)
areaPolygon(interRWSN) / 1e6 # 666046
# percent of each niche that overlaps with the other by area
# inter/rock niche
(666046/686940.1)*100 # 96.95838
# inter/water niche
(666046/3225799)*100 # 20.64747


######################################################################################

# dirt poly intersect with dirt niche
# DirtPolyL area
areaPolygon(DirtPolyL) / 1e6 # 3920842
# DirtModpolL area
areaPolygon(DirtModpolL) / 1e6 # 4847568
# intersection between them
DDL <- raster::intersect(DirtPolyL,DirtModpolL)
areaPolygon(DDL) / 1e6 # 3310788
# percent of the polygon explained by the niche
(3310788/3920842)*100 # 84.44074

###
# Veg poly intersect with Veg niche
# VegPolyL area
areaPolygon(VegPolyL) / 1e6 # 971408.6
# VegModpolL area
areaPolygon(VegModpolL) / 1e6 # 2401722
# intersection between them
VVL <- raster::intersect(VegPolyL,VegModpolL)
areaPolygon(VVL) / 1e6 # 721217.6
# percent of the polygon explained by the niche
(721217.6/971408.6)*100 # 74.24451

###
# rock poly intersect with rock niche
# RockPolyL area
areaPolygon(RockPolyL) / 1e6 # 1976929
# RockModpolL area
areaPolygon(RockModpolL) / 1e6 # 2313223
# intersection between them
RRL <- raster::intersect(RockPolyL,RockModpolL)
areaPolygon(RRL) / 1e6 # 1755196
# percent of the polygon explained by the niche
(1755196/1976929)*100 # 88.78397

###
# water poly intersect with water niche
# WaterPolyL area
areaPolygon(WaterPolyL) / 1e6 # 2829823
# WaterModpolL area
areaPolygon(WaterModpolL) / 1e6 # 3251389
# intersection between them
WWL <- raster::intersect(WaterPolyL,WaterModpolL)
areaPolygon(WWL) / 1e6 # 2646686
# percent of the polygon explained by the niche
(2646686/2829823)*100 # 93.52832


###
# intersection area Dirt-Veg niches
# Area for the niches
areaPolygon(DirtModpolL) / 1e6 # 4847568
areaPolygon(VegModpolL) / 1e6 # 2401722
#inter bet dirt and veg at 0.5 cutoff
interDVLN <- raster::intersect(DirtModpolL, VegModpolL)
areaPolygon(interDVLN) / 1e6 # 1373597
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(1373597/4847568)*100 # 28.3358
# inter/veg niche
(1373597/2401722)*100 # 57.19217

###
# intersection area Dirt-Rock niches
# Area for the niches
areaPolygon(DirtModpolL) / 1e6 # 4847568
areaPolygon(RockModpolL) / 1e6 # 2313223
#inter bet dirt and rock at 0.5 cutoff
interDRLN <- raster::intersect(DirtModpolL, RockModpolL)
areaPolygon(interDRLN) / 1e6 # 2185973
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(2185973/4847568)*100 # 45.09422
# inter/rock niche
(2185973/2313223)*100 # 94.49902

###
# intersection area Dirt-Water niches
# Area for the niches
areaPolygon(DirtModpolL) / 1e6 # 4847568
areaPolygon(WaterModpolL) / 1e6 # 3251389
#inter bet dirt and water at 0.5 cutoff
interDWLN <- raster::intersect(DirtModpolL, WaterModpolL)
areaPolygon(interDWLN) / 1e6 # 2952630
# percent of each niche that overlaps with the other by area
# inter/dirt niche
(2952630/4847568)*100 # 60.90951
# inter/water niche
(2952630/3251389)*100 # 90.81134

###
# intersection area Veg-Rock niches
# Area for the niches
areaPolygon(VegModpolL) / 1e6 # 2401722
areaPolygon(RockModpolL) / 1e6 # 2313223
#inter bet veg and rock at 0.5 cutoff
interVRLN <- raster::intersect(VegModpolL, RockModpolL)
areaPolygon(interVRLN) / 1e6 # 833317.7
# percent of each niche that overlaps with the other by area
# inter/veg niche
(833317.7/2401722)*100 # 34.69668
# inter/rock niche
(833317.7/2313223)*100 # 36.0241

###
# intersection area Veg-Water niches
# Area for the niches
areaPolygon(VegModpolL) / 1e6 # 2401722
areaPolygon(WaterModpolL) / 1e6 # 3251389
#inter bet Veg and Water at 0.5 cutoff
interVWLN <- raster::intersect(VegModpolL, WaterModpolL)
areaPolygon(interVWLN) / 1e6 # 829057.8
# percent of each niche that overlaps with the other by area
# inter/veg niche
(829057.8/2401722)*100 # 34.51931
# inter/water niche
(829057.8/3251389)*100 # 25.49857

###
# intersection area Rock-Water niches
# Area for the niches
areaPolygon(RockModpolL) / 1e6 # 2313223
areaPolygon(WaterModpolL) / 1e6 # 3251389
#inter bet rock and Water at 0.5 cutoff
interRWLN <- raster::intersect(RockModpolL, WaterModpolL)
areaPolygon(interRWLN) / 1e6 # 2155252
# percent of each niche that overlaps with the other by area
# inter/rock niche
(2155252/2313223)*100 # 93.17096
# inter/water niche
(2155252/3251389)*100 # 66.28712


#######################################
#### distribution inter with niche ###
#######################################

# intersect dirt polygon with veg niche model
areaPolygon(VegModpolS) / 1e6 # 1308248
interDSVN <- raster::intersect(DirtPolyS, VegModpolS)
areaPolygon(interDSVN) / 1e6 # 593912.5
(593912.5/1308248)*100 # 45.39755
# this is dirt spp present where veg can live

# intersect dirt polygon with rock niche model
areaPolygon(RockModpolS) / 1e6 # 686940.1
interDSRN <- raster::intersect(DirtPolyS, RockModpolS)
areaPolygon(interDSRN) / 1e6 # 680632.2
(680632.2/686940.1)*100 # 99.08174
# this is dirt spp present where rock can live

# intersect dirt polygon with water niche model
areaPolygon(WaterModpolS) / 1e6 # 3225799
interDSWN <- raster::intersect(DirtPolyS, WaterModpolS)
areaPolygon(interDSWN) / 1e6 # 2788255
(2788255/3225799)*100 # 86.4361
# this is dirt spp present where water can live

# intersect veg polygon with dirt niche model
areaPolygon(DirtModpolS) / 1e6 # 5152820
interVSDN <- raster::intersect(VegPolyS, DirtModpolS)
areaPolygon(interVSDN) / 1e6 # 236260.3
(236260.3/5152820)*100 # 4.585068
# this is veg spp present where dirt can live

# intersect veg polygon with rock niche model
areaPolygon(RockModpolS) / 1e6 # 686940.1
interVSRN <- raster::intersect(VegPolyS, RockModpolS)
areaPolygon(interVSRN) / 1e6 # 0
(0/686940.1)*100 # 0
# this is veg spp present where rock can live

# intersect veg polygon with water niche model
areaPolygon(WaterModpolS) / 1e6 # 3225799
interVSWN <- raster::intersect(VegPolyS, WaterModpolS)
# this is zero, no intersection
# this is terr spp present where arb can live

# intersect rock polygon with dirt niche model
areaPolygon(DirtModpolS) / 1e6 # 5152820
interRSDN <- raster::intersect(RockPolyS, DirtModpolS)
areaPolygon(interRSDN) / 1e6 #557154
(557154/5152820)*100 #  10.8126
# this is rock spp present where dirt can live

# intersect rock polygon with veg niche model
areaPolygon(VegModpolS) / 1e6 # 1308248
interRSVN <- raster::intersect(RockPolyS, VegModpolS)
areaPolygon(interRSVN) / 1e6 # 1317.182
(1317.182/1308248)*100 # 0.1006829
# this is rock spp present where veg can live

# intersect rock polygon with Water niche model
areaPolygon(WaterModpolS) / 1e6 # 3225799
interRSWN <- raster::intersect(RockPolyS, WaterModpolS)
areaPolygon(interRSWN) / 1e6 # 535408.9
(535408.9/3225799)*100 # 16.59771
# this is rock spp present where water can live

# intersect water polygon with dirt niche model
areaPolygon(DirtModpolS) / 1e6 # 5152820
interWSDN <- raster::intersect(WaterPolyS, DirtModpolS)
areaPolygon(interWSDN) / 1e6 # 2615420
(2615420/5152820)*100 # 50.75706
# this is water spp present where dirt can live

# intersect Water polygon with veg niche model
areaPolygon(VegModpolS) / 1e6 # 1308248
interWSVN <- raster::intersect(WaterPolyS, VegModpolS)
areaPolygon(interWSVN) / 1e6 # 19864.68
(19864.68/1308248)*100 # 1.518419
# this is water spp present where veg can live

# intersect Water polygon with rock niche model
areaPolygon(RockModpolS) / 1e6 # 686940.1
interWSRN <- raster::intersect(WaterPolyS, RockModpolS)
areaPolygon(interWSRN) / 1e6 # 585585.8
(585585.8/686940.1)*100 # 85.24554
# this is water spp present where rock can live


##############################################################################################

# intersect dirt polygon with veg niche model
areaPolygon(VegModpolL) / 1e6 # 2401722
interDLVN <- raster::intersect(DirtPolyL, VegModpolL)
areaPolygon(interDLVN) / 1e6 # 1192061
(1192061/2401722)*100 # 49.6336
# this is dirt spp present where veg can live

# intersect dirt polygon with rock niche model
areaPolygon(RockModpolL) / 1e6 # 2313223
interDLRN <- raster::intersect(DirtPolyL, RockModpolL)
areaPolygon(interDLRN) / 1e6 # 1587893
(1587893/2313223)*100 # 68.64418
# this is dirt spp present where rock can live

# intersect dirt polygon with water niche model
areaPolygon(WaterModpolL) / 1e6 # 3251389
interDLWN <- raster::intersect(DirtPolyL, WaterModpolL)
areaPolygon(interDLWN) / 1e6 # 2375955
(2375955/3251389)*100 # 73.07508
# this is dirt spp present where water can live

# intersect veg polygon with dirt niche model
areaPolygon(DirtModpolL) / 1e6 # 4847568
interVLDN <- raster::intersect(VegPolyL, DirtModpolL)
areaPolygon(interVLDN) / 1e6 # 478055.7
(478055.7/4847568)*100 # 9.861764
# this is veg spp present where dirt can live

# intersect veg polygon with rock niche model
areaPolygon(RockModpolL) / 1e6 # 2313223
interVSRN <- raster::intersect(VegPolyL, RockModpolL)
areaPolygon(interVSRN) / 1e6 # 223573.9
(223573.9/2313223)*100 # 9.665039
# this is veg spp present where rock can live

# intersect veg polygon with water niche model
areaPolygon(WaterModpolL) / 1e6 # 3251389
interVLWN <- raster::intersect(VegPolyL, WaterModpolL)
areaPolygon(interVLWN) / 1e6 # 209403.2
(209403.2/3251389)*100 # 6.440423
# this is veg spp present where water can live

# intersect rock polygon with dirt niche model
areaPolygon(DirtModpolL) / 1e6 # 4847568
interRLDN <- raster::intersect(RockPolyL, DirtModpolL)
areaPolygon(interRLDN) / 1e6 #1804815
(1804815/4847568)*100 # 37.23135
# this is rock spp present where dirt can live

# intersect rock polygon with veg niche model
areaPolygon(VegModpolL) / 1e6 # 2401722
interRLVN <- raster::intersect(RockPolyL, VegModpolL)
areaPolygon(interRLVN) / 1e6 # 741989.2
(741989.2/2401722)*100 # 30.89405
# this is rock spp present where veg can live

# intersect rock polygon with Water niche model
areaPolygon(WaterModpolL) / 1e6 # 3251389
interRLWN <- raster::intersect(RockPolyL, WaterModpolL)
areaPolygon(interRLWN) / 1e6 # 1753772
(1753772/3251389)*100 # 53.93916
# this is rock spp present where water can live

# intersect water polygon with dirt niche model
areaPolygon(DirtModpolL) / 1e6 # 4847568
interWLDN <- raster::intersect(WaterPolyL, DirtModpolL)
areaPolygon(interWLDN) / 1e6 # 2556028
(2556028/4847568)*100 # 52.72805
# this is water spp present where dirt can live

# intersect Water polygon with veg niche model
areaPolygon(VegModpolL) / 1e6 # 2401722
interWLVN <- raster::intersect(WaterPolyL, VegModpolL)
areaPolygon(interWLVN) / 1e6 # 749869.8
(749869.8/2401722)*100 # 31.22217
# this is water spp present where veg can live

# intersect Water polygon with rock niche model
areaPolygon(RockModpolL) / 1e6 # 2313223
interWLRN <- raster::intersect(WaterPolyL, RockModpolL)
areaPolygon(interWLRN) / 1e6 #1862782
(1862782/2313223)*100 # 80.52756
# this is water spp present where rock can live

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
predictors.crop <- crop(x = ClimateData, y = geographic.extent)
predictors <- predictors.crop

# points
DirtPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Dirt_Points_strict/chull.shp")
VegPointsS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Veg_Points_strict/chull.shp")
DirtPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Dirt_Points_lenient/chull.shp")
VegPointsL <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Substrate_Strict_Len/Points/Veg_Points_lenient/chull.shp")

# transform points
DirtSDF <- data.frame(DirtPointsS)
DirtSDF <- DirtSDF[,1:2]
VegSDF <- data.frame(VegPointsS)
VegSDF <- VegSDF[,1:2]

DirtLDF <- data.frame(DirtPointsL)
DirtLDF <- DirtLDF[,1:2]
VegLDF <- data.frame(VegPointsL)
VegLDF <- VegLDF[,1:2]


# run the tests
# strict
IDTestS <- dismo::nicheEquivalency(sp1=DirtSDF, sp2=VegSDF, predictors = predictors,
                                   n=100, model=maxent, verbose=T)
chars <- capture.output(print(IDTestS))
writeLines(chars, con = file("output_sub_strict.txt"))
OutputSimple <- rbind(IDTestS$statistic, IDTestS$null.distribution)
D_Rand <- OutputSimple[-1,1]
D_Obs <- OutputSimple[1,1]
(length(which(D_Rand < D_Obs))+1)/100 # p = 0.01
I_Rand <- OutputSimple[-1,2]
I_Obs <- OutputSimple[1,2]
(length(which(I_Rand < I_Obs))+1)/100 # p = 0.01
write.csv(OutputSimple, "IandDOutput_subS.csv", row.names = F)
pdf("IandDSig_subS.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar = c(4,4,2,1))
hist(D_Rand, col = "blue", xlim = c(0, 1), main = "Shoener's D", xlab = "D", ylab = "Frequency")
abline(v = D_Obs, col = "blue", lwd = 3)
par(mar=c(4,2,2,1))
hist(I_Rand, col = "red", xlim = c(0, 1), main = "Warren's I", xlab = "I")
abline(v = I_Obs, col = "red", lwd = 3)
dev.off()

# lenient
IDTestL <- dismo::nicheEquivalency(sp1=DirtLDF, sp2=VegLDF, predictors = predictors,
                                    n=100, model=maxent, verbose=T)
chars <- capture.output(print(IDTestL))
writeLines(chars, con = file("output_sub_lenient.txt"))
OutputSimple <- rbind(IDTestL$statistic, IDTestL$null.distribution)
D_Rand <- OutputSimple[-1,1]
D_Obs <- OutputSimple[1,1]
(length(which(D_Rand < D_Obs))+1)/100 # p = 0.01
I_Rand <- OutputSimple[-1,2]
I_Obs <- OutputSimple[1,2]
(length(which(I_Rand < I_Obs))+1)/100 # p = 0.01
write.csv(OutputSimple, "IandDOutput_subL.csv", row.names = F)
pdf("IandDSig_subL.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar = c(4,4,2,1))
hist(D_Rand, col = "blue", xlim = c(0, 1), main = "Shoener's D", xlab = "D", ylab = "Frequency")
abline(v = D_Obs, col = "blue", lwd = 3)
par(mar=c(4,2,2,1))
hist(I_Rand, col = "red", xlim = c(0, 1), main = "Warren's I", xlab = "I")
abline(v = I_Obs, col = "red", lwd = 3)
dev.off()





















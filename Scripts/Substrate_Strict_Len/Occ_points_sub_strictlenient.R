###################################################
## MAKING OCC POINTS FOR SUBSTRATE strict and lenient ###
###################################################

# MAKING POINTS FROM POLYGONS TO USE IN MAXENT ANALYSIS

# Packages
library(raster); library(rgdal); library(dismo); library(rJava)
library(sdm); library(maptools); library(maxnet)
library(rgeos); library(mapdata); library(SDMTools)

# Polygons
Polygons <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

#check class
class(Polygons)

# check crs
crs(Polygons)
projection(Polygons) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
crs(Polygons)

# Check polygon labels
Polygons$binomial <- as.character(Polygons$binomial)
Polygons$binomial[which(Polygons$binomial == "Eurycea longicauda melanopleura")] <- "Eurycea longicauda" # combine polys of subspecies together
Polygons <- aggregate(Polygons, by = "binomial")

### load clasifications
Class <- read.csv("./Data/Pruned/MicrohabitatsPruned.csv")
Class$Species <- as.character(Class$Species)
Class$Species[which(Class$Species == "Pseudoeurycea lineolus")] <- "Pseudoeurycea lineola" #match the species names
Class1 <- Class[match(Polygons$binomial, Class$Species), ]
anyNA(Class1$Species) # want FALSE

# prune the polygons by substrate
#
###### substrate ########## 
################################ STRICT
#
### prune dirt
Dirt <- Class1[Class1$ArbVegStrict == "D", ]
Dirt$Species #189
DirtPoly <- Polygons[match(Dirt$Species,Polygons$binomial), ]
DirtPolyy <- rgeos::gBuffer(DirtPoly, width = 0, byid=F)
DirtPolyAll <- aggregate(DirtPolyy, dissolve=T)
DirtPolyAll.SP <- SpatialPolygonsDataFrame(DirtPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(DirtPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/DirtPoly_strict", layer= "chull", driver = "ESRI Shapefile")

# prune veg
Veg <- Class1[Class1$ArbVegStrict == "V", ]
Veg$Species #71
VegPoly <- Polygons[match(Veg$Species,Polygons$binomial), ]
VegPolyAll <- aggregate(VegPoly, dissolve=T)
VegPolyAll.SP <- SpatialPolygonsDataFrame(VegPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(VegPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/VegPoly_strict", layer= "chull", driver = "ESRI Shapefile")

# prune rock
Rock <- Class1[Class1$ArbVegStrict == "R", ]
Rock$Species #18
RockPoly <- Polygons[match(Rock$Species,Polygons$binomial), ]
RockPolyAll <- aggregate(RockPoly, dissolve=T)
RockPolyAll.SP <- SpatialPolygonsDataFrame(RockPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(RockPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/RockPoly_strict", layer= "chull", driver = "ESRI Shapefile")

# prune water
Water <- Class1[Class1$ArbVegStrict == "W", ]
Water$Species #32
WaterPoly <- Polygons[match(Water$Species,Polygons$binomial), ]
WaterPolyAll <- aggregate(WaterPoly, dissolve=T)
WaterPolyAll.SP <- SpatialPolygonsDataFrame(WaterPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(WaterPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/WaterPoly_strict", layer= "chull", driver = "ESRI Shapefile")

#
##################

# make points and point files with all of the PolyAll files

# FOR SUBSTRATE - DIRT 
#creating the grid to put the polygons in
gridSD <- raster(extent(DirtPolyAll))
res(gridSD) <- 0.360
proj4string(gridSD) <- proj4string(DirtPolyAll)
dry.gridSD <- raster::intersect(gridSD, DirtPolyAll)
proj4string(dry.gridSD) <- proj4string(DirtPolyAll)
eek <- rasterToPoints(dry.gridSD, spatial=T, progress="text")
ekek <- raster::intersect(eek, DirtPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
DirtPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(DirtPoints, "./Analysis_Scripts/Chapter3/Points/Dirt_Points_strict", layer= "chull", driver = "ESRI Shapefile",overwrite_layer = T)


# FOR SUBSTRATE - VEG -
#creating the grid to put the polygons in
gridSV <- raster(extent(VegPolyAll))
res(gridSV) <- 0.360
proj4string(gridSV) <- proj4string(VegPolyAll)
dry.gridSV <- raster::intersect(gridSV, VegPolyAll)
proj4string(dry.gridSV) <- proj4string(VegPolyAll)
eek <- rasterToPoints(dry.gridSV, spatial=T, progress="text")
ekek <- raster::intersect(eek, VegPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
VegPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(VegPoints, "./Analysis_Scripts/Chapter3/Points/Veg_Points_strict", layer= "chull", driver = "ESRI Shapefile",overwrite_layer = T)

# FOR SUBSTRATE - ROCK - 
#creating the grid to put the polygons in
gridSR <- raster(extent(RockPolyAll))
res(gridSR) <- 0.360
proj4string(gridSR) <- proj4string(RockPolyAll)
dry.gridSR <- raster::intersect(gridSR, RockPolyAll)
proj4string(dry.gridSR) <- proj4string(RockPolyAll)
eek <- rasterToPoints(dry.gridSR, spatial=T, progress="text")
ekek <- raster::intersect(eek, RockPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
RockPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(RockPoints, "./Analysis_Scripts/Chapter3/Points/Rock_Points_strict", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)

# FOR SUBSTRATE - WATER -
#creating the grid to put the polygons in
gridSW <- raster(extent(WaterPolyAll))
res(gridSW) <- 0.360
proj4string(gridSW) <- proj4string(WaterPolyAll)
dry.gridSW <- raster::intersect(gridSW, WaterPolyAll)
proj4string(dry.gridSW) <- proj4string(WaterPolyAll)
eek <- rasterToPoints(dry.gridSW, spatial=T, progress="text")
ekek <- raster::intersect(eek, WaterPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
WaterPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(WaterPoints, "./Analysis_Scripts/Chapter3/Points/Water_Points_strict", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)







############################# LENIENT
#
### prune dirt
Dirt <- Class1[Class1$ArbVegLenient == "D", ]
Dirt$Species #143
DirtPoly <- Polygons[match(Dirt$Species,Polygons$binomial), ]
DirtPolyy <- rgeos::gBuffer(DirtPoly, width = 0, byid=F)
DirtPolyAll <- aggregate(DirtPolyy, dissolve=T)
DirtPolyAll.SP <- SpatialPolygonsDataFrame(DirtPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(DirtPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/DirtPoly_lenient", layer= "chull", driver = "ESRI Shapefile")

# prune veg
Veg <- Class1[Class1$ArbVegLenient == "V", ]
Veg$Species #95
VegPoly <- Polygons[match(Veg$Species,Polygons$binomial), ]
VegPolyAll <- aggregate(VegPoly, dissolve=T)
VegPolyAll.SP <- SpatialPolygonsDataFrame(VegPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(VegPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/VegPoly_lenient", layer= "chull", driver = "ESRI Shapefile")

# prune rock
Rock <- Class1[Class1$ArbVegLenient == "R", ]
Rock$Species #35
RockPoly <- Polygons[match(Rock$Species,Polygons$binomial), ]
RockPolybuffer <- rgeos::gBuffer(RockPoly, width = 0, byid=F)
RockPolyAll <- aggregate(RockPolybuffer, dissolve=T)
RockPolyAll.SP <- SpatialPolygonsDataFrame(RockPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(RockPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/RockPoly_lenient", layer= "chull", driver = "ESRI Shapefile",overwrite_layer = T)

# prune water
Water <- Class1[Class1$ArbVegStrict == "W", ]
Water$Species #32
WaterPoly <- Polygons[match(Water$Species,Polygons$binomial), ]
WaterPolyAll <- aggregate(WaterPoly, dissolve=T)
WaterPolyAll.SP <- SpatialPolygonsDataFrame(WaterPolyAll, data=data.frame(binomial=1), match.ID = F)
writeOGR(WaterPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/WaterPoly_lenient", layer= "chull", driver = "ESRI Shapefile")

#
##################

# make points and point files with all of the PolyAll files

# FOR SUBSTRATE - DIRT -
#creating the grid to put the polygons in
gridSD <- raster(extent(DirtPolyAll))
res(gridSD) <- 0.360
proj4string(gridSD) <- proj4string(DirtPolyAll)
dry.gridSD <- raster::intersect(gridSD, DirtPolyAll)
proj4string(dry.gridSD) <- proj4string(DirtPolyAll)
eek <- rasterToPoints(dry.gridSD, spatial=T, progress="text")
ekek <- raster::intersect(eek, DirtPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
DirtPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(DirtPoints, "./Analysis_Scripts/Chapter3/Points/Dirt_Points_lenient", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)


# FOR SUBSTRATE - VEG - 
#creating the grid to put the polygons in
gridSV <- raster(extent(VegPolyAll))
res(gridSV) <- 0.360
proj4string(gridSV) <- proj4string(VegPolyAll)
dry.gridSV <- raster::intersect(gridSV, VegPolyAll)
proj4string(dry.gridSV) <- proj4string(VegPolyAll)
eek <- rasterToPoints(dry.gridSV, spatial=T, progress="text")
ekek <- raster::intersect(eek, VegPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
VegPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                    proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(VegPoints, "./Analysis_Scripts/Chapter3/Points/Veg_Points_lenient", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)

# FOR SUBSTRATE - ROCK 
#creating the grid to put the polygons in
gridSR <- raster(extent(RockPolyAll))
res(gridSR) <- 0.360
proj4string(gridSR) <- proj4string(RockPolyAll)
dry.gridSR <- raster::intersect(gridSR, RockPolyAll)
proj4string(dry.gridSR) <- proj4string(RockPolyAll)
eek <- rasterToPoints(dry.gridSR, spatial=T, progress="text")
ekek <- raster::intersect(eek, RockPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
RockPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(RockPoints, "./Analysis_Scripts/Chapter3/Points/Rock_Points_lenient", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)

# FOR SUBSTRATE - WATER -
#creating the grid to put the polygons in
gridSW <- raster(extent(WaterPolyAll))
res(gridSW) <- 0.360
proj4string(gridSW) <- proj4string(WaterPolyAll)
dry.gridSW <- raster::intersect(gridSW, WaterPolyAll)
proj4string(dry.gridSW) <- proj4string(WaterPolyAll)
eek <- rasterToPoints(dry.gridSW, spatial=T, progress="text")
ekek <- raster::intersect(eek, WaterPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
WaterPoints <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                      proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(WaterPoints, "./Analysis_Scripts/Chapter3/Points/Water_Points_lenient", layer= "chull", driver = "ESRI Shapefile", overwrite_layer = T)




######################################################
## MAKING OCC POINTS FOR MICROHABITAT using STRICT ###
######################################################

# resolution picked from tests 0.360 in this classification scheme

# MAKING POINTS FROM microhabitat POLYGONS TO USE IN MAXENT ANALYSIS with strict classification

# Packages
library(raster); library(rgdal); library(dismo); library(rJava)
library(sdm); library(maptools); library(maxnet)
library(rgeos); library(mapdata); library(SDMTools)

# Polygons
Polygons <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 

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

# prune the polygons micro habitat
##### microhabitat #########
#
### prune arboreal polys - A ##
Arb <- Class1[Class1$Strict == "A", ]
Arb$Species #56 species
ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]  
ArbPolyAll <- aggregate(ArbPoly, dissolve=T)
# below is making a file for the ArbPolygons but cannot use in making the points because of class ID
#ArbPolyAll.SP <- SpatialPolygonsDataFrame(ArbPolyAll, data=data.frame(binomial=1), match.ID = F)
#writeOGR(ArbPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/ArbPoly_strict", layer= "chull", driver = "ESRI Shapefile")
#ArbPolyAll <- readOGR("./Analysis_Scripts/Chapter3/Polygons/ArbPoly_strict/chull.shp")
#
### prune terrestrial polys - T ###
Terr <- Class1[Class1$Strict == "T", ]
Terr$Species #201 species
TerrPolygon <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPolybuffer <- rgeos::gBuffer(TerrPolygon, width = 0, byid=F)
TerrPolyAll <- aggregate(TerrPolybuffer, dissolve=T)
# below is making a file for the ArbPolygons but cannot use in making the points because of class ID
#TerrPolyAll.SP <- SpatialPolygonsDataFrame(TerrPolyAll, data=data.frame(binomial=1), match.ID = F)
#writeOGR(TerrPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/TerrPoly_strict", layer= "chull", driver = "ESRI Shapefile")
#TerrPolyAll <-readOGR("./Analysis_Scripts/Chapter3/Polygons/TerrPoly_strict/chull.shp")
#
### prune aquatic polys - W##
Aqua <- Class1[Class1$Strict == "W", ]
Aqua$Species # 32
AquaPoly <- Polygons[match(Aqua$Species, Polygons$binomial), ]
AquaPolyAll <- aggregate(AquaPoly, dissolve=T)
# below is making a file for the ArbPolygons but cannot use in making the points because of class ID
#AquaPolyAll.SP <- SpatialPolygonsDataFrame(AquaPolyAll, data=data.frame(binomial=1), match.ID = F)
#writeOGR(AquaPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/AquaPoly_strict", layer= "chull", driver = "ESRI Shapefile")
#AquaPolyAll <-readOGR("./Analysis_Scripts/Chapter3/Polygons/AquaPoly_strict/chull.shp")
#
### prune cave polys - C##
Cave <- Class1[Class1$Strict == "C", ]
Cave$Species # 11
CavePoly <- Polygons[match(Cave$Species, Polygons$binomial), ]
CavePolyAll <- aggregate(CavePoly, dissolve=T)
# below is making a file for the ArbPolygons but cannot use in making the points because of class ID
#CavePolyAll.SP <- SpatialPolygonsDataFrame(CavePolyAll, data=data.frame(binomial=1), match.ID = F)
#writeOGR(CavePolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/CavePoly_strict", layer= "chull", driver = "ESRI Shapefile")
#CavePolyAll <- readOGR("./Analysis_Scripts/Chapter3/Polygons/CavePoly_strict/chull.shp")
#
### prune saxicolous polys - S##
Sax <- Class1[Class1$Strict == "S", ]
Sax$Species # 7
SaxPoly <- Polygons[match(Sax$Species, Polygons$binomial), ]
SaxPolyAll <- aggregate(SaxPoly, dissolve=T)
# below is making a file for the ArbPolygons but cannot use in making the points because of class ID
#SaxPolyAll.SP <- SpatialPolygonsDataFrame(SaxPolyAll, data=data.frame(binomial=1), match.ID = F)
#writeOGR(SaxPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/SaxPoly_strict", layer= "chull", driver = "ESRI Shapefile")
#SaxPolyAll <- readOGR("./Analysis_Scripts/Chapter3/Polygons/SaxPoly_strict/chull.shp")
#
### prune fossorial polys - F##
Foss <- Class1[Class1$Strict == "F", ]
Foss$Species  # 3
FossPoly <- Polygons[match(Foss$Species, Polygons$binomial), ]
FossPolyAll <- aggregate(FossPoly, dissolve=T)
# below is making a file for the ArbPolygons but cannot use in making the points because of class ID
#FossPolyAll.SP <- SpatialPolygonsDataFrame(FossPolyAll, data=data.frame(binomial=1), match.ID = F)
#writeOGR(FossPolyAll.SP, "./Analysis_Scripts/Chapter3/Polygons/FossPoly_strict", layer= "chull", driver = "ESRI Shapefile")
#FossPolyAll <- readOGR("./Analysis_Scripts/Chapter3/Polygons/FossPoly_strict/chull.shp")
#
##############
# note that AllPolygon files have to be in SpatialPolygons format and not SpatialPolygonsDataFrame format!!!!
# make points and point files with all of the PolyAll files
#
## FOR MICROHABITAT - ARB - 432
# #creating the grid to put the polygons in
gridMA <- raster(extent(ArbPolyAll))
res(gridMA)
res(gridMA) <- 0.360
proj4string(gridMA) <- proj4string(ArbPolyAll)
dry.gridMAA <- raster::intersect(gridMA, ArbPolyAll)
proj4string(dry.gridMAA) <- proj4string(ArbPolyAll)
eek <- rasterToPoints(dry.gridMAA, spatial=T, progress="text")
ekek <- raster::intersect(eek, ArbPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
ArbPointsS <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(ArbPointsS, "./Analysis_Scripts/Chapter3/Points/Arb_Points_strict_ressmall", layer= "chull", driver = "ESRI Shapefile",
         overwrite=T)
#
## FOR MICROHABITAT - TERR - 3664
gridMA <- raster(extent(TerrPolyAll))
res(gridMA) <- 0.360
proj4string(gridMA) <- proj4string(TerrPolyAll)
dry.gridMAA <- raster::intersect(gridMA, TerrPolyAll)
proj4string(dry.gridMAA) <- proj4string(TerrPolyAll)
eek <- rasterToPoints(dry.gridMAA, spatial=T, progress="text")
ekek <- raster::intersect(eek, TerrPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
TerrPointsS <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(TerrPointsS, "./Analysis_Scripts/Chapter3/Points/Terr_Points_strict_ressmall", layer= "chull", driver = "ESRI Shapefile",
         overwrite=TRUE)

#
## FOR MICROHABITAT - AQUA 
gridMA <- raster(extent(AquaPolyAll))
res(gridMA) <- 0.360
proj4string(gridMA) <- proj4string(AquaPolyAll)
dry.gridMAA <- raster::intersect(gridMA, AquaPolyAll)
proj4string(dry.gridMAA) <- proj4string(AquaPolyAll)
eek <- rasterToPoints(dry.gridMAA, spatial=T, progress="text")
ekek <- intersect(eek, AquaPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
maybe
AquaPointsS <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                      proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(AquaPointsS, "./Analysis_Scripts/Chapter3/Points/Aqua_Points_strict_ressmall", layer= "chull", driver = "ESRI Shapefile",
         overwrite=T)

#
## FOR MICROHABITAT - CAVE
gridMA <- raster(extent(CavePolyAll))
res(gridMA) <- 0.360
proj4string(gridMA) <- proj4string(CavePolyAll)
dry.gridMAA <- raster::intersect(gridMA, CavePolyAll)
proj4string(dry.gridMAA) <- proj4string(CavePolyAll)
eek <- rasterToPoints(dry.gridMAA, spatial=T, progress="text")
ekek <- intersect(eek, CavePolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
CavePointsS <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                      proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(CavePointsS, "./Analysis_Scripts/Chapter3/Points/Cave_Points_strict_ressmall", layer= "chull", driver = "ESRI Shapefile",
         overwrite=T)

#
## FOR MICROHABITAT - SAX 
gridMA <- raster(extent(SaxPolyAll))
res(gridMA) <- 0.360
proj4string(gridMA) <- proj4string(SaxPolyAll)
dry.gridMAA <- raster::intersect(gridMA, SaxPolyAll)
proj4string(dry.gridMAA) <- proj4string(SaxPolyAll)
eek <- rasterToPoints(dry.gridMAA, spatial=T, progress="text")
ekek <- intersect(eek, SaxPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
maybe
SaxPointsS <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                      proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(SaxPointsS, "./Analysis_Scripts/Chapter3/Points/Sax_Points_strict_ressmall", layer= "chull", driver = "ESRI Shapefile",
         overwrite=T)
#
## FOR MICROHABITAT - FOSS 
gridMA <- raster(extent(FossPolyAll))
res(gridMA) <- 0.360
proj4string(gridMA) <- proj4string(FossPolyAll)
dry.gridMAA <- raster::intersect(gridMA, FossPolyAll)
proj4string(dry.gridMAA) <- proj4string(FossPolyAll)
eek <- rasterToPoints(dry.gridMAA, spatial=T, progress="text")
ekek <- intersect(eek, FossPolyAll)
coords <- ekek@coords
maybe <- data.frame(ekek)
maybe
FossPointsS <- SpatialPointsDataFrame(coords = coords, data = maybe, coords.nrs = numeric(0),
                                     proj4string = CRS(as.character('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')))
writeOGR(FossPointsS, "./Analysis_Scripts/Chapter3/Points/Foss_Points_strict_ressmall", layer= "chull", driver = "ESRI Shapefile",
         overwrite=T)

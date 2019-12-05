##########################################################
##########################################################
######## CLIMATE, POLYGON, POINT DATA WRANGLING ##########
##########################################################
##########################################################

# THIS IS FOR MAKING FILES TO SUMMARIZE OVER ENTIRE POLYGONS
# USED FOR STANDARD NORMAL DEVIATES ANALYSIS
# TO DECIDE WHAT WE SHOULD USE IN THE MODELS

# Load packages
library(maptools);library(raster);library(rgeos);library(rgdal)
library(sp);library(tidyr);library(dplyr);library(maps);library(ape)
library(stringr);library(ggmap);library(data.table);library(jpeg)
library(tiff);library(utils); library(biomod2)
library(rasterVis); library(elevatr)

# Get species names
# you need to make sure all caudata files are downloaded from the cloud before running this line
Caudata <- readOGR("Analysis_Scripts/Chapter3/CAUDATA/CAUDATA.shp")

# Exploring shape file
class(Caudata)
names(Caudata)
head(Caudata$id_no)
head(Caudata$binomial) # species names

# Prune shape files down to family = Plethodontidae
head(Caudata$family)
levels(Caudata$family)
PlethPolys <- Caudata[which(Caudata$family == "PLETHODONTIDAE"),]
PlethPolys$binomial <- as.character(PlethPolys$binomial)
length(unique(PlethPolys$binomial)) #379
writeOGR(obj=PlethPolys, dsn="Analysis_Scripts/Chapter3/Shapefiles/PlethPolys", layer="PlethPolys", driver="ESRI Shapefile", overwrite_layer = T)

# Selecting just our species
BB <- read.tree("Data/Pruned/BB.PrunedToPleths.tre")
BB$tip.label <- str_replace_all(BB$tip.label, "[[:punct:]]", " ") 
Species <- BB$tip.label
length(unique(BB$tip.label)) #327

#what we dont have polygons for
length(BB$tip.label[is.na(match(BB$tip.label, Caudata$binomial))]) # 41
length(BB$tip.label[is.na(match(BB$tip.label, PlethPolys$binomial))]) #41

#Which polygons don't match the tree names
PlethPolys$binomial[is.na(match(PlethPolys$binomial, BB$tip.label))] # 101

#Which species we dont have polygons for
BB$tip.label[is.na(match(BB$tip.label, PlethPolys$binomial))] #34

# Prune BB tree down to just shape file species
BBPruned <- drop.tip(BB, Species[which(is.na(match(Species, PlethPolys$binomial)))])
length(unique(BBPruned$tip.label)) #293
write.tree(BBPruned, file="Analysis_Scripts/Chapter3/Data/Pruned/BBPruned", digits = 20)

# Prune shape file down to just BB Species
PolyDrops <- which(is.na(match(PlethPolys$binomial, BBPruned$tip.label)))
PlethPolyPruned <- PlethPolys[-PolyDrops,]
PlethPolyPruned$binomial[which(is.na(match(PlethPolys$binomial, BBPruned$tip.label)))]
length(BBPruned$tip.label) # 293
length(unique(PlethPolyPruned$binomial)) #293
anyNA(match(BBPruned$tip.label, PlethPolyPruned$binomial)) # want False
sort(unique(PlethPolys$binomial)) # All the pleth species we have data for
writeOGR(obj=PlethPolyPruned, dsn="Analysis_Scripts/Chapter3/Shapefiles/PlethPolyPruned", layer="PlethPolyPruned", driver="ESRI Shapefile", overwrite_layer = T)

#assign spatial coordinates
proj4string(PlethPolyPruned) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
proj4string(BindedPolys) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#writeOGR(obj=BindedPolys, dsn="BindedPolys", layer="BindedPolys", driver="ESRI Shapefile")

######## merge polygons with same name #####
UniquePolys <- aggregate(BindedPolys, by="binomial")
length(UniquePolys@polygons)
sort(table(UniquePolys$binomial))
proj4string(UniquePolys) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
spTransform(UniquePolys, CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
proj4string(UniquePolys) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
#writeOGR(UniquePolys, "UniquePolys", layer="chull", driver="ESRI Shapefile")
UniquePolys <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/UniquePolys/chull.shp")
length(UniquePolys)
length(unique(UniquePolys$binomial))

##################################################################
############# CLIMATE ANALYSIS FOR POLYS #########################
##################################################################
  
for (i in 1:19) {
  #i = 1
  print(paste('Clipping current WORLDCLIM raster ', i))
  flush.console()
  wc.data <- raster(paste0('Analysis_Scripts/Chapter3/Climate Data/wc2/wc2.0_bio_2.5m_', i, '.tif'))
  wc.data <- crop(wc.data, extent(AllPolysforanalysis))
  wc.data <- setMinMax(wc.data)
  projection(wc.data) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  writeRaster(wc.data, paste0('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC2', ifelse(i < 10, '0', ''), i),
              format = 'GTiff', datatype = 'INT2S', overwrite = T)}

# MAKE RASTERS FOR STACK #
###########################
# Versions for plotting
wc01 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC201.tif')
#annual mean temp 
wc02 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC202.tif')
#mean diurnal range
wc03 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC203.tif')
#isothermality
wc04 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC204.tif')
#temperature seasonality
wc05 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC205.tif')
# max temp of warmest month
wc06 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC206.tif')
# min temp of coldest month
wc07 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC207.tif')
# temp annual range
wc08 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC208.tif')
# mean temp of wettest quarter
wc09 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC209.tif')
# mean temp of driest quarter
wc10 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC210.tif')
# mean temp of warm quarter
wc11 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC211.tif')
# mean temp of coldest quarter
wc12 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC212.tif')
# annual precipitation !!!!!!!!!!!!!!!!!
wc13 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC213.tif')
# precipitation of wettest month
wc14 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC214.tif')
# precipitation of driest month
wc15 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC215.tif')
# precipitation seasonality
wc16 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC216.tif')
# precipitation wettest quarter
wc17 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC217.tif')
# precipitation driest quarter
wc18 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC218.tif')
# precipitation of warmest quarter
wc19 <- raster::stack('Analysis_Scripts/Chapter3/Climate Data/Salamander Range/WC219.tif')
# precipitation of coldest quarter
###########################

# MAKE JPEG FILES FOR VISUALS # we dont really need these
###########################
jpeg(file = "./Salamander Climate Plots/WC201.jpg")
plot(wc01, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC202.jpg")
plot(wc02, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC203.jpg")
plot(wc03, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC204.jpg")
plot(wc04, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC205.jpg")
plot(wc05, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC206.jpg")
plot(wc06, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC207.jpg")
plot(wc07, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC208.jpg")
plot(wc08, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC209.jpg")
plot(wc09, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC210.jpg")
plot(wc10, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC211.jpg")
plot(wc11, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC212.jpg")
plot(wc12, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC213.jpg")
plot(wc13, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC214.jpg")
plot(wc14, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC215.jpg")
plot(wc15, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC216.jpg")
plot(wc16, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC217.jpg")
plot(wc17, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC218.jpg")
plot(wc18, col = topo.colors(255))
dev.off()
jpeg(file = "./Salamander Climate Plots/WC219.jpg")
plot(wc19, col = topo.colors(255))
dev.off()
###########################

# Get a single object that includes all climate data
current <- stack(list.files('Analysis_Scripts/Chapter3/Climate Data/Salamander Range', full.names = T, 
                            pattern = '.tif'))

# Set study extent for clipping
eco <- rgdal::readOGR('Analysis_Scripts/Chapter3/Borders', 'provinces', verbose = F)
eco <- eco[-which(eco$DOM_DESC == 'outside polygon'), ]
ecoContain <- eco[Polygons, ]
touchMatrix <- gTouches(eco, ecoContain, byid = T)
touchVector <- colSums(touchMatrix)
ecoStudyRegion <- eco[touchVector > 0, ]
ecoStudyRegion <- rbind(ecoStudyRegion, ecoContain, makeUniqueIDs = T)
studyExtent <- extent(ecoStudyRegion)
save(studyExtent, file = 'Analysis_Scripts/Chapter3/Climate Data/ASP', compress = T)

#unload tidyr because has same function extract and R cant tell difference
.rs.unloadPackage("tidyr")

# get elevation data #
elevation <- raster('Analysis_Scripts/Chapter3/Climate Data/alt_2-5m_bil/alt.bil')
elevation <- crop(elevation, AllPolysforanalysis)
#elevation <- crop(elevation, Polygons)
elevation <- setMinMax(elevation)

# elevation
projection(elevation) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
writeRaster(elevation, 'Analysis_Scripts/Chapter3/Climate Data/USETHISelevation',
            format = 'GTiff', datatype = 'INT2S', overwrite = T)
plot(elevation, main='Elevation (m)')

r.elev <- raster("Analysis_Scripts/Chapter3/Climate Data/USETHISelevation.tif")
r.slp <- terrain(r.elev, opt="slope", file="USETHISelevation", unit
                 ="degrees")
r.asp <- terrain(r.elev, opt="aspect", file="USETHISelevation.tif",
                 unit="degrees")
r.tri <- terrain(r.elev, opt="TRI", file="USETHISelevation.tif", overwrite=T)

# make data stack 
alldata <- stack(c('./Analysis_Scripts/Chapter3/Climate Data/elevation/USETHISelevation.tif',
                   list.files('./Analysis_Scripts/Chapter3/Climate Data/Salamander Range', full.names = T, pattern = '.tif')))

Records <- matrix(NA, nrow = 311, ncol = 121)
colnames(Records) <- c("binomial","elevationMin","elevation1Q","elevationMed","elevationMea","elevation3Q","elevationMax",
                       "WC1Min","WC11Q","WC1Med","WC1Mea","WC13Q","WC1Max",
                       "WC2Min","WC21Q","WC2Med","WC2Mea","WC23Q","WC2Max",
                       "WC3Min","WC31Q","WC3Med","WC3Mea","WC33Q","WC3Max",
                       "WC4Min","WC41Q","WC4Med","WC4Mea","WC43Q","WC4Max",
                       "WC5Min","WC51Q","WC5Med","WC5Mea","WC53Q","WC5Max",
                       "WC6Min","WC61Q","WC6Med","WC6Mea","WC63Q","WC6Max",
                       "WC7Min","WC71Q","WC7Med","WC7Mea","WC73Q","WC7Max",
                       "WC8Min","WC81Q","WC8Med","WC8Mea","WC83Q","WC8Max",
                       "WC9Min","WC91Q","WC9Med","WC9Mea","WC93Q","WC9Max",
                       "WC10Min","WC101Q","WC10Med","WC10Mea","WC103Q","WC10Max",
                       "WC11Min","WC111Q","WC11Med","WC11Mea","WC113Q","WC11Max",
                       "WC12Min","WC121Q","WC12Med","WC12Mea","WC123Q","WC12Max",
                       "WC13Min","WC131Q","WC13Med","WC13Mea","WC133Q","WC13Max",
                       "WC14Min","WC141Q","WC14Med","WC14Mea","WC143Q","WC14Max",
                       "WC15Min","WC151Q","WC15Med","WC15Mea","WC153Q","WC15Max",
                       "WC16Min","WC161Q","WC16Med","WC16Mea","WC163Q","WC16Max",
                       "WC17Min","WC171Q","WC17Med","WC17Mea","WC173Q","WC17Max",
                       "WC18Min","WC181Q","WC18Med","WC18Mea","WC183Q","WC18Max",
                       "WC19Min","WC191Q","WC19Med","WC19Mea","WC193Q","WC19Max")

RecordsDF <- as.data.frame(Records)
for (i in 1:311) {
  #i <- 4
  envSpecies <- data.frame(extract(alldata, AllPolysforanalysis[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  RecordsDF[i,] <-  c(as.character(AllPolysforanalysis[i,]$binomial),summary(envSpecies$USETHISelevation),summary(envSpecies$WC201),summary(envSpecies$WC202),
                      summary(envSpecies$WC203),summary(envSpecies$WC204),summary(envSpecies$WC205),
                      summary(envSpecies$WC206),summary(envSpecies$WC207),summary(envSpecies$WC208),
                      summary(envSpecies$WC209),summary(envSpecies$WC210),summary(envSpecies$WC211),
                      summary(envSpecies$WC212),summary(envSpecies$WC213),summary(envSpecies$WC214),
                      summary(envSpecies$WC215),summary(envSpecies$WC216),summary(envSpecies$WC217),
                      summary(envSpecies$WC218),summary(envSpecies$WC219))
  print(i)
}

RecordsDF
length(RecordsDF$binomial)
unique(RecordsDF$binomial)
write.csv(RecordsDF, file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Bioclim_IUCNLMRecords.csv")







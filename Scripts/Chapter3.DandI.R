# Chapter 3
# I and D significance tests

library(phyloclim)
library(geosphere) 
library(raster)
library(rgdal) 
library(rgeos)
library(dismo)

ClimateData <- raster::stack("Data/Raw/Bioclim and Shapefile Data/RawClimateFiles/ResizedClimateFilesForMaxEnt/AllDataTogetherFinalGTiff.gri")

ArbPolyS <- rgdal::readOGR("Data/Pruned/Chapter3/MaxEnt/ArbPoly_strict/chull.shp")
TerrPolyS <- rgdal::readOGR("Data/Pruned/Chapter3/MaxEnt/TerrPoly_strict/chull.shp")
ArbPointsS <- rgdal::readOGR("Data/Pruned/Chapter3/MaxEnt/Arb_Points_strict/chull.shp")
TerrPointsS <- rgdal::readOGR("Data/Pruned/Chapter3/MaxEnt/Terr_Points_strict/chull.shp")

# crop data to reasonable extent
max.lat = 140
min.lat = -140
max.lon = 70
min.lon = -20
geographic.extent <- extent(x = c(min.lat, max.lat, min.lon, max.lon))
predictors.crop <- crop(x = ClimateData, y = geographic.extent)
predictors <- predictors.crop

Sys.setenv(NOAWT=TRUE) 
library(rJava)
.jinit()

OutputTest <- dismo::nicheEquivalency(sp1=ArbPointsS, sp2=TerrPointsS, predictors = predictors,
                        n=1, model=maxent, verbose=T) 

OutputTest$statistic

Output <- dismo::nicheEquivalency(sp1=ArbPointsS, sp2=TerrPointsS, predictors = predictors,
                        n=100, model=maxent, verbose=T) # should be 33 hours (started 1030am thurs, done late Friday night)
OutputSimple <- rbind(Output$statistic, Output$null.distribution)

D_Rand <- OutputSimple[-1,1]
D_Obs <- OutputSimple[1,1]
(length(which(D_Rand < D_Obs))+1)/100 # p = 0.01

I_Rand <- OutputSimple[-1,2]
I_Obs <- OutputSimple[1,2]
(length(which(I_Rand < I_Obs))+1)/100 # p = 0.01

# writing output file and figures

write.csv(OutputSimple, "Data/Pruned/Chapter3/MaxEnt/IandDOutput.csv", row.names = F)

pdf("Figures Scripts/Chapter3/IandDSig.pdf", height = 3, width = 5)
par(mfrow=c(1,2), mar = c(4,4,2,1))
hist(D_Rand, col = "blue", xlim = c(0, 1), main = "Shoener's D", xlab = "D", ylab = "Frequency")
abline(v = D_Obs, col = "blue", lwd = 3)
par(mar=c(4,2,2,1))
hist(I_Rand, col = "red", xlim = c(0, 1), main = "Warren's I", xlab = "I")
abline(v = I_Obs, col = "red", lwd = 3)
dev.off()

# Run once and never need to again #
#library(devtools)
#install_github("johnbaums/rmaxent")
#install_github("danlwarren/ENMTools")
#library(rmaxent)
#get_maxent(version = "latest", quiet = FALSE)
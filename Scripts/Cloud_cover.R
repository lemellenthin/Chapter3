########################################
#### cloud cover new dataset ###########
########################################

# from website
# https://www.ipcc-data.org/observ/clim/cru_ts2_1.html

# load packages
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)


cloud.raw <- stack(list.files('./Analysis_Scripts/Chapter3/Climate Data/Cloud_cover/cru_cld_clim_1991-2000', full.names = T, 
                              pattern = '.tif'))
cloud.raw

rescale <- function(x, x.min = NULL, x.max = NULL, new.min = 0, new.max = 1) {
  if(is.null(x.min)) x.min = min(x)
  if(is.null(x.max)) x.max = max(x)
  new.min + (x - x.min) * ((new.max - new.min) / (x.max - x.min))
}

right <- rescale(cloud.raw, x.min = 0, x.max = 255, new.min = 0, new.max = 1)
right2 <- mean(right)
right2

writeRaster(right2, paste0('Analysis_Scripts/Chapter3/Climate Data/Cloud_cover/IPCC_data'),format = 'GTiff')




















# NASA <- raster("./Analysis_Scripts/Chapter3/Climate Data/globalcldfr_amo_200207-201504_geo.tif")
# identicalCRS(NASA, Polygons)
# NASA <- projectRaster(NASA, crs=crs(Polygons))
# NASA.p <- rasterToPoints(NASA)
# NASA.df <- data.frame(NASA.p)
# colnames(NASA.df) <- c("x","y","MAP")
# summary(NASA.df)
# NASA1 <- crop(NASA, alldata_tog)
# writeRaster(NASA1, filename="./Analysis_Scripts/Chapter3/Climate Data/Cropped_Cloud.tif")

NASA <- raster("./Analysis_Scripts/Chapter3/Climate Data/Cropped_Cloud.tif")

# make note of the units of each var somewhere
# edges, bounces, no data how do you check this and play around with what you want
# look at the different cut off points
# make midquart as the midpoint which can pull it apart enough

AllPolysforanalysis <- readOGR("./Analysis_Scripts/Chapter3/Shapefiles/AllPolysforAnalysis/chull.shp")

Records <- matrix(NA, nrow = 311, ncol = 7)
colnames(Records) <- c("binomial","CloudMin","Cloud1Q","CloudMed","CloudMea","Cloud3Q","CloudMax")

RecordsDF <- as.data.frame(Records)
for (i in 1:311) {
  #i <- 4
  envSpecies <- data.frame(extract(NASA, AllPolysforanalysis[i,]))
  if (any(is.na(rowSums(envSpecies)))) envSpecies <- envSpecies[-which(is.na(rowSums(envSpecies))), ]
  RecordsDF[i,] <-  c(as.character(AllPolysforanalysis[i,]$binomial),summary(envSpecies))
  print(i)
}

RecordsDF
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Max.   :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Mean   :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Median :", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="\\Min.   :", replacement="")

RecordsDF[] <- lapply(RecordsDF, gsub, pattern="1st Qu.:", replacement="")
RecordsDF[] <- lapply(RecordsDF, gsub, pattern="3rd Qu.:", replacement="")
RecordsDF
write.csv(RecordsDF, file = "Analysis_Scripts/Chapter3/Climate Data/Output_for_Analysis/Cloud_cover.csv")

























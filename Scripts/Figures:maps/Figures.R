############################################
## tables and figures #####################
###########################################
# SCRIPT TO MAKE FIGURES FOR POSTERS/PAPER

library(expss); library(tables); library(grDevices)
require(graphics); library(ggthemes); library(ggmap)
library(gridExtra); library(ape); library(stringr)
library(phytools); library(mapdata); library(ggplot2)
library(readxl); library(utils); library(leaflet)
library(tidyr); library(tidyverse); library(rnaturalearth)


# phylogeny
ColsBasic <- c("#D55E00","#56B4E9")
Phylo <- read.tree(file = "Analysis_Scripts/Chapter3/Data/Pruned/BBPruned.tre")
Phylo$tip.label <- str_replace_all(Phylo$tip.label, "_", " ")
Cols <- as.character(Class1$Strict[match(Phylo$tip.label, Class1$Species)])
Cols[which(Cols == "A")] <- ColsBasic[2]
Cols[which(Cols != ColsBasic[2])] <- ColsBasic[1]
pdf("PhyloColored.pdf", width = 3, height = 21)
par(mfrow = c(1,1), mar = c(0,1,1,0))
plot.phylo(Phylo, tip.color = Cols, cex = 0.3)
dev.off()

# load the maxent predictions
ArbC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapAC5.grd")
TerrC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapTC5.grd")
ArbModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_strict.grd")
TerrModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/TerrMod_prediction_strict.grd")

# load the polygons
ArbPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/ArbPolyAll/chull.shp")
TerrPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/TerrPolyAll/chull.shp")

# get the colors
jet.colors <-
  colorRampPalette(c("white","#D55E00","#56B4E9","#56B4E9"))( 80 )

# set margins
par(mfrow = c(1,1), mar = c(1,1,1,1))
#plot(ArbC5,col=jet.colors) # testing to see what the colors look like

#land
#data(land) doesn't work anymore with the new update of ggplot
land <- rnaturalearth::ne_coastline()
plot(land)

# project the polygons
TerrPolySP <- spTransform(TerrPoly, CRSobj = crs(land))
ArbPolySP <- spTransform(ArbPoly, CRSobj = crs(land))

#plot testing
plot(land, xlim=c(-150,-10), ylim=c(-20,55), axes=F, col="white",bg='light gray') 
plot(ArbPolySP, add=T)



# Arboreal niche and terrestrial polygon
png("./Analysis_Scripts/Chapter3/Docs/Figures/ArbNiche_Terrpoly6.png", width = 2000, height = 1500)
plot(land, xlim=c(-150,-10), ylim=c(-20,55), axes=F, col="white",bg='light gray') 
plot(ArbModS, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F, add=T)
plot(TerrPolySP, add=T)
dev.off()

# Terrestrial niche and arboreal polygon
png("./Analysis_Scripts/Chapter3/Docs/Figures/TerrNiche_Arbpoly6.png", width = 2000, height = 1500)
plot(land, xlim=c(-150,-10), ylim=c(-20,55), axes=F, col="white",bg='light gray') 
plot(TerrModS, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F, add=T)
plot(ArbPolySP, add=T)
dev.off()

# Arboreal niche and arboreal polygon
png("./Analysis_Scripts/Chapter3/Docs/Figures/ArbNiche_Arbpoly6.png", width = 2000, height = 1500)
plot(land, xlim=c(-150,-10), ylim=c(-20,55), axes=F, col="white",bg='light gray') 
plot(ArbModS, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F, add=T)
plot(ArbPolySP, add=T)
dev.off()

# Terrestrial niche and terrestrial polygon
png("./Analysis_Scripts/Chapter3/Docs/Figures/TerrNiche_Terrpoly6.png", width = 2000, height = 1500)
plot(land, xlim=c(-150,-10), ylim=c(-20,55), axes=F, col="white",bg='light gray') 
plot(TerrModS, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F, add=T)
plot(TerrPolySP, add=T)
dev.off()




# playing around with x and y lims
plot(land, xlim=c(-120,-30), ylim=c(10,20), axes=F, col="white",bg='light gray')
plot(TerrModS, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-120,-30), 
     ylim = c(-10,30), legend=F, add=T)
ArbPolySP <- spTransform(ArbPoly, CRSobj = crs(land))
ArbPolySP <- spTransform(ArbPolySP, CRSobj = crs(TerrModS))
plot(ArbPolySP, add=T)


####################################################################################################

# THIS DOESN'T WORK AND CREATES MANY PDFS FOR EACH? 
# have not trouble shooted yet
pdf("Sample4panelplot.pdf", width = 7, height = 5)
par(mar = c(1,.001,.05,.01), mfrow=c(3,3))
plot.new()
plot.new()
text(x=.5,y=.15,paste("Arboreal\n", 
                     "Niche Model"), cex = 1.6, col = "black")
plot.new()
text(x=.5,y=.15,paste("Terrestrial\n", 
                     "Niche Model"), cex = 1.6, col = "black")
plot.new()
text(x=.75,y=.5,paste("Arboreal\n", 
                     "Distribution"), cex = 1.6, col = "black",srt=90)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(ArbC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F)
plot(ArbModS, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F)
plot(ArbPoly, add = T,lwd=2)
#text("59.3% Overlap", x=-110, y=0,cex = 1)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(TerrC5, col=jet.colors, xaxt = "n", yaxt = "n",xlim = c(-150,-10), 
             ylim = c(-20,55),legend=F)
plot(ArbPoly, add = T,lwd=2)

text("4.2% Overlap", x=-110, y=0,cex = 1)
plot.new()
text(x=.85,y=.5,paste("Terrestrial\n", 
                     "Distribution"), cex = 1.6, col = "black",srt=90)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(ArbC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), ylim = c(-20,55),legend=F)
plot(TerrPoly, add = T,lwd=2)

text("35.0% Overlap", x=-110, y=0,cex = 1)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(TerrC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), ylim = c(-20,55),legend=F)
plot(TerrPoly, add = T,lwd=2)
text("84.4% Overlap", x=-110, y=0,cex = 1)
dev.off()



#############################################
## ERICA FIGURE #############################
##############################################

# we want to get a list of species that are overlapping with the 0.5 arboreal suitability cutoff
# mainly in the tropics but also in texas and florida
# terrestrial species only - strict classification

# download arboreal suitability strict map
ArbModS <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/ArbMod_prediction_strict.grd")
ArbModSSS <- ArbModS > 0.5
ArbModpolS <- rasterToPolygons(ArbModSSS,function(x) x == 1,dissolve=T)

# all of the species distributions
# define terrestrial polygons
Polygons <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 
Polygons$binomial <- as.character(Polygons$binomial)
Polygons$binomial[which(Polygons$binomial == "Eurycea longicauda melanopleura")] <- "Eurycea longicauda" # combine polys of subspecies together
Polygons <- aggregate(Polygons, by = "binomial")
Class <- read.csv("./Data/Pruned/MicrohabitatsPruned.csv")
Class$Species <- as.character(Class$Species)
Class$Species[which(Class$Species == "Pseudoeurycea lineolus")] <- "Pseudoeurycea lineola" #match the species names
Class1 <- Class[match(Polygons$binomial, Class$Species), ]
anyNA(Class1$Species) # want FALSE
Terr <- Class1[Class1$Strict == "T", ]
Terr$Species #201
TerrPoly <- Polygons[match(Terr$Species, Polygons$binomial), ] 
TerrPoly$binomial #201
plot(TerrPoly)

Arboreal <- buffer(ArbModpolS, width=0)
Terrspecies <- buffer(ArbPoly, width=0)
idk <- intersect(ArbPoly, TerrPoly)
idk@polygons

# leaflet
leaflet:::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet:::addPolygons(data = ArbPolyB,
                        color = "green",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group= "Arb",
                        popup = ArbPolyB$binomial
                        ) %>%
  leaflet:::addPolygons(data = TerrPolyB,
                        color = c("red"),
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group = "Terr",
                        popup = TerrPolyB$binomial
                        ) 

leaflet:::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet:::addPolygons(data = AllNew,
                        color = "green",
                        weight = 1,
                        smoothFactor = 0.1,
                        opacity = 1.0,
                        fillOpacity = 0.5,
                        group= "Arb",
                        popup = AllNew$binomial
  )


#########################################################################
## this is a box plot of all the AUC scoring for all the k-fold runs ####
#########################################################################

# read in the data
AUCscores <- read.csv("./Analysis_Scripts/Chapter3/Docs/AUC_scores.csv", header=T)
class(AUCscores)
class(AUCscores$Maxent.Run)

boxplot <- graphics::boxplot(formula=AUC~Maxent.Run, data=AUCscores,
                             main="AUC Scores for k-fold runs", xlab= "Classification scheme",
                             ylab="AUC Score")

########## with ggplot
p <- ggplot(AUCscores, aes(Maxent.Run, AUC))
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 60, hjust = 1))




































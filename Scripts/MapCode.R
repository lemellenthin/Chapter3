library(devtools)
install_github(repo = "liamrevell/phytools")
library(phytools)
library(mapdata)
library(rgdal)
library(viridis)
library(stringr)
library(raster)
library(rgeos)

BBTree <- read.tree("Analysis_Scripts/Chapter3/Data/Pruned/BBPruned.tre")
BBTree$tip.label <- str_replace_all(BBTree$tip.label, "_", " ")
#LatLong <- readOGR("Analysis_Scripts/Chapter3/Points/All_Poly_Points.csv/chull.shp")
# ArbLatLon <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Arb_Points_strict_ressmall/chull.shp")
# TerrLatLon <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Points/Terr_Points_strict_ressmall/chull.shp")
# ALL <- as.data.frame(ArbLatLon)
# TLL <- as.data.frame(TerrLatLon)
# # ArbPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/ArbPoly_strict/chull.shp")
# # TerrPolyS <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Scripts/Strict/Polygons/TerrPoly_strict/chull.shp")
# Polygons <- readShapePoly("Analysis_Scripts/Chapter3/Shapefiles/AllPolysforanalysis/chull.shp") 
# 
# # Check polygon labels
# Polygons$binomial <- as.character(Polygons$binomial)
# Polygons$binomial[which(Polygons$binomial == "Eurycea longicauda melanopleura")] <- "Eurycea longicauda" # combine polys of subspecies together
# Polygons <- aggregate(Polygons, by = "binomial")
# Polygons$binomial

### load clasifications
# Class <- read.csv("./Data/Pruned/MicrohabitatsPruned.csv")
# Class$Species <- as.character(Class$Species)
# Class$Species[which(Class$Species == "Pseudoeurycea lineolus")] <- "Pseudoeurycea lineola" #match the species names
# Class1 <- Class[match(Polygons$binomial, Class$Species), ]
# anyNA(Class1$Species) # want FALSE
# Arb <- Class1[Class1$Strict == "A", ]
# Arb$Species #60 is around the right number
# ArbPoly <- Polygons[match(Arb$Species,Polygons$binomial), ]   #56
# Terr <- Class1[Class1$Strict == "T", ]
# Terr$Species #207
# TerrPoly <- Polygons[match(Terr$Species, Polygons$binomial), ] #201
# 
# ArbPoly$binomial <- as.character(ArbPoly$binomial)
# TerrPoly$binomial <- as.character(TerrPoly$binomial)
# 
# ArbNames <- matrix(unlist(ArbPoly$binomial))
# ArbNames
# TerrNames <- matrix(unlist(TerrPoly$binomial))
# TerrNames


# Caudata <- readOGR("Analysis_Scripts/Chapter3/CAUDATA/CAUDATA.shp")
# # Prune shape files down to family = Plethodontidae
# PlethPolys <- Caudata[which(Caudata$family == "PLETHODONTIDAE"),]
# PlethPolys1 <- PlethPolys[which(PlethPolys$genus == "Bolitoglossa"),]
# PlethPolys$binomial <- as.character(PlethPolys$binomial)
# PlethPolys1$binomial <- as.character(PlethPolys1$binomial)
# length(unique(PlethPolys$binomial)) #379
# writeOGR(obj=PlethPolys, dsn="Analysis_Scripts/Chapter3/Shapefiles/PlethPolys", layer="PlethPolys", driver="ESRI Shapefile", overwrite_layer = T)
# writeOGR(obj = PlethPolys1, dsn = "Analysis_Scripts/Chapter3/Shapefiles/PlethPolysBol", layer="PlethPolys1", driver="ESRI Shapefile")

Bol <- readOGR("Analysis_Scripts/Chapter3/Shapefiles/PlethPolysBol/PlethPolys1.shp")
Class <- read.csv("./Data/Pruned/MicrohabitatsPruned.csv")
Class$Species <- as.character(Class$Species)
Class1 <- Class[match(Bol$binomial, Class$Species), ]
Class1 <- na.omit(Class1)
Class1$Species
anyNA(Class1$Species) # want FALSE

Arb <- Class1[Class1$Strict == "A", ]
Arb$Species #60 is around the right number
ArbPolyB <- Polygons[match(Arb$Species,Polygons$binomial), ]   #56
ArbPolyB$binomial # 30

Terr <- Class1[Class1$Strict == "T", ]
Terr$Species #207
TerrPolyB <- Polygons[match(Terr$Species, Polygons$binomial), ] #201
TerrPolyB$binomial #36

# LatLong$binomial <- as.character(LatLong$binomial)
# LatLong <- LatLong[which(LatLong$binomial %in% "Eurycea longicauda melanopleura" == "FALSE"),]
# LatLongNamesLong <- strsplit(LatLong$binomial, split = " ")
# LatLongNamesLongMat  <- matrix(unlist(LatLongNamesLong), ncol=2, byrow=TRUE)
# LatLongNames <- LatLongNamesLongMat[,1]

# Groupings <- as.factor(LatLongNames[match(unique(LatLong$binomial), LatLong$binomial)])
# GroupingsA <- as.factor(ArbNames[match(unique(ArbPoly$binomial), ArbPoly$binomial)])
# GroupingsT <- as.factor(TerrNames[match(unique(TerrPoly$binomial), TerrPoly$binomial)])
# names(GroupingsA) <- unique(ArbPoly$binomial)
# names(GroupingsT) <- unique(TerrPoly$binomial)
# class(GroupingsA)
# GroupingsA[,1]
# ArbPolyB$binomial
# Groupings <- cbind(ArbPolyB$binomial, TerrPolyB$binomial)
# ArbNames <- as.character(ArbPolyB$binomial)
# ArbNames <- as.list(ArbNames)
# class(ArbNames)
# TerrNames <- as.character(TerrPolyB$binomial)
# TerrNames <- as.list(TerrNames)
# Groupings <- c(ArbNames, TerrNames)
# Groupings <- as.character(Groupings)
# Groupings
# class(BBTree$tip.label)

AllNewLMPolys <- c(ArbPolyB,TerrPolyB)

# bind all and check names
AllNew <- do.call(bind, AllNewLMPolys)
AllNew$binomial
AllNew <- aggregate(AllNew, by = "binomial")

Drop <- BBTree$tip.label[!(BBTree$tip.label %in% AllNew$binomial)]
BBPruned <- drop.tip(BBTree, tip = Drop)

anyNA(match(BBPruned$tip.label, AllNew$binomial)) # want FALSE
anyNA(match(AllNew$binomial, BBPruned$tip.label)) # want FALSE

BBPruned$tip.label <- as.character(AllNew$binomial[match(BBPruned$tip.label, AllNew$binomial)])

plot.phylo(BBPruned)

for (i in 1:length(unique(BBPruned$tip.label))) {
  if (i == 1) {
    Tree <- BBPruned
  }
  NumInGroup <- length(which(Tree$tip.label %in% unique(Tree$tip.label)[i]))
  
  if (NumInGroup > 1) {
    Tree$tip.label[which(Tree$tip.label %in% unique(Tree$tip.label)[i])][2:NumInGroup] <-"DropMe"
    Tree <- drop.tip(Tree, tip = "DropMe")
  }
}

plot(Tree)
Tree

# TreeAmerica <- drop.tip(Tree, tip = c("Hydromantes", "Karsenia"))
LatLongAmerica <- LatLong[!(row.names(LatLong) %in% c("Hydromantes", "Karsenia")),]


cols<-setNames(sample(viridis(n=Ntip(Tree))),Tree$tip.label)
cols2 <- cbind(cols, cols)
cols2

cols <-setNames(sample(rainbow(n=Ntip(Tree))),Tree$tip.label)

leg_centers <- SpatialPointsDataFrame(gCentroid(AllNew, byid=TRUE), 
                                      AllNew@data, match.ID=T)

leg_centers@coords
leg_centers@coords[,2]
DFcoords <- data.frame(matrix(ncol=2, nrow=63))
x <- c("lat","long")
colnames(DFcoords) <- x
DFcoords$lat <- leg_centers@coords[,2]
DFcoords$long <- leg_centers@coords[,1]
rownames(DFcoords) <- leg_centers$binomial
DFcoords

Tree$tip.label
DFcoords
class(Tree)
class(Tree$tip.label)

obj <- phylo.to.map(Tree, idk, plot=F, rotate = T, database="worldHires",direction = "rightwards",
                    type="phylogram")
plot(obj, colors = cols, fsize = 0.6, ftype='i', lwd=c(1,2), direction = "rightwards", xlim = c(-100, -70), ylim = c(5, 15),
     cex.points=c(0.7,1.2))
plot(AllNew, add=T)

anyNA(match(rownames(DFcoords),Tree$tip.label))
match(rownames(DFcoords),Tree$tip.label)
dim(DFcoords)
class(DFcoords)

idk <- as.matrix(DFcoords)
idk



# just polygon means in America
LatLongAmericaMeans <- aggregate(LatLongAmerica[, 1:2], list(rownames(LatLongAmerica)), mean)
rownames(LatLongAmericaMeans) <- LatLongAmericaMeans$Group.1
LatLongAmericaMeans <- LatLongAmericaMeans[,-1]
obj<-phylo.to.map(tree = Tree, coords = LatLongAmericaMeans, database="worldHires", plot=FALSE, rotate = TRUE, direction = "rightwards")
plot(obj, colors = cols2, ftype="i", lwd=c(3,1), fsize = .8, direction = "rightwards", xlim = c(-125, -70), ylim = c(0, 50))


# just polygon means all
LatLongMeans <- aggregate(LatLong[, 1:2], list(rownames(LatLong)), mean)
rownames(LatLongMeans) <- LatLongMeans$Group.1
LatLongMeans <- LatLongMeans[,-1]
obj<-phylo.to.map(tree = Tree, coords = LatLongMeans, database="worldHires", plot=FALSE, rotate = TRUE, direction = "rightwards")
obj$coords

cols<-setNames(sample(viridis(n=Ntip(Tree))),Tree$tip.label)
cols[which(names(cols)== "Hydromantes")] <- "transparent"
cols[which(names(cols)== "Karsenia")] <- "transparent"
cols2 <- cbind(cols, cols)
tree <- obj$tree
coords <- obj$coords
colors <- cols2
ftype <- "i"
fsize <- 2
cex.points <- c(2,3)
map <- obj$map
mar<- rep(0, 4)
lwd <- c(4,3)
ftype <- (which(c("off", "reg", "b", "i", "bi") == ftype) - 1)
xlim <- c(-130, -70)
ylim <- c(-5, 55)
xlim <- c(xlim[1] - split[1]/split[2] * (xlim[2] - xlim[1]), xlim[2])

pdf("Figures Scripts/Map.GenusDistributionAmerica.pdf", width = 25, height = 20)
   par(mar = rep(0, 4))
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = asp)
    map(map, add = TRUE, fill = TRUE, col = "gray95", mar = rep(0, 4))
    
        cw <- reorder(tree, "cladewise")
        n <- Ntip(cw)
        
            dy <- abs(diff(ylim))
            sh <- fsize * strwidth(paste(" ", cw$tip.label, sep = "")) + 
                0.2 * fsize * strwidth("W")
            cw$edge.length <- cw$edge.length/max(nodeHeights(cw)) * 
                (split[1] * (xlim[2] - xlim[1]) - max(sh))
            pw <- reorder(cw, "postorder")
            y <- vector(length = n + cw$Nnode)
            y[cw$edge[cw$edge[, 2] <= n, 2]] <- 0:(n - 1)/(n - 
                1) * (ylim[2] - ylim[1]) + ylim[1]
            nn <- unique(pw$edge[, 1])
            for (i in 1:length(nn)) {
                yy <- y[pw$edge[which(pw$edge[, 1] == nn[i]), 
                  2]]
                y[nn[i]] <- mean(range(yy))
            }
            H <- nodeHeights(cw)
            X <- xlim[1] + H
            coords <- coords[cw$tip.label, 2:1]
            for (i in 1:n) lines(c(X[which(cw$edge[, 2] == i), 
                2] + if (from.tip) 0 else sh[i], coords[i, 1]), 
                c(y[i], coords[i, 2]), col = colors[cw$tip.label, ][i, 1], lty = lty, lwd = lwd[2], bg = "red")
            points(coords, pch = pch, cex = cex.points[2], bg = colors[cw$tip.label, 2])
            for (i in 1:nrow(X)) lines(X[i, ], rep(y[cw$edge[i, 2]], 2), lwd = lwd[1], lend = 2, bg = "red")
            for (i in 1:cw$Nnode + n) lines(X[which(cw$edge[,  1] == i), 1], range(y[cw$edge[which(cw$edge[, 1] == i), 2]]), lwd = lwd[1], lend = 2, bg = "transparent")
            for (i in 1:n) {
                  text(X[which(cw$edge[, 2] == i), 2], y[i], paste(" ", sub("_", " ", cw$tip.label[i]), sep = ""), 
                       pos = 4, offset = 0.1, cex = fsize, font = ftype)
            }
            PP <- list(type = "phylogram", use.edge.length = TRUE, 
                node.pos = 1, show.tip.label =  TRUE, bg = "transparent",
                show.node.label = FALSE, font = (which(c("off", "reg", "b", "i", "bi") == ftype) - 1), cex = fsize, 
                adj = 0, srt = 0, no.margin = FALSE, label.offset = 0.1, 
                x.lim = par()$usr[1:2], y.lim = par()$usr[3:4], 
                direction = direction, tip.color = "black", Ntip = Ntip(cw), 
                Nnode = cw$Nnode, edge = cw$edge, xx = sapply(1:(Ntip(cw) + 
                  cw$Nnode), function(x, y, z) y[match(x, z)], 
                  y = X, z = cw$edge), yy = y)
        
        assign("last_plot.phylo", PP, envir = .PlotPhyloEnv)
dev.off()

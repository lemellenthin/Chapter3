# install_github(repo = "liamrevell/phytools")
library(phytools)
library(mapdata)
library(rgdal)
library(viridis)

BBTree <- read.tree("Data/Pruned/BB.PrunedToPleths.tre")
BBTree$tip.label <- str_replace_all(BBTree$tip.label, "_", " ")
LatLong <- readOGR("Analysis_Scripts/Chapter3/Points/All_Poly_Points.csv/chull.shp")
LatLong <- as.data.frame(LatLong)
LatLong$binomial <- as.character(LatLong$binomial)
LatLong <- LatLong[which(LatLong$binomial %in% "Eurycea longicauda melanopleura" == "FALSE"),]
LatLongNamesLong <- strsplit(LatLong$binomial, split = " ")
LatLongNamesLongMat  <- matrix(unlist(LatLongNamesLong), ncol=2, byrow=TRUE)
LatLongNames <- LatLongNamesLongMat[,1]

Groupings <- as.factor(LatLongNames[match(unique(LatLong$binomial), LatLong$binomial)])
names(Groupings) <- unique(LatLong$binomial)

LatLong <- as.matrix(LatLong[,c(3,2)])
row.names(LatLong) <- LatLongNames
colnames(LatLong) <- c("lat", "long")

Drop <- BBTree$tip.label[!(BBTree$tip.label %in% names(Groupings))]
BBPruned <- drop.tip(BBTree, tip = Drop)

anyNA(match(BBPruned$tip.label, names(Groupings))) # want FALSE
anyNA(match(names(Groupings), BBPruned$tip.label)) # want FALSE

BBPruned$tip.label <- as.character(Groupings[match(BBPruned$tip.label, names(Groupings))])

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

TreeAmerica <- drop.tip(Tree, tip = c("Hydromantes", "Karsenia"))
LatLongAmerica <- LatLong[!(row.names(LatLong) %in% c("Hydromantes", "Karsenia")),]


cols<-setNames(sample(viridis(n=Ntip(TreeAmerica))),TreeAmerica$tip.label)
cols2 <- cbind(cols, cols)

obj<-phylo.to.map(tree = TreeAmerica, coords = LatLongAmerica, database="worldHires", plot=FALSE, rotate = TRUE, direction = "rightwards")
obj

plot(obj, colors = cols2, ftype="i", lwd=c(3,1), fsize = .6, direction = "rightwards", xlim = c(-125, -70), ylim = c(0, 50))

# just polygon means in America

LatLongAmericaMeans <- aggregate(LatLongAmerica[, 1:2], list(rownames(LatLongAmerica)), mean)
rownames(LatLongAmericaMeans) <- LatLongAmericaMeans$Group.1
LatLongAmericaMeans <- LatLongAmericaMeans[,-1]


class(LatLongAmericaMeans$lat)
TreeAmerica$tip.label
class(TreeAmerica$tip.label)

obj<-phylo.to.map(tree = TreeAmerica, coords = LatLongAmericaMeans, database="worldHires", plot=FALSE, rotate = TRUE, direction = "rightwards")
obj$coords

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
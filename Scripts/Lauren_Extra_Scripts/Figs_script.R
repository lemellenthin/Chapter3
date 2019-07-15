############################################
## tables and figures? #####################
###########################################

install.packages("tables")

library(expss)
library(tables)

datame <- readxl::read_excel("./Analysis_Scripts/Chapter3/Data.xlsx", col_names = T)
datame

mah <- cro(datame$Model, datame$`Mean AUC`)

# at the end 
# pdf("myfile.pdf")
# plot(x,y)
  # this also has par() or dim in it
# dev.off()

library(grDevices) # add nice colors
require(graphics)
install.packages("ggthemes")
library(ggthemes)
?colorRampPalette()
palet.col <- colorRampPalette(c('deepskyblue','green','yellow', 'red'))( 80 )
#palet.col <- colorRampPalette(c("red", "white", "blue"))
palet.col <- colorRampPalette(c('deepskyblue','green','yellow', 'red'),
                              space = "Lab")( 80 )
raster::plot(ArbC5, col=palet.col)
show_col(colorblind_pal()(8))
jet.colors <-
  colorRampPalette(c("#D55E00","#56B4E9", "#009E73"))( 80 )

blue.palette <- colorRampPalette(c('deepskyblue','green','yellow', 'red'))(100)
raster::plot(ArbC5, col=jet.colors,main= "Arboreal Niche Model")

# load the maxent predictions
#Arb5 <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapA5.grd")
#Terr5 <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapT5.grd")

ArbC5 <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapAC5.grd")
TerrC5 <- raster("./Analysis_Scripts/Chapter3/SDM/Prediction/Final_mapTC5.grd")

# load the polygons
ArbPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/ArbPolyAll/chull.shp")
TerrPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/TerrPolyAll/chull.shp")








library(ggplot2)
library(stringr)

Suitability <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
ArbRangeArbSuit <- c(24.04635, 43.55858, 56.57343, 64.73496, 71.71916, 77.72204, 84.69559, 92.84719, 98.24202)
TerrRangeTerrSuit <- c(0.459594, 10.23582, 54.89077, 75.87608, 83.84304, 88.68449, 92.04374, 96.23867, 98.87006)
ArbRangeTerrSuit <- c(2.129467, 8.549135, 16.82058, 28.27418, 37.25371, 51.99306, 67.1153,81.4081,91.50068)
TerrRangeArbSuit <- c(3.241491, 6.314055, 8.549385, 10.138, 11.23028, 12.20456, 13.20766,15.19335,25.81004)

DataFrame <- cbind(rep(Suitability,4), 
                   rep(c("Arboreal Species Distribution, Suitable Arboreal Area", "Terrestrial Species Distribution, Suitable Terrestrial Area", "Arboreal Species Distribution, Suitable Terrestrial Area","Terrestrial Species Distribution, Suitable Arboreal Area"), each = 9), 
                   c(ArbRangeArbSuit, TerrRangeTerrSuit, ArbRangeTerrSuit, TerrRangeArbSuit),
                   rep(c("Baseline", "Reciprocal"), each = 18))
DataFrame <- as.data.frame(DataFrame)
colnames(DataFrame) <- c("Suitability", "Comparison", "ReciprocalSuitability", "Comparison2")
DataFrame$ReciprocalSuitability <- as.character(DataFrame$ReciprocalSuitability)
DataFrame$ReciprocalSuitability <- as.numeric(DataFrame$ReciprocalSuitability)
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.2")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.3")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.4")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.5")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.6")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.7")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.8")
DataFrame$Suitability <- relevel(DataFrame$Suitability, "0.9")
DataFrame$Comparison <- relevel(DataFrame$Comparison, "Arboreal Species Distribution, Suitable Terrestrial Area")
DataFrame$Comparison <- relevel(DataFrame$Comparison, "Arboreal Species Distribution, Suitable Arboreal Area")
DataFrame$Comparison <- relevel(DataFrame$Comparison, "Terrestrial Species Distribution, Suitable Terrestrial Area")
override.shape <- c(19, 17, 17, 19)

jpeg("./Analysis_Scripts/Chapter3/Docs/Figures/RecipSuit9.jpeg", width = 550, height = 400, quality = 100)
ggplot(DataFrame, aes(y = ReciprocalSuitability, x = Suitability, color = str_wrap(Comparison, 34), shape = Comparison2)) + 
    geom_point(size = 4) +
    geom_line(data = DataFrame, aes(y = ReciprocalSuitability, group = Comparison)) + 
    scale_color_manual(values=c("mediumseagreen", "lightgreen", "orange4","peru"))  + 
    xlab("Suitability Cutoff") +
    ylab("Reciprocal Suitability Score") +
    theme(legend.title = element_blank(), legend.position = c(.19,.85), legend.text = element_text(size = 10),
          legend.background = element_rect("transparent"), legend.key.size = unit(.8, "cm"),
          panel.background = element_rect("transparent", "transparent", 1, 1, "black")) + 
  guides(colour = guide_legend(override.aes = list(shape = override.shape))) + 
  scale_shape(guide = FALSE)
dev.off()

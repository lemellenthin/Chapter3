#-------------------------------------------------------#
#                BioGeography Plot                      #
#-------------------------------------------------------# 

library(ape); library(phytools); library(geiger); library(readxl); library(stringr); library(phytools) ; library(plyr)
library(ggplot2); library(RColorBrewer); library(devtools); library(btw); library(pbapply)
library(BAMMtools); library(plyr); library(geomorph); library(parallel); library(phangorn); library(corHMM)
library(tidyr); library(paleotree)

# Making colors to use for all color schemes
    #  http://colorbrewer2.org/#type=sequential&scheme=GnBu&n=8
    Cols <- c("Arboreal" = "#7fbf7b", "Saxicolous" = "#fee090", "Terrestrial" = "#af8dc3", "Aquatic" = "#91bfdb", "Fossorial" = "#762a83")
   
    
# Read in Data ####
Habitats <- read.csv("Data/Pruned/HabitatsPruned.csv")
Habitats <- Habitats[,c("Species","Class3", "Class4", "Class5")]
Geography <- read_excel("Data/Raw/Species Classified.xlsx", sheet = "Geography")

Geo <- separate(Geography, col = "Geography", into = c("Geo1", "Geo2", "Geo3", "Geo4", "Geo5", 
                                                     "Geo6", "Geo7", "Geo8", "Geo9", "Geo10",
                                                     "Geo11", "Geo12", "Geo13", "Geo14", "Geo15",
                                                     "Geo16", "Geo17", "Geo18", "Geo19", "Geo20", 
                                                     "Geo21", "Geo22", "Geo23", "Geo24", "Geo25",
                                                     "Geo26", "Geo27", "Geo28", "Geo29", "Geo30",
                                                     "Geo31", "Geo32", "Geo33"), 
                sep = ", ", remove = F, extra = "merge")
            
Geo$Region1 <- rep(NA, length(Geo$Species))
Geo$Region2 <- rep(NA, length(Geo$Species))
Geo$Region3 <- rep(NA, length(Geo$Species))
Geo$Region4 <- rep(NA, length(Geo$Species))
Geo$Region5 <- rep(NA, length(Geo$Species))
                    
Rows1 <- unique(c(grep("Alabama", Geo$Geography),grep("Florida", Geo$Geography),
           grep("Mississippi", Geo$Geography), grep("Georgia", Geo$Geography),
           grep("Louisiana", Geo$Geography)))
   Geo$Region1[Rows1] <- "Region1"
Rows2 <- unique(c(grep("Georgia", Geo$Geography), grep("North Carolina", Geo$Geography),
           grep("South Carolina", Geo$Geography), grep("Virginia", Geo$Geography)))
   Geo$Region2[Rows2] <- "Region2"
Rows3 <- unique(c(grep("Tennessee", Geo$Geography), grep("Connecticut", Geo$Geography),
         grep("West Virginia", Geo$Geography), grep("Illinois", Geo$Geography),
         grep("Indiana", Geo$Geography), grep("Maryland", Geo$Geography), 
         grep("New Jersey", Geo$Geography), grep("Pennsylvania", Geo$Geography),
         grep("Wisconsin", Geo$Geography), grep("Kansas", Geo$Geography), 
         grep("Missouri", Geo$Geography), grep("Minnesota", Geo$Geography), 
         grep("Michigan", Geo$Geography), grep("Delaware", Geo$Geography),
         grep("New York", Geo$Geography), grep("Kentucky", Geo$Geography), 
         grep("Ohio", Geo$Geography), grep("Massachusetts", Geo$Geography), 
         grep("Vermont", Geo$Geography), grep("New Hampshire", Geo$Geography)))
   Geo$Region3[Rows3] <- "Region3"
Rows4 <- unique(c(grep("Texas", Geo$Geography), grep("Arkansas", Geo$Geography),
           grep("Oklahoma", Geo$Geography), grep("Michigan", Geo$Geography)))
   Geo$Region4[Rows4] <- "Region4"
Rows5 <- unique(c(grep("Panama", Geo$Geography), grep("Nicaragua", Geo$Geography),
           grep("Costa Rica", Geo$Geography), grep("Guatemala", Geo$Geography), 
           grep("Honduras", Geo$Geography), grep("Belize", Geo$Geography), grep("El Salvador", Geo$Geography)))
   Geo$Region5[Rows5] <- "Region5"

  
States <- c("Alabama","Florida","Mississippi" ,"Georgia" ,"Louisiana" ,"North Carolina",
            "South Carolina", "Virginia","Tennessee" , "Connecticut", "West Virginia",
            "Illinois" ,"Indiana" , "Maryland" ,"New Jersey" ,"Pennsylvania" , "Minnesota",
            "Wisconsin" ,"Michigan", "Delaware", "New York" , "Texas" , "Arkansas",
             "Oklahoma" ,"Michigan")

colnames(Geo)
dim(Geo)
Stuff <- na.omit(unlist(Geo[,c(3:35, 38:41)]))
Exclude <- which(Stuff%in%States == "TRUE")
Summary <- table(Stuff[-Exclude]) 
Summary

GeoSummary <- Geo[,c(-2, -36, -37)]
GeoSummary$CenteredAt <- rep(NA, length(GeoSummary$Species))
View(GeoSummary[,c(1,35:38,2:7)])

# Collapse Phylogeny into Clades ####

GeoSummary$Clades <- rep(NA, length(GeoSummary$Species))
GeoSummary$Clades[grep("Aquiloeurycea", Geo$Species)] <- "Aquiloeurycea genus"
GeoSummary$CenteredAt[grep("Aquiloeurycea", Geo$Species)] <- "Mexico"

GeoSummary$Clades[grep("Batrachoseps", Geo$Species)] <- "Batrachoseps genus"
GeoSummary$CenteredAt[grep("Batchrachoseps", Geo$Species)] <- "California"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa engelhardti" | Geo$Species == "Bolitoglossa rostrata" | 
                          Geo$Species == "Bolitoglossa helmrichi"| Geo$Species == "Bolitoglossa cuchumatana" | 
                          Geo$Species == "Bolitoglossa lincolni" | Geo$Species == "Bolitoglossa meliana" |
                          Geo$Species == "Bolitoglossa franklini" | Geo$Species == "Bolitoglossa flavimembris" |
                          Geo$Species == "Bolitoglossa morio" | Geo$Species == "Bolitoglossa kaqchikelorum" |
                          Geo$Species == "Bolitoglossa suchitanensis" | Geo$Species == "Bolitoglossa pacaya" | 
                          Geo$Species == "Bolitoglossa eremia"| Geo$Species == "Bolitoglossa heiroreias" | 
                          Geo$Species == "Bolitoglossa synoria" | Geo$Species == "Bolitoglossa celaque" |
                          Geo$Species == "Bolitoglossa porrasorum" | Geo$Species == "Bolitoglossa longissima" |
                          Geo$Species == "Bolitoglossa decora" | Geo$Species == "Bolitoglossa cataguana" | 
                          Geo$Species == "Bolitoglossa dunni" | Geo$Species == "Bolitoglossa diaphora" | 
                          Geo$Species == "Bolitoglossa conanti"| Geo$Species == "Bolitoglossa carri" | 
                          Geo$Species == "Bolitoglossa oaxacensis" | Geo$Species == "Bolitoglossa macrinii" |
                          Geo$Species == "Bolitoglossa zapoteca" | Geo$Species == "Bolitoglossa riletii" |
                          Geo$Species == "Bolitoglossa hermosa" | Geo$Species == "Bolitoglossa sombra" | 
                          Geo$Species == "Bolitoglossa dofleini" | Geo$Species == "Bolitoglossa alvaradoi" | 
                          Geo$Species == "Bolitoglossa stuarti"| Geo$Species == "Bolitoglossa hartwegi" | 
                          Geo$Species == "Bolitoglossa nympha" | Geo$Species == "Bolitoglossa rufescens" |
                          Geo$Species == "Bolitoglossa occidentalis" | Geo$Species == "Bolitoglossa chinanteca" |
                          Geo$Species == "Bolitoglossa platydactyla" | Geo$Species == "Bolitoglossa flaviventris" | 
                          Geo$Species == "Bolitoglossa mombachoensis" | Geo$Species == "Bolitoglossa mulleri" | 
                          Geo$Species == "Bolitoglossa mexicana"| Geo$Species == "Bolitoglossa yucatana" | 
                          Geo$Species == "Bolitoglossa odonnelli" | Geo$Species == "Bolitoglossa alberchi")] <- "Bolitoglossa group 1"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 1")] <- "Mexico and CAm"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa splendida" | Geo$Species == "Bolitoglossa pesrubra" | 
                          Geo$Species == "Bolitoglossa gracilis"| Geo$Species == "Bolitoglossa tica" | 
                          Geo$Species == "Bolitoglossa subpalmata" | Geo$Species == "Bolitoglossa kamuk" |
                          Geo$Species == "Bolitoglossa gomezi" | Geo$Species == "Bolitoglossa bramei" | 
                          Geo$Species == "Bolitoglossa chucantiensis"| Geo$Species == "Bolitoglossa minutula" | 
                          Geo$Species == "Bolitoglossa marmorea" | Geo$Species == "Bolitoglossa epimela" |
                          Geo$Species == "Bolitoglossa cerroensis")] <- "Bolitoglossa group 2"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 2")] <- "Costa Rica and Panama"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa nigrescens" | Geo$Species == "Bolitoglossa schizodactyla" | 
                          Geo$Species == "Bolitoglossa robusta"| Geo$Species == "Bolitoglossa compacta" | 
                          Geo$Species == "Bolitoglossa colonnea" | Geo$Species == "Bolitoglossa robinsoni"| 
                          Geo$Species == "Bolitoglossa aureogularis")] <- "Bolitoglossa group 3"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 3")] <- "Costa Rica and Panama"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa cuna" | Geo$Species == "Bolitoglossa biseriata" )] <- "Bolitoglossa group 4"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 4")] <- "Panama and Colombia"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa peruviana" | Geo$Species == "Bolitoglossa palmata")] <- "Bolitoglossa group 5"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 5")] <- "Peru and Ecuador"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa orestes" | Geo$Species == "Bolitoglossa mucuyensis")] <- "Bolitoglossa group 6"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 6")] <- "Venezuela"

GeoSummary$Clades[which(Geo$Species == "Bolitoglossa medemi" | Geo$Species == "Bolitoglossa adspersa")] <- "Bolitoglossa group 7"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Bolitoglossa group 7")] <- "Colombia"

GeoSummary$Clades[grep("Chiropterotriton", Geo$Species)] <- "Chiropterotriton genus"
GeoSummary$CenteredAt[grep("Chiropterotriton", Geo$Species)] <- "Mexico"

GeoSummary$Clades[which(Geo$Species == "Cryptotriton sierraminensis" | Geo$Species == "Cryptotriton veraepacis" | Geo$Species == "Cryptotriton xucaneborum")] <- "Cryptotriton group 1"
GeoSummary$CenteredAt[which(Geo$Species == "Cryptotriton sierraminensis" | Geo$Species == "Cryptotriton veraepacis" | Geo$Species == "Cryptotriton xucaneborum")] <- "Guatemala"

GeoSummary$Clades[which(Geo$Species == "Cryptotriton nasalis" | Geo$Species == "Cryptotriton necopinus")] <- "Cryptotriton group 2"
GeoSummary$CenteredAt[which(Geo$Species == "Cryptotriton nasalis" | Geo$Species == "Cryptotriton necopinus" )] <- "Honduras"

GeoSummary$Clades[which(Geo$Species == "Dendrotriton chujorum" | Geo$Species == "Dendrotriton cuchumatanus" | Geo$Species == "Dendrotriton kekchiorum")] <- "Dendrotriton group"
GeoSummary$CenteredAt[which(Geo$Species == "Dendrotriton chujorum" | Geo$Species == "Dendrotriton cuchumatanus" | Geo$Species == "Dendrotriton kekchiorum")] <- "Guatemala"


GeoSummary$Clades[which(Geo$Species == "Eurycea waterlooensis" | Geo$Species == "Eurycea rathbuni" | 
                          Geo$Species == "Eurycea sosorum"| Geo$Species == "Eurycea tridentifera" | 
                          Geo$Species == "Eurycea pterophila" | Geo$Species == "Eurycea neotenes" |
                          Geo$Species == "Eurycea nana" | Geo$Species == "Eurycea troglodytes P" |
                          Geo$Species == "Eurycea troglodytes M" | Geo$Species == "Eurycea latitans"| 
                          Geo$Species == "Eurycea naufragia" | Geo$Species == "Eurycea tonkawae" | 
                          Geo$Species == "Eurycea chisholmensis")] <- "Eurycea group"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Eurycea group")] <- "Texas"

GeoSummary$Clades[which(Geo$Species == "Hydromantes imperialis" | Geo$Species == "Hydromantes supramontis" | 
                          Geo$Species == "Hydromantes flavus")] <- "Hydromantes group 1"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Hydromantes group 1")] <- "Sardegna"

GeoSummary$Clades[which(Geo$Species == "Hydromantes shastae" | Geo$Species == "Hydromantes platycephalus" | 
                          Geo$Species == "Hydromantes brunus")] <- "Hydromantes group 2"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Hydromantes group 2")] <- "California"

GeoSummary$Clades[grep("Nototriton", Geo$Species)] <- "Nototriton genus"
GeoSummary$CenteredAt[grep("Nototriton", Geo$Species)] <- "CAm"

GeoSummary$Clades[which(Geo$Species == "Oedipina taylori" | Geo$Species == "Oedipina uniformis" | 
                        Geo$Species == "Oedipina gracilis" | Geo$Species == "Oedipina poelzi"|
                        Geo$Species == "Oedipina grandis" | Geo$Species == "Oedipina pseudouniformis"|
                        Geo$Species == "Oedipina cyclocauda")] <- "Oedipina group 1"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Oedipina group 1")] <- "CAm"

GeoSummary$Clades[which(Geo$Species == "Oedipina tomasi" | Geo$Species == "Oedipina gephyra")] <- "Oedipina group 2"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Oedipina group 2")] <- "CAm"

GeoSummary$Clades[which(Geo$Species == "Plethodon larselli" | Geo$Species == "Plethodon vandykei" | 
                        Geo$Species == "Plethodon idahoensis")] <- "Plethodon group 1"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 1")] <- "Pac NW"

GeoSummary$Clades[which(Geo$Species == "Plethodon vehiculum" | Geo$Species == "Plethodon dunni" )] <- "Plethodon group 2"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 2")] <- "Pac NW"

GeoSummary$Clades[which(Geo$Species == "Plethodon stormi" | Geo$Species == "Plethodon elongatus" | 
                        Geo$Species == "Plethodon asupak")] <- "Plethodon group 3"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 3")] <- "California"

GeoSummary$Clades[which(Geo$Species == "Plethodon hubrichti" | Geo$Species == "Plethodon nettingi" | 
                        Geo$Species == "Plethodon richmondi" | Geo$Species == "Plethodon electromorphus" | 
                        Geo$Species == "Plethodon virginia" | Geo$Species == "Plethodon hoffmani" | 
                        Geo$Species == "Plethodon shenandoah" | Geo$Species == "Plethodon cinereus")] <- "Plethodon group 4"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 4")] <- "Region 2"

GeoSummary$Clades[which(Geo$Species == "Plethodon ouchitae" | Geo$Species == "Plethodon fourchensis" | 
                        Geo$Species == "Plethodon caddoensis")] <- "Plethodon group 5"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 5")] <- "Region 4"

GeoSummary$Clades[which(Geo$Species == "Plethodon montanus" | Geo$Species == "Plethodon metcalfi")] <- "Plethodon group 6"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 6")] <- "Region 2"

GeoSummary$Clades[which(Geo$Species == "Plethodon jordani" | Geo$Species == "Plethodon glutinosus" | 
                        Geo$Species == "Plethodon shermani" | Geo$Species == "Plethodon aureolus")] <- "Plethodon group 7"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 7")] <- "Region 2"

GeoSummary$Clades[which(Geo$Species == "Plethodon mississippi" | Geo$Species == "Plethodon kisatchie" | 
                        Geo$Species == "Plethodon savannah" | Geo$Species == "Plethodon ocmulgee" | 
                        Geo$Species == "Plethodon grobmani")] <- "Plethodon group 8"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 8")] <- "Region 1"

GeoSummary$Clades[which(Geo$Species == "Plethodon websteri" | Geo$Species == "Plethodon wehrlei" | 
                        Geo$Species == "Plethodon punctatus")] <- "Plethodon group 9"
GeoSummary$CenteredAt[which(GeoSummary$Clades == "Plethodon group 9")] <- "Region 2"

GeoSummary$Clades[grep("Pseudoeurycea", Geo$Species)] <- "Pseudoeurycea genus"
GeoSummary$CenteredAt[grep("Pseudoeurycea", Geo$Species)] <- "Mexico"

GeoSummary$Clades[grep("Thorius", Geo$Species)] <- "Thorius genus"
GeoSummary$CenteredAt[grep("Thorius", Geo$Species)] <- "Mexico"

GeoSummary$Clades[grep("Desmognathus", Geo$Species)] <- "Desmognathus genus"
GeoSummary$CenteredAt[grep("Desmognathus", Geo$Species)] <- "East Coast"
  
BB <- read.tree("Data/Pruned/BBPruned.tre")
BB$tip.label <- str_replace_all(BB$tip.label, "_", " ")  
anyNA(match(BB$tip.label, Habitats$Species)) # want false

row.names(GeoSummary) <- GeoSummary$Species
GeoBBAll <- treedata(BB, GeoSummary)

GeoBB <- as.data.frame(GeoBBAll$data)
Groups <- levels(as.factor(GeoBB$Clades))
BB1 <- GeoBBAll$phy

for (i in 1:length(Groups)) {
  CladeRough <- GeoBB$Species[which(GeoBB$Clades == Groups[[i]])]
  Clade <- as.character(CladeRough)
  BBNew <- drop.tip(BB1, tip = Clade[-1])
  BBNew$tip.label[BBNew$tip.label %in% Clade[1]] <- Groups[[i]]
  BB1 <- BBNew
}

# Checking it did what I think it did
length(GeoBBAll$phy$tip.label)-length(na.omit(as.factor(GeoBB$Clades)))+length(Groups)
length(BB1$tip.label)# should be the same

plot(BB1, type = "fan", cex = 0.5)


# Getting lat and long data
GeoBB$CenteredAt <- as.character(GeoBB$CenteredAt)
GeoBB$Geo1 <- as.character(GeoBB$Geo1)
GeoBB$CenteredAt[is.na(GeoBB$CenteredAt)] <- GeoBB$Geo1[is.na(GeoBB$CenteredAt)]
GeoBB$CenteredAt <- as.factor(GeoBB$CenteredAt)
levels(GeoBB$CenteredAt)
GeoBB$Lat <- rep(0, length(GeoBB$Species))
GeoBB$Long <- rep(0, length(GeoBB$Species))

GeoBB$Lat[which(GeoBB$CenteredAt == "Alabama")] <- "32.3182"
GeoBB$Long[which(GeoBB$CenteredAt == "Alabama")] <- "-86.9023"

GeoBB$Lat[which(GeoBB$CenteredAt == "Arkansas")] <- "35.2010"
GeoBB$Long[which(GeoBB$CenteredAt == "Arkansas")] <- "-91.8318"

GeoBB$Lat[which(GeoBB$CenteredAt == "Brazil")] <- "-14.2350"
GeoBB$Long[which(GeoBB$CenteredAt == "Brazil")] <- "-51.9253"

GeoBB$Lat[which(GeoBB$CenteredAt == "California")] <- "36.7783"
GeoBB$Long[which(GeoBB$CenteredAt == "California")] <- "-119.4179"

GeoBB$Lat[which(GeoBB$CenteredAt == "CAm")] <- "12.7690"
GeoBB$Long[which(GeoBB$CenteredAt == "CAm")] <- "-85.6024"

GeoBB$Lat[which(GeoBB$CenteredAt == "Colombia")] <- "4.5709"
GeoBB$Long[which(GeoBB$CenteredAt == "Colombia")] <- "-74.2973"

GeoBB$Lat[which(GeoBB$CenteredAt == "Costa Rica")] <- "9.7489"
GeoBB$Long[which(GeoBB$CenteredAt == "Costa Rica")] <- "-83.7534"

GeoBB$Lat[which(GeoBB$CenteredAt == "Costa Rica and Panama")] <- "8.97844" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Costa Rica and Panama")] <- "-82.961" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "East Coast")] <- "38.17432" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "East Coast")] <- "-79.0060" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Ecuador")] <- "-1.8312"
GeoBB$Long[which(GeoBB$CenteredAt == "Ecuador")] <- "-78.1834"

GeoBB$Lat[which(GeoBB$CenteredAt == "Florida")] <- "27.6648"
GeoBB$Long[which(GeoBB$CenteredAt == "Florida")] <- "-81.5158"

GeoBB$Lat[which(GeoBB$CenteredAt == "Georgia")] <- "32.1656"
GeoBB$Long[which(GeoBB$CenteredAt == "Georgia")] <- "-82.9001"

GeoBB$Lat[which(GeoBB$CenteredAt == "Guatemala")] <- "15.7835"
GeoBB$Long[which(GeoBB$CenteredAt == "Guatemala")] <- "-90.2308"

GeoBB$Lat[which(GeoBB$CenteredAt == "Honduras")] <- "15.2000"
GeoBB$Long[which(GeoBB$CenteredAt == "Honduras")] <- "-86.2419"

GeoBB$Lat[which(GeoBB$CenteredAt == "Italy")] <- "41.8719"
GeoBB$Long[which(GeoBB$CenteredAt == "Italy")] <- "12.5674"

GeoBB$Lat[which(GeoBB$CenteredAt == "Kentucky")] <- "37.8393"
GeoBB$Long[which(GeoBB$CenteredAt == "Kentucky")] <- "-84.2700"

GeoBB$Lat[which(GeoBB$CenteredAt == "Louisiana")] <- "30.9843"
GeoBB$Long[which(GeoBB$CenteredAt == "Louisiana")] <- "-91.9623"

GeoBB$Lat[which(GeoBB$CenteredAt == "Mexico")] <- "23.6345"
GeoBB$Long[which(GeoBB$CenteredAt == "Mexico")] <- "-102.5528"

GeoBB$Lat[which(GeoBB$CenteredAt == "Mexico and CAm")] <- "17.021105" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Mexico and CAm")] <- "-89.9923" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Mississippi")] <- "32.3547"
GeoBB$Long[which(GeoBB$CenteredAt == "Mississippi")] <- "-89.3985"

GeoBB$Lat[which(GeoBB$CenteredAt == "Missouri")] <- "37.9643"
GeoBB$Long[which(GeoBB$CenteredAt == "Missouri")] <- "-91.8318"

GeoBB$Lat[which(GeoBB$CenteredAt == "New Mexico")] <- "34.5199"
GeoBB$Long[which(GeoBB$CenteredAt == "New Mexico")] <- "-105.8701"

GeoBB$Lat[which(GeoBB$CenteredAt == "Nicaragua")] <- "12.8654"
GeoBB$Long[which(GeoBB$CenteredAt == "Nicaragua")] <- "-85.2072"

GeoBB$Lat[which(GeoBB$CenteredAt == "North Carolina")] <- "35.7596"
GeoBB$Long[which(GeoBB$CenteredAt == "North Carolina")] <- "-79.0193"

GeoBB$Lat[which(GeoBB$CenteredAt == "Oklahoma")] <- "35.0078"
GeoBB$Long[which(GeoBB$CenteredAt == "Oklahoma")] <- "-97.0929"

GeoBB$Lat[which(GeoBB$CenteredAt == "Oregon")] <- "43.8041"
GeoBB$Long[which(GeoBB$CenteredAt == "Oregon")] <- "-120.5542"

GeoBB$Lat[which(GeoBB$CenteredAt == "Pac NW")] <- "46.533195" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Pac NW")] <- "-122.2482" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Panama")] <- "8.5380"
GeoBB$Long[which(GeoBB$CenteredAt == "Panama")] <- "-80.7821"

GeoBB$Lat[which(GeoBB$CenteredAt == "Panama and Colombia")] <- "7.316910" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Panama and Colombia")] <- "-77.0601" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Peru and Ecuador")] <- "-4.874028"  # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Peru and Ecuador")] <- "-77.851158" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Region 1")] <- "31.75709674" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Region 1")] <- "-87.9585" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Region 2")] <- "35.8453" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Region 2")] <- "-78.817955" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Region 4")] <- "33.1660" # Estimated
GeoBB$Long[which(GeoBB$CenteredAt == "Region 4")] <- "-94.4624" # Estimated

GeoBB$Lat[which(GeoBB$CenteredAt == "Sardegna")] <- "40.1209" 
GeoBB$Long[which(GeoBB$CenteredAt == "Sardegna")] <- "9.0129" 

GeoBB$Lat[which(GeoBB$CenteredAt == "South Carolina")] <- "33.8361" 
GeoBB$Long[which(GeoBB$CenteredAt == "South Carolina")] <- "-81.1637" 

GeoBB$Lat[which(GeoBB$CenteredAt == "South Korea")] <- "35.9078" 
GeoBB$Long[which(GeoBB$CenteredAt == "South Korea")] <- "127.7669" 

GeoBB$Lat[which(GeoBB$CenteredAt == "Tennessee")] <- "35.5175" 
GeoBB$Long[which(GeoBB$CenteredAt == "Tennessee")] <- "-86.5804" 

GeoBB$Lat[which(GeoBB$CenteredAt == "Texas")] <- "31.9686" 
GeoBB$Long[which(GeoBB$CenteredAt == "Texas")] <- "-99.9018" 

GeoBB$Lat[which(GeoBB$CenteredAt == "Venezuela")] <- "6.4238" 
GeoBB$Long[which(GeoBB$CenteredAt == "Venezuela")] <- "-66.5897" 

GeoBB$Lat[which(GeoBB$CenteredAt == "Virginia")] <- "37.4316" 
GeoBB$Long[which(GeoBB$CenteredAt == "Virginia")] <- "-78.6569" 

GeoBB$Lat[which(GeoBB$CenteredAt == "West Virginia")] <- "38.5976" 
GeoBB$Long[which(GeoBB$CenteredAt == "West Virginia")] <- "-80.4549" 

LatLong <- GeoBB[,c("Lat", "Long")]
row.names(LatLong)

#library(devtools)
#install_version("phytools", version = "0.4-45", quiet = F)
#library(phytools)

LatLongPruned <- GeoBB[is.na(GeoBB$Clades),c("Lat", "Long")]
for (i in 1:length(Groups)) {
LatLongPruned <- rbind(GeoBB[which(GeoBB$Clades == Groups[[i]])[1], c("Lat", "Long")], LatLongPruned)
}
row.names(LatLongPruned)[1:length(Groups)] <- Groups

TipLabels <- BB1$tip.label
LatLongPruned2 <- LatLongPruned[match(BB1$tip.label, rownames(LatLongPruned)),]
BB2 <- BB1
BB2$tip.label <- 1:length(TipLabels)
rownames(LatLongPruned2) <- 1:length(TipLabels)
LatLongNEW <- LatLongPruned2
LatLongNEW[,1] <- as.numeric(LatLongNEW[,1])
LatLongNEW[,2] <- as.numeric(LatLongNEW[,2])
obj <- phylo.to.map(BB2, LatLongNEW, plot = F)
plot(obj, type = "phylogram",asp=1.3)



plot.phylo.to.map.EB <- function (x, type = c("phylogram", "direct"), ...) 
{
    type <- type[1]
    if (class(x) == "phylo.to.map") {
        tree <- x$tree
        map <- x$map
        coords <- x$coords
    }
    else stop("x should be an object of class \"phylo.to.map\"")
    if (hasArg(xlim)) 
        xlim <- list(...)$xlim
    else xlim <- map$range[1:2]
    if (hasArg(ylim)) 
        ylim <- list(...)$ylim
    else ylim <- map$range[3:4]
    if (hasArg(fsize)) 
        fsize <- list(...)$fsize
    else fsize <- 1
    if (hasArg(split)) 
        split <- list(...)$split
    else split <- c(0.4, 0.6)
    if (hasArg(psize)) 
        psize <- list(...)$psize
    else psize <- 1
    if (hasArg(mar)) 
        mar <- list(...)$mar
    else mar <- rep(0, 4)
    if (hasArg(asp)) 
        asp <- list(...)$asp
    else asp <- 1
    if (hasArg(ftype)) 
        ftype <- list(...)$ftype
    else ftype <- "reg"
    ftype <- which(c("off", "reg", "b", "i", "bi") == ftype) - 
        1
    if (!ftype) 
        fsize = 0
    if (hasArg(from.tip)) 
        from.tip <- list(...)$from.tip
    else from.tip <- FALSE
    if (hasArg(colors)) 
        colors <- list(...)$colors
    else colors <- "green"
    if (length(colors) != 2) 
        colors <- rep(colors[1], 2)
    if (type == "phylogram") 
        ylim <- c(ylim[1], ylim[2] + split[1]/split[2] * (ylim[2] - 
            ylim[1]))
    par(mar = mar)
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = asp)
    map(map, add = TRUE, fill = TRUE, col = "gray95", mar = rep(0, 
        4))
    if (type == "phylogram") {
        dx <- abs(diff(xlim))
        rect(xlim[1] - 1.04 * dx, ylim[2] - split[1] * (ylim[2] - 
            ylim[1]), xlim[2] + 1.04 * dx, ylim[2], col = "white", 
            border = "white")
        sh <- (fsize * strwidth(tree$tip.label))/(par()$usr[2] - 
            par()$usr[1]) * (par()$usr[4] - par()$usr[3])
        tree$edge.length <- tree$edge.length/max(nodeHeights(tree)) * 
            (split[1] * (ylim[2] - ylim[1]) - max(sh))
        n <- length(tree$tip.label)
        cw <- reorder(tree, "cladewise")
        x <- vector(length = n + cw$Nnode)
        x[cw$edge[cw$edge[, 2] <= n, 2]] <- 0:(n - 1)/(n - 1) * 
            (xlim[2] - xlim[1]) + xlim[1]
        pw <- reorder(tree, "pruningwise")
        nn <- unique(pw$edge[, 1])
        for (i in 1:length(nn)) {
            xx <- x[pw$edge[which(pw$edge[, 1] == nn[i]), 2]]
            x[nn[i]] <- mean(range(xx))
        }
        Y <- ylim[2] - nodeHeights(cw)
        for (i in 1:nrow(Y)) lines(rep(x[cw$edge[i, 2]], 2), 
            Y[i, ], lwd = 2, lend = 2)
        for (i in 1:tree$Nnode + n) lines(range(x[cw$edge[which(cw$edge[, 
            1] == i), 2]]), Y[which(cw$edge[, 1] == i), 1], lwd = 2, 
            lend = 2)
        coords <- coords[tree$tip.label, 2:1]
        points(coords, pch = 16, cex = psize, col = colors[2])
        for (i in 1:n) lines(c(x[i], coords[i, 1]), c(Y[which(cw$edge[, 
            2] == i), 2] - if (from.tip) 0 else sh[i], coords[i, 
            2]), col = colors[1], lty = "dashed")
        for (i in 1:n) text(x[i], Y[which(cw$edge[, 2] == i), 
            2], sub("_", " ", tree$tip.label[i]), pos = 4, offset = 0.1, 
            srt = -90, cex = fsize, font = ftype)
    }
    else if (type == "direct") {
        phylomorphospace(tree, coords[, 2:1], add = TRUE, label = "horizontal", 
            node.size = c(0, psize), lwd = 1, control = list(col.node = setNames(rep(colors[2], 
                max(tree$edge)), 1:max(tree$edge)), col.edge = setNames(rep(colors[1], 
                nrow(tree$edge)), tree$edge[, 2])))
    }
}


plot.phylo.to.map.EB( obj)

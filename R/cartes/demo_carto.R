library(rgdal)
library(leaflet)
library(tidyverse)
library(geosphere)
library(sp)
#library(scatterpie)
library(maps)
library(leaflet.providers) 
library(leaflet.minicharts)
library(readxl)
library(raster)
library(smoothr)
library(rmapshaper)

#library(esquisse)
#library(gtsummary)
#library(GGally)
load("data/demo/basecom.RData")
load("data/demo/tab_demo.RData")
chemin <- "C:/Users/plebre/Documents/carto/IGN/ADMIN-EXPRESS-COG-CARTO_3-1__SHP__FRA_WM_2022-04-15/ADMIN-EXPRESS-COG-CARTO/1_DONNEES_LIVRAISON_2022-04-15/ADECOGC_3-1_SHP_WGS84G_FRA/"
epciwgs84 <- readOGR(paste0(chemin,"EPCI.shp"))
regwgs84 <- readOGR(paste0(chemin,"REGION.shp"))
depwgs84 <- readOGR(paste0(chemin,"DEPARTEMENT.shp"))
comm84 <- readOGR(paste0(chemin,"COMMUNE.shp"))

densitewgs <- readOGR("I:/SUPPORT/05_CARTO/Fonds de cartes/densité et AU/grille_densite_7_niveaux.shp")
densiteBFC <- readOGR("I:/SUPPORT/05_CARTO/Fonds de cartes/densité et AU/grille_densite_7_niveauxBFC.shp")
bvwgs84 <-  readOGR("I:/SUPPORT/05_CARTO/Fonds de cartes/bv/bv2012_2022.shp")


regwgs <- ms_simplify(regwgs84, keep=0.05,keep_shapes = T)
rm(regwgs84)
#regwgs <- smooth(regwgs,method = "ksmooth",8)
depwgs <- ms_simplify(depwgs84, keep=0.05,keep_shapes = T)
rm(depwgs84)

epciwgs <-ms_simplify(epciwgs84, keep=0.05,keep_shapes = T)
rm(epciwgs84)
bvwgs <- ms_simplify(bvwgs84, keep=0.05,keep_shapes = T)
rm(bvwgs84)

comm27 <- subset(comm84,INSEE_REG == "27")
rm(comm84)
com27wgs <- ms_simplify(comm27, keep=0.05,keep_shapes = T)
rm(comm27)

EPCI <- EPCI %>%  rename(CODE_EPCI=EPCI) %>% filter(CODE_EPCI %in% epci27_tab$EPCI) 
epciwgs@data <- epciwgs@data %>% rename(CODE_EPCI=CODE_SIREN)
epcicarto <- merge(epciwgs,EPCI,by= "CODE_EPCI",all.x=F, all.y=T)

BV <- BV %>% filter(BV2012 %in% bv27_tab$BV2012) 
bvwgs@data <- bvwgs@data %>% rename(BV2012=bv2012)
bvcarto <- merge(bvwgs,BV,by= "BV2012",all.x=F, all.y=T)

regwgs <- merge(regwgs,region %>% rename(INSEE_REG=REG),by="INSEE_REG")
depwgs <- merge(depwgs,departement %>% rename(INSEE_DEP=DEP),by="INSEE_DEP")


#QPV et ZRR
QPV <- readOGR("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/qp-politiquedelaville-shp/QP_METROPOLEOUTREMER_WGS84_EPSG4326.shp")
QPVpop <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/DEMO_2020_V1.xlsx",sheet = 1,skip = 5)
QPVpop <- QPVpop %>% filter(substr(CODGEO,3,5) %in% c("021","025","039","058","070","071","089","090",
                                                      "010","077","052","068","088","001","069","003",
                                                      "042","018","043")) %>%
                    rename(CODE_QP=CODGEO)

QPV27 <- merge(QPV,QPVpop,by="CODE_QP",all.x=F,all.y=T)
plot(QPV27)

ZRR <- readOGR("I:/SUPPORT/05_CARTO/Fonds de cartes/ZRR/ZRR2018.shp")
ZRR <- spTransform(ZRR,CRS("+proj=longlat +datum=WGS84"))  
ZRR <- ms_simplify(ZRR, keep=0.05,keep_shapes = T)

regwgs <- regwgs[order(regwgs$pop,decreasing = T),]
depwgs <- depwgs[order(depwgs$pop,decreasing = T),]
epcicarto <- epcicarto[order(epcicarto$pop,decreasing = T),]
bvcarto <- bvcarto[order(bvcarto$pop,decreasing = T),]

ZRR <- subset(ZRR,ZRR_2018 != "Non classée")
regwgs <- subset(regwgs,INSEE_REG > "10")
depwgs <- subset(depwgs,INSEE_REG > "10")

reg27carto <- subset(regwgs ,INSEE_REG=="27")
dep27carto <- subset(depwgs, INSEE_REG=="27")



leaflet(regwgs_s) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 5.1, lat = 47.27, zoom = 8) %>%
  addPolygons(weight=2,opacity = 1,color = "#2F4F4F", fill=F )


save(regwgs,depwgs,bvwgs,epciwgs,densiteBFC,densitewgs,reg27carto,dep27carto,epcicarto,bvcarto,
     com27wgs,QPV27,ZRR,file = "data/demo/cartes.RData")
save(regwgs,depwgs,bvwgs,epciwgs,densiteBFC,densitewgs,reg27carto,dep27carto,epcicarto,bvcarto,
     com27wgs,QPV27,ZRR,file = "I:/SUPPORT/05_CARTO/Fonds de cartes/cartes.RData")

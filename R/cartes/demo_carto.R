#library(rgdal)
library(leaflet)
library(tidyverse)
library(geosphere)
library(sp)
library(sf)
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

#https://geoservices.ign.fr/adminexpress#telechargementCogCarto
chemin <- "data/carto/ADMIN-EXPRESS-COG-CARTO_3-2__SHP_WGS84G_FRA_2024-02-22/" 
epciwgs84 <- st_read(paste0(chemin,"EPCI.shp"))
regwgs84 <- st_read(paste0(chemin,"REGION.shp"))
depwgs84 <- st_read(paste0(chemin,"DEPARTEMENT.shp"))
comm84 <- st_read(paste0(chemin,"COMMUNE.shp"))

#https://www.insee.fr/fr/information/6439600
densite <- read_excel("data/carto/grille_densite_7_niveaux_2023.xlsx",sheet = 1,skip = 4) 

jointure <- merge(comm84 |> 
                    filter(INSEE_REG>'10'),densite,by.x="INSEE_COM",by.y="CODGEO",all.x=F)

densitegeo <- jointure |> 
  group_by(DENS,LIBDENS) |> 
  summarise(geometry=st_union(geometry))

plot(densitegeo[1])

#densite <- aggregate(jointure,by=list(jointure$DENS,jointure$LIBDENS),FUN=mean,simplify=TRUE)
#densite <- ms_simplify(densite, keep=0.05,keep_shapes = T)

densitewgs <- st_simplify(densitegeo,dTolerance = 1e3)

plot(densitewgs[2])

#export du fond de carte
st_write(densitewgs,
         "data/carto/densite.shp",
         layer="densite",
         driver="ESRI Shapefile")

jointure <- merge(comm84 |> 
                    filter(INSEE_REG=="27"),
                  densite,by.x="INSEE_COM",by.y="CODGEO",all.x=F)
densitebfc <- jointure |> 
  group_by(DENS,LIBDENS) |> 
  summarise(geometry=st_union(geometry))

#densiteBFC <- readOGR("I:/SUPPORT/05_CARTO/Fonds de cartes/densité et AU/grille_densite_7_niveauxBFC.shp")

densitebfc <- st_simplify(densitebfc,dTolerance = 100)
#densitebfc <- smooth(densitebfc,method = "ksmooth",8)

plot(densitebfc[1])
leaflet(densitebfc) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 5.1, lat = 47.27, zoom = 8) %>%
  addPolygons(weight=2,opacity = 1,color = "#2F4F4F", fill=F )

#export du fond de carte
st_write(densitebfc,
         "data/carto/densitebfc.shp",
         layer="densitebfc",
         driver="ESRI Shapefile")

#regwgs <- ms_simplify(regwgs84, keep=0.05,keep_shapes = T)
regwgs <- st_simplify(regwgs84[-1] |> 
                        filter(INSEE_REG>'10'),
                      dTolerance = 1e3)
#rm(regwgs84)
regwgs <- smooth(regwgs,method = "ksmooth",4)
plot(regwgs[3])

#depwgs <- ms_simplify(depwgs84, keep=0.05,keep_shapes = T)
depwgs <- st_simplify(depwgs84[-1] |> 
                        filter(INSEE_REG>'10'),
                      dTolerance = 1e3)
depwgs <- smooth(depwgs,method = "ksmooth",2)
plot(depwgs[3])
#rm(depwgs84)

#epciwgs <-ms_simplify(epciwgs84, keep=0.05,keep_shapes = T)
epciwgs <- st_simplify(epciwgs84[-1] |> 
                        filter(CODE_SIREN %in% basecom$EPCI ),
                      dTolerance = 1e3)
plot(epciwgs[2])
#rm(epciwgs84)

BV2022 <- read_excel("data/insee/BV2022_au_01-01-2024.xlsx",sheet = 2,skip = 5)  

jointure <- merge(comm84 |> 
                    filter(INSEE_REG>"10"),
                  BV2022,by.x="INSEE_COM",by.y="CODGEO",all.x=F)
BVwgs <- jointure |> 
  group_by(BV2022,LIBBV2022) |> 
  summarise(geometry=st_union(geometry))

bvwgs <- st_simplify(BVwgs,dTolerance = 1e3)
plot(bvwgs[1])

comm27 <- comm84 |> filter(INSEE_REG == "27")
rm(comm84)
com27wgs <- st_simplify(comm27, dTolerance = 100)
rm(comm27)
plot(com27wgs[1])

EPCI <- EPCI %>%  filter(EPCI %in% epci27_tab$EPCI) 
#epciwgs@data <- epciwgs@data %>% rename(EPCI=CODE_SIREN)
epcicarto <- merge(epciwgs |> 
                     filter(CODE_SIREN %in% epci27_tab$EPCI),
                   EPCI,
                   by.x= "CODE_SIREN",
                   by.y= "EPCI",
                   all.x=F, all.y=T)
plot(epcicarto[1])


BV <- BV %>% filter(BV2022 %in% bv27_tab$BV2022) 

bvcarto <- merge(bvwgs |> 
                   filter(BV2022 %in% BV$BV2022),
                 BV,
                 by= "BV2022",
                 all.x=F, 
                 all.y=T)
plot(bvcarto[1])

regwgs <- regwgs %>% rename(REG=INSEE_REG)
regwgs <- merge(regwgs,region,by="REG")
depwgs <- depwgs %>% rename(DEP=INSEE_DEP)
depwgs <- merge(depwgs,departement,by="DEP")


#QPV et ZRR
QPV <- st_read("data/carto/QPV/Quartier_prioritaire_metropole_ANCT_20240426.shp") #https://www.data.gouv.fr/fr/datasets/quartiers-prioritaires-de-la-politique-de-la-ville-qpv/
qpvpassage <- read_excel("data/carto/QPV/liste-correspondance-qp2024-qp2015-hexagone.xlsx") #https://www.data.gouv.fr/fr/datasets/quartiers-prioritaires-de-la-politique-de-la-ville-qpv/#/resources
QPVpop <- read_excel("data/insee/pop_qpv_2018.xlsx",sheet = 1,skip = 7) #https://www.insee.fr/fr/statistiques/5428762?sommaire=2500477
QPVpop <- QPVpop %>% filter(codeReg=='27')

QPV27 <- merge(QPV,QPVpop |> 
                 left_join(qpvpassage |> 
                             dplyr::select(QP2024,QPV2015=`QP 2015 correspondant`),
                           by=join_by(codeQPV==QPV2015)
                           ),
               by.x="Code_QP",
               by.y="QP2024",
               all.x=F,all.y=T)
plot(QPV27[1])

ZRR <- read_excel("data/data.gouv/diffusion-zonages-zrr-cog2021.xls",sheet = 1,skip = 5) #https://www.data.gouv.fr/fr/datasets/zones-de-revitalisation-rurale-zrr/

passage <- read_excel("data/insee/table_passage_annuelle_2024.xlsx",sheet = 1,skip=5)

#passage en géométrie 2024
ZRR2024 <- ZRR |> 
  filter(substr(CODGEO,1,2)<97) |> 
  left_join(passage |> dplyr::select(CODGEO_2021,CODGEO_2024),
                              by=join_by(CODGEO==CODGEO_2021)) |> 
  group_by(CODGEO_2024,ZRR_SIMP) |> summarise(first(CODGEO_2024))

jointure <- merge(comm84 |> 
                    filter(INSEE_REG>"10"),
                  ZRR2024,by.x="INSEE_COM",by.y="CODGEO_2024",all.x=F)

ZRRwgs <- jointure |> 
  group_by(ZRR_SIMP) |> 
  summarise(geometry=st_union(geometry))

ZRRwgs <- st_simplify(ZRRwgs,dTolerance = 1e3)
#ZRRwgs <- smooth(ZRRwgs,method = "ksmooth",4)

plot(ZRRwgs[1])
#ZRR <- spTransform(ZRR,CRS("+proj=longlat +datum=WGS84"))  

regwgs <- regwgs[order(regwgs$pop,decreasing = T),]
depwgs <- depwgs[order(depwgs$pop,decreasing = T),]
epcicarto <- epcicarto[order(epcicarto$pop,decreasing = T),]
bvcarto <- bvcarto[order(bvcarto$pop,decreasing = T),]

ZRR <- subset(ZRRwgs,ZRR_SIMP != "NC - Commune non classée")
#regwgs <- subset(regwgs,REG > "10")
#depwgs <- subset(depwgs,INSEE_REG > "10")

reg27carto <- subset(regwgs ,REG=="27")
dep27carto <- subset(depwgs, REG=="27")


leaflet(ZRR) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 5.1, lat = 47.27, zoom = 8) %>%
  addPolygons(weight=2,opacity = 1,color = "#2F4F4F", fill=F )


save(regwgs,depwgs,bvwgs,epciwgs,densitebfc,densitewgs,reg27carto,dep27carto,epcicarto,bvcarto,
     com27wgs,QPV27,ZRR,file = "data/demo/cartes.RData")
#save(regwgs,depwgs,bvwgs84,epciwgs,densiteBFC,densitewgs,reg27carto,dep27carto,epcicarto,bvcarto,
#     com27wgs,QPV27,ZRR,file = "I:/SUPPORT/05_CARTO/Fonds de cartes/cartes.RData")


library(leaflet)
library(fontawesome)
library(tidyverse)
library(osrm)
library(osrmr)

limitrophe <- c('21','25','39','58','70','71','89','90','10','52','88','68','01','69','42','03','18','45','77') 
#base des bassins carto----
bassins <- equip %>% filter (EquipementTypeCode %in% c('101','102','103') & DEP %in% limitrophe  ) %>% 
  filter (EquUtilScolaire==1 | (EquNatSurfaceBassin >20 & EquNatProfMax > 1) ) %>%
  select(id=EquipementId,lon=EquGPSX,lat=EquGPSY,EquipementTypeCode,EquipementTypeLib,
         InsNom,EquNom,
         ComInsee,LIBGEO.x,EquAnneeService,NatureLibelle,NatureSolLib, 
         EquOuvertSaison, EquNatSurfaceBassin,EquNatProfMax,
         InsNumeroInstall,EquEclairage,EquERPCategorie,EquDateMaj,EquDateCreation  ) %>%
  filter(!is.na(lon))

bassins_couv <- equip %>% filter (EquipementTypeCode %in% c('101','102','103') & DEP %in% limitrophe  ) %>% 
  filter (EquUtilScolaire==1 | (EquNatSurfaceBassin >20 & EquNatProfMax > 1) ) %>%
  filter (NatureLibelle %in% c("Intérieur","Découvrable")) %>%  #bassins couverts
  select(id=EquipementId,lon=EquGPSX,lat=EquGPSY,EquipementTypeCode,EquipementTypeLib,
         InsNom,EquNom,
         ComInsee,LIBGEO.x,EquAnneeService,NatureLibelle,NatureSolLib, 
         EquOuvertSaison, EquNatSurfaceBassin,EquNatProfMax,
         InsNumeroInstall,EquEclairage,EquERPCategorie,EquDateMaj,EquDateCreation  ) %>%
  filter(!is.na(lon))

bassins_annuels <- equip %>% filter (EquipementTypeCode %in% c('101','102','103') & DEP %in% limitrophe  ) %>% 
  filter (EquUtilScolaire==1 | (EquNatSurfaceBassin >20 & EquNatProfMax > 1) ) %>%
  filter (EquOuvertSaison==0 ) %>%  #ouverts à l'année
  select(id=EquipementId,lon=EquGPSX,lat=EquGPSY,EquipementTypeCode,EquipementTypeLib,
         InsNom,EquNom,
         ComInsee,LIBGEO.x,EquAnneeService,NatureLibelle,NatureSolLib, 
         EquOuvertSaison, EquNatSurfaceBassin,EquNatProfMax,
         InsNumeroInstall,EquEclairage,EquERPCategorie,EquDateMaj,EquDateCreation  ) %>%
  filter(!is.na(lon))

bassins_annuels_couv <- equip %>% filter (EquipementTypeCode %in% c('101','102','103') & DEP %in% limitrophe  ) %>% 
  filter (EquUtilScolaire==1 | (EquNatSurfaceBassin >20 & EquNatProfMax > 1) ) %>%
  filter (NatureLibelle %in% c("Intérieur","Découvrable")) %>%  #bassins couverts
  filter (EquOuvertSaison == 0 ) %>%
  select(id=EquipementId,lon=EquGPSX,lat=EquGPSY,EquipementTypeCode,EquipementTypeLib,
         InsNom,EquNom,
         ComInsee,LIBGEO.x,EquAnneeService,NatureLibelle,NatureSolLib, 
         EquOuvertSaison, EquNatSurfaceBassin,EquNatProfMax,
         InsNumeroInstall,EquEclairage,EquERPCategorie,EquDateMaj,EquDateCreation  ) %>%
  filter(!is.na(lon))

rm(limitrophe)

colbassin <- function(x) {
  sapply(x$EquipementTypeCode, function(EquipementTypeCode) {
    if(EquipementTypeCode == '101') {"#FFFFFF"}
    else if(EquipementTypeCode== '102') {"#1E90FF"}
    else {"#7B68EE"}
  } ) }

icons <- awesomeIcons(
  text = fa("swimmer"),  
  iconColor = colbassin(bassins),
  library = 'fa',
  markerColor = "lightblue")

icons_couv <- awesomeIcons(
  text = fa("swimmer"),  
  iconColor = colbassin(bassins_couv),
  library = 'fa',
  markerColor = "lightblue")

icons_annuel <- awesomeIcons(
  text = fa("swimmer"),  
  iconColor = colbassin(bassins_annuels),
  library = 'fa',
  markerColor = "lightblue")

icons_couv_annuel <- awesomeIcons(
  text = fa("swimmer"),  
  iconColor = colbassin(bassins_annuels_couv),
  library = 'fa',
  markerColor = "lightblue")




#serveur OSRM----
##docker pull osrm/osrm-backend
##geofabrik.de
##docker run -t -v c:/docker:/data osrm/osrm-backend osrm-extract -p /opt/car.lua /data/france-latest.osm.pbf
##docker run -t -v c:/docker:/data osrm/osrm-backend osrm-contract /data/france-latest.osm.pbf
#docker run -t -i -p 5000:5000 -v c:/docker:/data osrm/osrm-backend osrm-routed /data/france-latest.osrm
options(osrm.server = "http://localhost:5000/") 


#source("P:/projets R/RES/piscines/base_bassin.R")
#source("P:/projets R/RES/piscines/ecoles.R")


isochrone <- map2(bassins$lon, bassins$lat, 
                  ~ osrmIsochrone(loc = c(.x, .y),res=70,
                                  breaks = seq(0, 60, 10)) ) %>%
  do.call(what = rbind)

isochrone@data$drive_times <- factor(paste(isochrone@data$min , "à" ,isochrone@data$max, "mn"))

iso_sf <- st_as_sf(isochrone)

iso_union <- iso_sf %>%
  group_by(id,drive_times) %>%
  summarise()  
# slice(c(1,3,5,7,9,11))  %>%
# arrange(desc(drive_times)) 

iso_diff6 <- st_difference(iso_union[iso_union$id=="6",],st_union(iso_union[iso_union$id<"6",]))
iso_diff5 <- st_difference(iso_union[iso_union$id=="5",],st_union(iso_union[iso_union$id<"5",]))
iso_diff4 <- st_difference(iso_union[iso_union$id=="4",],st_union(iso_union[iso_union$id<"4",]))
iso_diff3 <- st_difference(iso_union[iso_union$id=="3",],st_union(iso_union[iso_union$id<"3",]))
iso_diff2 <- st_difference(iso_union[iso_union$id=="2",],st_union(iso_union[iso_union$id<"2",]))

iso_diff <- rbind(iso_union[iso_union$id=="1",],
                  iso_diff2,
                  iso_diff3,
                  iso_diff4,
                  iso_diff5,
                  iso_diff6)


isochrone_couv <- map2(bassins_couv$lon, bassins_couv$lat, 
                       ~ osrmIsochrone(loc = c(.x, .y),res=70,
                                       breaks = seq(0, 60, 10)) ) %>%
  do.call(what = rbind)

isochrone_couv@data$drive_times <- factor(paste(isochrone_couv@data$min , "à" ,isochrone_couv@data$max, "mn"))

iso_sf <- st_as_sf(isochrone_couv)

iso_union <- iso_sf %>%
  group_by(id,drive_times) %>%
  summarise()  
# slice(c(1,3,5,7,9,11))  %>%
# arrange(desc(drive_times)) 

iso_diff6 <- st_difference(iso_union[iso_union$id=="6",],st_union(iso_union[iso_union$id<"6",]))
iso_diff5 <- st_difference(iso_union[iso_union$id=="5",],st_union(iso_union[iso_union$id<"5",]))
iso_diff4 <- st_difference(iso_union[iso_union$id=="4",],st_union(iso_union[iso_union$id<"4",]))
iso_diff3 <- st_difference(iso_union[iso_union$id=="3",],st_union(iso_union[iso_union$id<"3",]))
iso_diff2 <- st_difference(iso_union[iso_union$id=="2",],st_union(iso_union[iso_union$id<"2",]))

iso_diff_couv <- rbind(iso_union[iso_union$id=="1",],
                       iso_diff2,
                       iso_diff3,
                       iso_diff4,
                       iso_diff5,
                       iso_diff6)

isochrone_annuel <- map2(bassins_annuels$lon, bassins_annuels$lat, 
                         ~ osrmIsochrone(loc = c(.x, .y),res=70,
                                         breaks = seq(0, 60, 10)) ) %>%
  do.call(what = rbind)

isochrone_annuel@data$drive_times <- factor(paste(isochrone_annuel@data$min , "à" ,isochrone_annuel@data$max, "mn"))

iso_sf <- st_as_sf(isochrone_annuel)

iso_union <- iso_sf %>%
  group_by(id,drive_times) %>%
  summarise()  
# slice(c(1,3,5,7,9,11))  %>%
# arrange(desc(drive_times)) 

iso_diff6 <- st_difference(iso_union[iso_union$id=="6",],st_union(iso_union[iso_union$id<"6",]))
iso_diff5 <- st_difference(iso_union[iso_union$id=="5",],st_union(iso_union[iso_union$id<"5",]))
iso_diff4 <- st_difference(iso_union[iso_union$id=="4",],st_union(iso_union[iso_union$id<"4",]))
iso_diff3 <- st_difference(iso_union[iso_union$id=="3",],st_union(iso_union[iso_union$id<"3",]))
iso_diff2 <- st_difference(iso_union[iso_union$id=="2",],st_union(iso_union[iso_union$id<"2",]))

iso_diff_annuel <- rbind(iso_union[iso_union$id=="1",],
                         iso_diff2,
                         iso_diff3,
                         iso_diff4,
                         iso_diff5,
                         iso_diff6)

isochrone_couvert_annuel <- map2(bassins_annuels_couv$lon, bassins_annuels_couv$lat, 
                                 ~ osrmIsochrone(loc = c(.x, .y),res=70,
                                                 breaks = seq(0, 60, 10)) ) %>%
  do.call(what = rbind)

isochrone_couvert_annuel@data$drive_times <- factor(paste(isochrone_couvert_annuel@data$min , "à" ,isochrone_couvert_annuel@data$max, "mn"))

iso_sf <- st_as_sf(isochrone_couvert_annuel)

iso_union <- iso_sf %>%
  group_by(id,drive_times) %>%
  summarise()  
# slice(c(1,3,5,7,9,11))  %>%
# arrange(desc(drive_times)) 

iso_diff6 <- st_difference(iso_union[iso_union$id=="6",],st_union(iso_union[iso_union$id<"6",]))
iso_diff5 <- st_difference(iso_union[iso_union$id=="5",],st_union(iso_union[iso_union$id<"5",]))
iso_diff4 <- st_difference(iso_union[iso_union$id=="4",],st_union(iso_union[iso_union$id<"4",]))
iso_diff3 <- st_difference(iso_union[iso_union$id=="3",],st_union(iso_union[iso_union$id<"3",]))
iso_diff2 <- st_difference(iso_union[iso_union$id=="2",],st_union(iso_union[iso_union$id<"2",]))

iso_diff_couvert_annuel <- rbind(iso_union[iso_union$id=="1",],
                                 iso_diff2,
                                 iso_diff3,
                                 iso_diff4,
                                 iso_diff5,
                                 iso_diff6)
rm(iso_diff2);rm(iso_diff3);rm(iso_diff4);rm(iso_diff5);rm(iso_diff6)




save(bassins,bassins_couv,bassins_annuels,bassins_annuels_couv,colbassin,icons,icons_couv,icons_annuel,icons_couv_annuel,
     iso_diff,iso_diff_couv,iso_diff_annuel,iso_diff_couvert_annuel,
     file="C:/Users/plebre/Documents/projets R/DRAJES/data/sport/bassins.RData")

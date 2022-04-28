{library(rgdal)
  library(gamm4)
  library(leafpop)
  library(leaflet)
  library(leaflet.extras2)
  library(geosphere)
  library(sp)
  library(sf)
  library(plotly)
  library(maps)
  library(leaflet.providers) 
  library(leaflet.minicharts)
  library(readxl)
  library(raster)
  library(smoothr)
  library(rmapshaper)
  library(rpivotTable)
  library(foreign)
  library(osrm)
  library(ggmap)
  library(fontawesome)
  library(tidyverse)
}

equipement <- read.csv2("I:/SUPPORT/04_STATS/Sources/MEDES/RES/2020/2020_Equipements.csv",as.is = T,encoding = "UTF-8")
passage <-  read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/communes/table_passage_annuelle_2021.xlsx",sheet=1,skip=5)

load("C:/Users/plebre/Documents/projets R/DRAJES/data/demo/basecom.RData")

proprio <- c("Etablissement privé commercial","Privé non commercial")
equips <- equipement %>% filter(ComInsee<"96000") %>%
  dplyr::filter (!GestionTypeProprietairePrincLib %in% proprio) %>%
  dplyr::select(ComInsee,DepCode,EquipementTypeCode,EquipementTypeLib,EquipementCateg,EquipementFamille,
         GestionTypeProprietairePrincLib,GestionTypeProprietaireSecLib,
         EquAnneeService,EquPeriodeService,
         NatureLibelle,NatureSolLib, EquProximite,EquOuvertSaison, EquNatSurfaceBassin,EquNatProfMax,EquGPSX,EquGPSY,
         InsNumeroInstall,InsNom, EquipementId,EquNom ,EquEclairage,EquERPCategorie,EquDateMaj,EquDateCreation,
         54:68,73,74) %>%
  dplyr::mutate (ComInsee = case_when(
    ComInsee>"75100" & ComInsee<"75200" ~ "75056",
    ComInsee>"13200" & ComInsee<"13300" ~ "13055",
    ComInsee>"69300" & ComInsee<"69400" ~ "69123",
    TRUE ~ ComInsee) ) 
rm(proprio);rm(equipement)



equip <- equips %>% 
  left_join(.,as.data.frame(passage) %>% distinct(CODGEO_2014,.keep_all = T) %>%
              dplyr::select(CODGEO_2014,CODGEO_2021),by=c("ComInsee"="CODGEO_2014") )%>%              
  left_join(.,basecom %>% dplyr::select (CODGEO_2021=1,2,3,4,5,17,18,31) ,by="CODGEO_2021")  %>%
  left_join(.,as.data.frame(appartenance)%>% 
              filter (NIVGEO=="BV2012") %>% 
              dplyr::select(CODGEO,LIBGEO),
            by=c("BV2012" = "CODGEO") ) 

#rpivotTable(equip)

##base des piscines----

piscines <- equip %>% filter (EquipementTypeCode %in% c('101','102','103') ) %>%
  filter (EquUtilScolaire==1 | (EquNatSurfaceBassin >20 & EquNatProfMax > 1) ) %>%
  mutate(surface=ifelse(EquOuvertSaison==1,EquNatSurfaceBassin/3,EquNatSurfaceBassin) ) %>%
  group_by(CODGEO_2021) %>%
  dplyr::summarise ( surfpiscines=sum(surface,na.rm=T),
                     nbpiscines=n(),
                     nbpiscines_couv=sum(NatureLibelle %in% c("Intérieur","Découvrable") ),
                     nbpisc_saison = sum(EquOuvertSaison == 1)  ) %>%
  full_join(.,basecom %>% dplyr::select (CODGEO_2021=1,2,3,4,5,17,18,31) ,by="CODGEO_2021")  
 #   mutate (surfnat=if_else(is.na(surfpiscines),0,10000*surfpiscines/pop) )
piscines[is.na(piscines)] <- 0


save(piscines,file = "C:/Users/plebre/Documents/projets R/DRAJES/data/sport/piscines.RData")



#calcul des carences ----
g <- ggplot(piscines,aes((popbv),surfpiscines))
f <- g+geom_point() + geom_smooth(method = "auto", size = 1)+ stat_smooth(method = "loess",formula=y~x,size = 0.5,col="red") + theme_classic() + annotate("text",x=9,y=80,label="droite")
ggplotly(f)


pisc.gam <- gam( surfpiscines ~ s(popbv,bs="cs"),data=piscines)
#confint(pisc.gam)
piscines$pisc.pred <- as.vector( predict.gam(pisc.gam, se = F) )
#predictions$delta.pisc <- 100*(equipt$piscines-predictions$pisc.pred)/predictions$pisc.pred
inter <- predict(pisc.gam, se=T)
summary(inter)
inter$upper <- ceiling(inter$fit+2.576*inter$se.fit)
inter$lower <- floor(inter$fit-2.576*inter$se.fit)
inter <- as.data.frame(bind_cols(bv=piscines$BV2012,upper=inter$upper, lower=inter$lower))
piscines$pisc.delta <- if_else(piscines$surfpiscines>=inter$lower & piscines$surfpiscines <=  inter$upper,0,
                               if_else(piscines$surfpiscines>inter$upper,
                                       100*(piscines$surfpiscines-inter$upper)/inter$upper,
                                       100*(piscines$surfpiscines-inter$lower)/inter$lower) )
rm(pisc.gam);rm(inter);rm(f);rm(g)
summary(piscines$pisc.delta)
boxplot(piscines$pisc.delta)



piscines$classement <- cut(piscines$pisc.delta,c(-Inf,-25,-10,10,25,Inf),
                           labels = c("5 très sous doté","4 sous doté","3 normal","2 sur doté","1 très sur doté") )
piscines$classement <- ifelse(piscines$surfpiscines==0,"6 aucune piscine",as.character(piscines$classement))

pal <- colorFactor(c("#006400","#228B22","#F5F5F5","#FF7F50","#B22222","#87CEEB"), 
                   domain = sort(unique(piscines$classement)  ) )

bv_piscines <- merge(bvwgs,piscines,by="BV2012")
#rpivotTable(piscines)



#carte des carences----

leaflet(bv_piscines) %>% addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(mean(bassins_iso$lon), mean(bassins_iso$lat), zoom = 8) %>%
  addAwesomeMarkers(data=bassins_iso,icon=icons,popup = popupTable(bassins_iso,
                                                                   row.numbers = F,feature.id = F)) %>%
  addCircles(data=piscines_bpe_wgs,  color = "blue", stroke=F,radius=300,popup = popupTable(piscines_bpe_wgs, row.numbers = F,feature.id = F),opacity = 0.8 ) %>%
  addPolygons(fillColor = ~pal(classement),stroke = F, fillOpacity =  0.6,
              highlight = highlightOptions (fillOpacity =  0.1, sendToBack = T),
              popup = popupTable(bv_piscines@data %>% 
                                   select(BV2012,LIBGEO,popbv,densité,partZRR,
                                          nbpiscines,nbpiscines_couv,nbpisc_saison,
                                          surfpiscines,pisc.pred,pisc.delta,classement,
                                          surfnat,surfnat_lib) %>%
                                   rename("surf_natatoire<br><sup>(théorie=170m²/10000hab)"=surfnat),
                                 row.numbers = F,feature.id = F) )%>%
  addPolygons(data=reg27carto,fill=F) %>%
  addLegend("bottomright", pal = pal, values = bv_piscines$classement,   
            title = "carence en piscines") 




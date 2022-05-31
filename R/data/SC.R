library(janitor)
require(forcats)
library(lubridate)
library(ggplot2)
library(tidyverse)
#library(plotly)

depbfc <- c("21","25","39","58","70","71","89","90")
dipl <- c("Sortie en cours de terminale ou Bac non validé (IV)",
          "Sortie en cours de seconde ou première (V)",
          "Sortie en cours de dernière année de CAP-BEP, sans valider de diplôme (V)",
          "Sortie en cours de CAP-BEP avant la dernière année (Vbis)",
          "Sortie en cours de 1er cycle de l'enseignement secondaire (6ème à 4ème) (VI)",
          "Sortie en 3ème (Vbis)",
          "CAP-BEP validé (V)")
base2020 <- read.csv2("I:/SUPPORT/04_STATS/engagement/Service Civique/liste_contrats_valides_2021_11_08_10_34_21.geocoded.csv",as.is = T)


basebfc <- base2020 %>% filter(CTV_DATE_DEBUT != "" & substr(dmy((CTV_DATE_DEBUT)),1,4) < "2021") %>%
  select(AGR_NUMERO,CTV_NUMERO,CTV_ETAT,CIV_LIBCOURT,CTV_DATE_DEBUT,CTV_DUREE_HEBDO,CTV_BOURSIER,
         COO_NUMERO_VOIE_VOL,LIBELLE_VOL,COO_LIBELLE_VOIE_VOL,COO_CPLT_ADRESSE_VOL,CODEPOSTALCEDEX_VOL,COMMUNE_VOL,DEPT_VOL,
         VOL_DATE_NAISSANCE,VOL_PAYS_NATIONALITE,VOL_PAYS_NAISSANCE,NIF_LIBELLE,SIF_LIBELLE,SAC_LIBELLE,MRC_MOTIF_RUPTURE,
         mission=NO_MISSION1,CODE_POST_MISSION1,PAYS_MISSION1, 
         ENA_LIBELLE_COURT,CTV_SERVICE_CIVIQUE_INITIATIVE,Devoirs.Faits,MONALISA,évènement.sportif.majeur,Réfugiés.Volont.R,
         result_citycode,result_city,latitude,longitude) %>%
  mutate_if(is.integer,as.character) %>% 
  mutate(CTV_DUREE_HEBDO=as.numeric(CTV_DUREE_HEBDO),
         depvol=substr( ifelse(nchar(result_citycode)==4,paste0("0",result_citycode),result_citycode) ,1 ,2 ) ,
         depmis=ifelse(PAYS_MISSION1 !="FRANCE","ETRANGER", substr (
                               ifelse(nchar(CODE_POST_MISSION1)==4 ,paste0("0",CODE_POST_MISSION1),CODE_POST_MISSION1)  ,1 ,2 ) ) ,
         agre=substr(AGR_NUMERO,1,2),
         debut= substr(CTV_DATE_DEBUT,7,11),
         mission =  ifelse(mission=="03 - Education pour tous","1-Education pour tous",
                           ifelse(mission=="01 - Solidarité","2-Solidarité",
                           ifelse(mission=="05 - Sport","3-Sport",
                           ifelse(mission=="04 - Culture et loisirs","4-Culture & loisirs",
                           ifelse(mission=="07 - Mémoire et citoyenneté","5-Mémoire & citoyenneté",
                           ifelse(mission=="06 - Environnement","6-Environnement",
                           ifelse(mission=="02 - Santé","7-Santé",
                           ifelse(mission=="09 - Intervention d'urgence","8-Intervention d'urgence",
                           ifelse(mission=="08 - Développement international et action humanitaire",
                                          "9-Dvt international & action humanitaire",substr(mission,2,100) ))))))))) ,
         age=time_length(dmy(VOL_DATE_NAISSANCE) %--% dmy(CTV_DATE_DEBUT),"years")
         )
basebfc$depvol <- ifelse(is.na(basebfc$depvol),basebfc$DEPT_VOL,basebfc$depvol)

tableau <- basebfc %>%filter(depmis %in% depbfc ) %>%
  group_by(depmis) %>%
  summarise(tot=n(),tot20=sum(debut==2020),
            mage=mean(age),mage20=mean(age[debut==2020]),
            femmes=100*sum(CIV_LIBCOURT =="MME")/tot,femmes20=100*sum(CIV_LIBCOURT[debut==2020] =="MME")/tot20,
            peudipl=100*sum(NIF_LIBELLE  %in% dipl )/tot,peudipl20=100*sum(NIF_LIBELLE[debut==2020]  %in% dipl )/tot20,
            sitfam=100*sum(SIF_LIBELLE =="Célibataire" )/tot,sitfam20=100*sum(SIF_LIBELLE[debut==2020] =="Célibataire" )/tot20,
            chomage=100*sum(SAC_LIBELLE =="Demandeur d'emploi" )/tot,chomage20=100*sum(SAC_LIBELLE[debut==2020] =="Demandeur d'emploi"  )/tot20,
            rupture=sum(MRC_MOTIF_RUPTURE != " - "),txrupture=100*rupture/tot,
            embauche=100*sum(MRC_MOTIF_RUPTURE %in% c(
              "04 - Embauche en CDD d'au moins 6 mois ou CDI ",
              "05 - Embauche en CDD moins de 6 mois")/rupture )
  ) %>% bind_rows( basebfc %>%filter(depmis %in% depbfc ) %>% group_by() %>%
  summarise(depmis="BFC",tot=n(),tot20=sum(debut==2020),
            mage=mean(age),mage20=mean(age[debut==2020]),
            femmes=100*sum(CIV_LIBCOURT =="MME")/tot,femmes20=100*sum(CIV_LIBCOURT[debut==2020] =="MME")/tot20,
            peudipl=100*sum(NIF_LIBELLE  %in% dipl )/tot,peudipl20=100*sum(NIF_LIBELLE[debut==2020]  %in% dipl )/tot20,
            sitfam=100*sum(SIF_LIBELLE =="Célibataire" )/tot,sitfam20=100*sum(SIF_LIBELLE[debut==2020] =="Célibataire" )/tot20,
            chomage=100*sum(SAC_LIBELLE =="Demandeur d'emploi" )/tot,chomage20=100*sum(SAC_LIBELLE[debut==2020] =="Demandeur d'emploi"  )/tot20,
            rupture=sum(MRC_MOTIF_RUPTURE != " - "),txrupture=100*rupture/tot,
            embauche=100*sum(MRC_MOTIF_RUPTURE %in% c(
              "04 - Embauche en CDD d'au moins 6 mois ou CDI ",
              "05 - Embauche en CDD moins de 6 mois")/rupture ) ) )



vol <-   basebfc %>% filter(depvol %in% depbfc ) %>% mutate(dept_vol=as.character(depvol)) %>%
  group_by(depvol,debut) %>%
  summarise(volresid=n()) 


p <- ggplot(vol,aes(debut,y=volresid,group=depvol,color=depvol)  ) + 
  geom_line(size = 2,linetype = 1) +
  #  geom_smooth()+
  geom_point(size = 4) +
  labs(caption = "Source : Agence du Service Civique, Elisa, traitement DRDJSCS",
       x="",y="volontaires au lieu de résidence",colour="département") +
  # theme_dark()   +
  scale_x_discrete(breaks=2010:2020) +
  theme(
    #panel.background = element_rect(fill = "grey15"),
    legend.key = element_rect(fill = "grey45"),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "grey45"),
    panel.grid.minor = element_line(color = "grey25"),
    legend.position = "right")
#scale_color_jcolors(palette = "pal6")
p

ggplotly(p,  dynamicTicks = T )

int_miss <-   basebfc %>% filter(depvol %in% depbfc & depmis %in% depbfc)    %>%
  filter() %>%
  group_by(mission,debut) %>%
  count() 
sum(int_miss$n)



p <- ggplot(int_miss,aes(x=debut,y=n,fill=mission,color=mission ) ) +
  geom_bar(width = 1,stat = "identity",position = position_stack(reverse = T),color="black" )+
  labs(caption = "Source : Agence du Service Civique, Elisa, traitement DRDJSCS",
       x="année",y="volontaires au lieu de résidence",colour="département") +
  #  theme_dark()   +
  #    scale_color_jcolors(palette = "pal8")+
  scale_x_discrete(breaks=2010:2020)+
  coord_polar("y",start=0) +
  theme(legend.position = "bottom",
        legend.key.size=unit(0.5, "cm"),
        legend.text = element_text(size = 7) )


p

#ggplotly(p)


duree <-   basebfc %>% filter(depvol %in% depbfc) %>%
  filter() %>%
  group_by(debut,depvol) %>%
  summarise(mduree=mean(CTV_DUREE_HEBDO,na.rm=T)) 

p <- ggplot(duree,aes(x=debut,y=mduree,group=depvol,color=depvol))+geom_line() + coord_polar(start = -pi/10) + ylim(23,31)
p
p <- ggplot(duree,aes(x=debut,y=mduree,group=depvol,color=depvol))+ ylim(23,31)+
  #geom_line(size = 2,linetype = 1) +
  geom_smooth(span=1.9,se=F,n=1)+
  geom_point(size = 4) +
  labs(caption = "Source : Agence du Service Civique, Elisa, traitement DRDJSCS",
       x="",y="nombre d'heures hebdomadaires",colour="département") +
  theme_dark()   +
  scale_x_discrete(breaks=2010:2020) +
  theme(
    #panel.background = element_rect(fill = "grey15"),
    legend.key = element_rect(fill = "grey45"),
    panel.border = element_blank(),
    panel.grid.major = element_line(color = "grey45"),
    panel.grid.minor = element_line(color = "grey25"),
    legend.position = "bottom")
#scale_color_jcolors(palette = "pal6")
p

ggplotly(p)

duree2 <- spread(duree,debut,mduree)



table(basebfc1$CIV_LIBCOURT)
a <-as.data.frame( table(basebfc1$NIF_LIBELLE) )

table(basebfc1$SIF_LIBELLE)
table(basebfc1$SAC_LIBELLE)
table(basebfc1$MRC_MOTIF_RUPTURE)

rpivotTable(basebfc1)





### Traitement QPV

#baseqpv <- base2020 %>% select(CTV_NUMERO,COO_NUMERO_VOIE_VOL,LIBELLE_VOL,COO_LIBELLE_VOIE_VOL,CODEPOSTALCEDEX_VOL,COMMUNE_VOL) %>%
#  mutate(adresse=toupper( 
#    ifelse(is.na(COO_NUMERO_VOIE_VOL) | COO_NUMERO_VOIE_VOL==0, 
#           str_trim(paste(LIBELLE_VOL,COO_LIBELLE_VOIE_VOL) ),
#           str_trim( paste(COO_NUMERO_VOIE_VOL,LIBELLE_VOL,COO_LIBELLE_VOIE_VOL)) ) ) ,
#    adresse=enc2native(adresse) )
#attention de laisser dans l'ordre et supprimer première colonne
# baseqpv <- baseqpv %>% select ("v1"=1,"v2"=5,"v3"=6,"v4"=7,"v5"=2) 

### PENSER à SUPPRIMER 1ERE COLONNE
#write.csv2(baseqpv,"I:/SUPPORT/04_STATS/engagement/Service Civique/baseqpv.csv")   


basesig <- read.csv2("I:/SUPPORT/04_STATS/engagement/Service Civique/baseqpv_QP_211108_1419.csv",as.is = T)

basebfcsig <- left_join(basebfc,basesig,by=c("CTV_NUMERO"="X_IDENTIFIANT") )

#write.dbf(basebfc,"/home/ps/Documents/teletravail/réseau/JEUNESSE/SERVICE CIVIQUE/base2020/baseqpvverif.dbf")

addmargins(table(basesig$TYPE_QUARTIER,basesig$LOCADR_REF) )
table(basesig$LOCADR_REF)

qp <- c("VOIE_EN_LIMITE","VOIE_EN_PARTIE","VOIE_ENTIERE") 
qp <- c("VOIE_ENTIERE") 

tableauqpv <- basebfc %>% mutate(dep_v=substr(CODE_COMMUNE_INSEE,1,2) ) %>%
  filter(dep_v %in% depbfc ) %>% 
  group_by(dep_v) %>%
  summarise(qpv=sum(!(CODE_QUARTIER=="")), n() ,tx= 100*qpv/n() 
            #chomageqpv=100*sum(LOCADR_REF[SAC_LIBELLE =="Demandeur d'emploi"] =="OUI" )/qpv,
  ) %>% adorn_totals()
write.csv(tableauqpv,"C:/Users/plebre/Documents/projets R/service civique/2020/tableau.csv")
rpivotTable::rpivotTable(basebfc%>% mutate(dep_v=substr(CODE_COMMUNE_INSEE,1,2) ))  


save(basebfc,basebfcsig,oursin,oursinxy,flows,file="data/engagement/SC.RData")

library(geosphere)
library(rgdal)

oursin <- read.dbf("I:/SUPPORT/04_STATS/Jeunesse/SERVICE CIVIQUE/service_civique_2019/oursin.dbf")


library(photon)


load("data/demo/cartes.RData")
cartexy <- centroid(com27wgs)
cartexy <- cbind(com27wgs,cartexy)

oursinxy <- left_join(oursin,cartexy@data %>% select(INSEE_COM,origine=NOM_COM,origine.x=X1,origine.y=X2),by=c("volontaire"="INSEE_COM"))
oursinxy <- left_join(oursinxy,cartexy@data %>% select(INSEE_COM,destination=NOM_COM,destination.x=X1,destination.y=X2),by=c("mission"="INSEE_COM")) %>%
  filter(!is.na(origine.x) & !is.na(destination.x)) 
oursinxy <- oursinxy %>%  group_by(destination) %>%
  mutate(missions=sum(nombre),
         rayon=missions*10)

library(ggforce)

flows <- gcIntermediate(oursinxy[,5:6],oursinxy[,8:9],n=5, sp=T,addStartEnd = T)

ggplot()+geom_bezier(aes(x=oursinxy[,5],y=oursinxy[,8]),data=oursinxy)

flows$nombre <- oursinxy$nombre
flows$origine <- oursinxy$origine
flows$destination <- oursinxy$destination

library(leaflet)
library(RColorBrewer)

hover <- paste0(flows$origine, " à ", 
                flows$destination, ': ', 
                as.character(flows$nombre))

pal <- colorFactor(brewer.pal(4, 'Set2'), flows$origine)



leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(data=oursinxy, ~destination.x, ~destination.y,
             weight =~oursinxy%>% group_by(destination)%>% mutate(sum(nombre)^(1/2))%>%pull, 
             #radius=40, 
             label = ~as.character(destination),color="#ffa500", stroke = TRUE, fillOpacity = 0.5) %>%
  addPolylines(data = flows, 
               weight = ~ifelse(nombre>3,nombre^(1/2), 0),
               label = hover,
              # group = ~origine
              stroke = T, 
              #color = "#6eff2a",
              #color = colorRampPalette(c("orange", "green"), alpha = TRUE)(8),
              #color = rainbow(30, alpha = NULL),
              #opacity = 0.8,
              fill = F, fillOpacity = 0.8, dashArray = NULL,
              smoothFactor = 1,
              fillColor = colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(8),
              color = scales::seq_gradient_pal(low = "lightblue", high = "lightgreen", space = "Lab")(seq(0, 1, length.out = 25))
                             ) 


leaflet(oursinxy) %>%
  addProviderTiles('CartoDB.Positron')  %>%
  addFlows( oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(origine.x),
            oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(origine.y),
            oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(destination.x),
            oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(destination.y),
            flow = oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(nombre),
               #popup = hover,
               # group = ~origine
               opacity = 0.6, 
               color = "lightblue",
               #color = colorRampPalette(c("orange", "green"), alpha = TRUE),
               #color = rainbow(30, alpha = NULL),
               #fill = F, fillOpacity = 0.8, dashArray = NULL,
               #smoothFactor = 1,
               #fillColor = colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(8),
               #color = scales::seq_gradient_pal(low = "lightblue", high = "lightgreen", space = "Lab")(seq(0, 1, length.out = 25))
  ) %>%
  addMinicharts( oursinxy %>% filter(destination==origine) %>% pull(destination.x),
                 oursinxy %>% filter(destination==origine) %>% pull(destination.y),
                 chartdata =  oursinxy %>% filter(destination==origine) %>%ungroup() %>% mutate(entrant=missions-nombre) %>%
                   select(stable=nombre,entrant),
                  opacity = 0.7,              
                 type = "pie",
                 colorPalette = c("lightgreen", "lightblue"),
                width = oursinxy %>% filter(destination==origine) %>% mutate(2*missions^(1/2)) %>% pull(12),
               #label = ~as.character(destination),color="#ffa500", stroke = TRUE, fillOpacity = 0.5
             )



library(mapdeck)


mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
   add_animated_arc(
    data = oursinxy[oursinxy$nombre>4,]
    , layer_id = "arc_layer"
    , origin = c("origine.x", "origine.y")
    , destination = c("destination.x", "destination.y")
    , stroke_from = "origine"
    , stroke_to = "destination"
    , stroke_width = ifelse("nombre"> 10,"nombre", 0)
    , animation_speed = 1
  ) %>%
  add_scatterplot(
    data = oursinxy[oursinxy$nombre>0,]
                  , layer_id = "arc_point"
                  , lon = "destination.x"
                  , lat = "destination.y"
                  , radius = "rayon"
                  , fill_colour = "country"
                  )









#Identify the points of the curve
p1 <- c(oursinxy$origine.x,
        oursinxy$origine.y)
p2 <- c(oursinxy$destination.x,
        oursinxy$destination.y)

#Create function to draw Brezier curve
bezier.curve <- function(p1, p2, p3) {
  n <- seq(0,1,length.out=50)
  bx <- (1-n)^2 * p1[[1]] +
    (1-n) * n * 2 * p3[[1]] +
    n^2 * p2[[1]]
  by <- (1-n)^2 * p1[[2]] +
    (1-n) * n * 2 * p3[[2]] +
    n^2 * p2[[2]]
  data.frame(lon=bx, lat=by)
}

bezier.arc <- function(p1, p2) {
  intercept.long <- (p1[[1]] + p2[[1]]) / 2
  intercept.lat  <- 85
  p3 <- c(intercept.long, intercept.lat)
  bezier.curve(p1, p2, p3)
}

arc3 <- bezier.arc(p1,p2)

bezier.uv.arc <- function(p1, p2) {
  # Get unit vector from P1 to P2
  u <- p2 - p1
  u <- u / sqrt(sum(u*u))
  d <- sqrt(sum((p1-p2)^2))
  
  # Calculate third point for spline
  m <- d / 2
  h <- floor(d * .2)
  
  # Create new points in rotated space 
  pp1 <- c(0,0)
  pp2 <- c(d,0)
  pp3 <- c(m, h)
  
  mx <- as.matrix(bezier.curve(pp1, pp2, pp3))
  
  # Now translate back to original coordinate space
  theta <- acos(sum(u * c(1,0))) * sign(u[2])
  ct <- cos(theta)
  st <- sin(theta)
  tr <- matrix(c(ct,  -1 * st, st, ct),ncol=2)
  tt <- matrix(rep(p1,nrow(mx)),ncol=2,byrow=TRUE)
  points <- tt + (mx %*% tr)
  
  tmp.df <- data.frame(points)
  colnames(tmp.df) <- c("lon","lat")
  tmp.df
}


arc4 <- bezier.uv.arc(p1,p2)

bezier.uv.merc.arc <- function(p1, p2) {
  pp1 <- p1
  pp2 <- p2
  pp1[2] <- asinh(tan(p1[2]/180 * pi))/pi * 180
  pp2[2] <- asinh(tan(p2[2]/180 * pi))/pi * 180
  
  arc <- bezier.uv.arc(pp1,pp2)
  arc$lat <-  atan(sinh(arc$lat/180 * pi))/pi * 180
  arc
}


arc5 <- bezier.uv.merc.arc(p1, p2)


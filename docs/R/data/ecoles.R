library(tidyverse)
library(fontawesome)
library(leaflet)
library(leaflet.extras2)


#écoles----
ecoles <- read.csv2("I:/SUPPORT/04_STATS/Sources/INSEE/BPE/bpe19_enseignement_xy.csv",as.is = T,encoding = "UTF-8")
## Recodage de ecoles$TYPEQU en ecoles$type_ecole
ecoles$Type_ecole <- fct_recode(ecoles$TYPEQU,
                                "Maternelle" = "C101",
                                "Maternelle" = "C102",
                                "Elémentaire" = "C104",
                                "Elémentaire" = "C105")
ecoles$Cantine <- fct_recode(ecoles$CANT,
                             "Sans objet" = "X",
                             "Absence de cantine" = "0",
                             "Présence de cantine" = "1",
                             "Non renseigné" = "")
ecoles$RPIC <- fct_recode(ecoles$RPIC,
                          "Sans objet" = "X",
                          "Absence de regroupement pédagogique intercommunal concentré" = "0",
                          "Présence de regroupement pédagogique intercommunal concentré" = "1")
ecoles$Pre_elementaire <- fct_recode(ecoles$CL_PELEM,
                                     "Sans objet" = "X",
                                     "Absence de classe pré-élementaire" = "0",
                                     "Présence de classe pré-élémentaire" = "1")
ecoles$Educ_prioritaire <- fct_recode(ecoles$EP,
                                      "Sans objet" = "X",
                                      "Non appartenance" = "0",
                                      "Apartenance" = "1")
ecoles$Secteur <- fct_recode(ecoles$SECT,
                             "Privé" = "PR",
                             "Public" = "PU")


ecole <- ecoles%>% filter (REG %in% c("27") & QUALITE_XY != "Non géolocalisé" &
                             TYPEQU %in% c("C101","C102","C103","C104","C105")       ) %>%
  mutate(x=as.numeric(LAMBERT_X),y=as.numeric(LAMBERT_Y)) %>%
  dplyr::select(DEP,DEPCOM,Type_ecole,Cantine,RPIC,Pre_elementaire,Educ_prioritaire,Secteur,x,y,QUALITE_XY)

coordinates(ecole) <- ~x+y
proj4string(ecole) <- CRS("+init=epsg:2154")
ecole <- spTransform(ecole,CRS=CRS("+init=epsg:4326"))





ecoleicon <- iconList(
  Maternelle = makeIcon("images/briefcase-mat.svg","images/briefcase-mat.svg",10,30),
  Elémentaire = makeIcon("images/briefcase-ele.svg","images/briefcase-ele.svg",10,30) )



#communes équipées d'au moins une école élémentaire ou primaire
ecolecom <- ecoles %>% filter (TYPEQU %in% c("C101","C102","C104","C105") & REG=='27') %>%
  group_by(DEPCOM) %>% summarise(Maternelle= sum(TYPEQU %in% c("C101","C102") ),
                                 Primaire =  sum(TYPEQU %in% c("C104","C105") ) ) %>% 
  rename(INSEE_COM=DEPCOM)


comecole <- merge(com27wgs,ecolecom,by="INSEE_COM")
comecole <- subset(comecole,comecole$Maternelle>0 |comecole$Primaire>0 )


save(ecole,comecole, ecoleicon ,
     file="data/BPE/ecoles.RData")

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



save(basebfc,basebfcsig,file="data/engagement/SC.RData")

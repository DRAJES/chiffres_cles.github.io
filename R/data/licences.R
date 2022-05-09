library(tidyverse)
library(readxl)

licences <- read.csv2("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/2. Données data.gouv.fr/2018/lic-data-2018.csv",as.is = T)
load("data/demo/basecom.RData")

lic_dep <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-departement-2020.xlsx",sheet = 2,skip=2)
levels(lic_dep) <-  lic_dep[1,]

lic_reg <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-région-2020.xlsx",sheet = 2,skip=2)
levels(lic_reg) <-  lic_reg[1,]
lic_reg_sexe <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-region-et-sexe-2020.xlsx",sheet = 5,skip = 2)
levels(lic_reg_sexe) <-  lic_reg_sexe[1,]
fedsexe <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-sexe-2020.xlsx",sheet = 2,skip = 2)

fedsexeuni <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-departement-et-sexe-2020.xlsx",sheet = 2,skip = 2)
fedsexenoly <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-departement-et-sexe-2020.xlsx",sheet = 3,skip = 2)
fedsexemulti <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-departement-et-sexe-2020.xlsx",sheet = 4,skip = 2)
feddepsexe <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-departement-et-sexe-2020.xlsx",sheet = 5,skip = 2)
levels(feddepsexe) <-  feddepsexe[1,]

lic_reg <- lic_reg %>% slice(2:118) %>% dplyr::select(-c(3,17:21,23:24)) %>% mutate_at(3:16,as.numeric)
lic_reg_sexe <- lic_reg_sexe %>% slice (2:18) %>% dplyr::select(-c(3,17:21,23:24)) %>% mutate_at(3:16,as.numeric)


lic_dep_bfc <- lic_dep %>% slice (2:118) %>%
              dplyr::select(1:2,"21","25","39","58","70","71","89","90",France="...114") %>%
              bind_cols(BFC=lic_reg$`27`) %>% 
              relocate(BFC,.before=France) %>% mutate_at(3:12,as.numeric)
feddepsexe <- feddepsexe %>%  slice (2:18) %>%
              dplyr::select(1:2,"21","25","39","58","70","71","89","90",France="...114") %>%
              bind_cols(BFC=lic_reg_sexe$`27`)%>%
              relocate(BFC,.before=France) %>% mutate_at(3:12,as.numeric)
feddepsexe <- as.data.frame(t(feddepsexe)) %>% select(16:17) %>% rownames_to_column() %>%  mutate(txfem=100*as.numeric(V16)/as.numeric(V17))


fede_sexe_dep <- fedsexeuni %>% filter (`Codes départements` %in% c('Licences féminines','Sous/Total')) %>% select(1,2,depbfc) %>% mutate_at(3:10,as.numeric) %>% adorn_totals("col",name = "BFC") %>%
  select(1,2,11) %>% pivot_wider(names_from =`Codes départements` ,values_from = BFC) %>% mutate(txfem=round(100*`Licences féminines`/`Sous/Total`,1)) %>%
        bind_rows(fedsexenoly %>% filter (`Codes départements` %in% c('Licences féminines','Sous/Total')) %>% select(1,2,depbfc) %>% mutate_at(3:10,as.numeric) %>% adorn_totals("col",name = "BFC") %>%
  select(1,2,11) %>% pivot_wider(names_from =`Codes départements` ,values_from = BFC) %>% mutate(txfem=round(100*`Licences féminines`/`Sous/Total`,1)) )%>%
      bind_rows(fedsexemulti %>% filter (`Codes départements` %in% c('Licences féminines','Sous/Total')) %>% select(1,2,depbfc) %>% mutate_at(3:10,as.numeric) %>% adorn_totals("col",name = "BFC") %>%
  select(1,2,11) %>% pivot_wider(names_from =`Codes départements` ,values_from = BFC) %>% mutate(txfem=round(100*`Licences féminines`/`Sous/Total`,1)) )
  
  


#rpivotTable(fedsexeuni)
licences27 <- basecom %>% 
  dplyr::filter(CODGEO %in% (basecom %>% dplyr::filter (EPCI %in% basecom$EPCI[basecom$REG=="27"] |
                                                          BV2012 %in% basecom$BV2012[basecom$REG=="27"]) %>%
                               dplyr::select (CODGEO) %>% pull ) ) %>%
  dplyr::select(CODGEO,EPCI,BV2012,poph,popf,pop) %>%
  left_join(.,licences %>% 
              dplyr::select (code_commune,fede=fed_2018,licences=l_2018,
                             lic_femmes=l_f_2018,lic_hommes=l_h_2018,lic_qpv=l_qp_2018)
            ,by=c("CODGEO"="code_commune")) %>% 
  left_join(.,clubs %>%
              dplyr::select (Code_commune, code_federation,clubs_sportifs_2018,etablissements_prof_2018,total_clubs_2018)
            ,by=c("CODGEO"="Code_commune","fede"="code_federation")) 



lic27epci <- licences27 %>% group_by(CODGEO,EPCI,pop,popf,poph) %>%
                             dplyr::filter (EPCI %in% basecom$EPCI[basecom$REG=="27"] ) %>%
                            summarise(licences=sum(licences,na.rm=T),
                                      licfemmes=sum(lic_femmes,na.rm=T),
                                      lichommes=sum(lic_hommes,na.rm=T),
                                      clubs_sportifs_2018=sum(clubs_sportifs_2018,na.rm=T),
                                      etablissements_prof_2018=sum(etablissements_prof_2018,na.rm=T),
                                      total_clubs_2018=sum(total_clubs_2018,na.rm=T)) %>%
                          group_by(EPCI) %>% 
                          summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
                          mutate(txlic=100*licences/pop,
                                 txfemmes=100*licfemmes/licences,
                                 txlicf=100*licfemmes/popf,
                                 txlich=100*lichommes/poph,
                                 txclub=10000*total_clubs_2018/pop,
                                 liclub=licences/total_clubs_2018) %>%
  left_join(.,appartenance %>% dplyr::filter(NIVGEO=="EPCI") %>%
              dplyr::select(CODGEO,LIBGEO) ,
            by=c("EPCI" = "CODGEO") ) 

lic27bv <- licences27 %>% group_by(CODGEO,BV2012,pop,popf,poph) %>%
  dplyr::filter ( BV2012 %in% basecom$BV2012[basecom$REG=="27"] ) %>%
    summarise(licences=sum(licences,na.rm=T),
            licfemmes=sum(lic_femmes,na.rm=T),
            lichommes=sum(lic_hommes,na.rm=T),
            clubs_sportifs_2018=sum(clubs_sportifs_2018,na.rm=T),
            etablissements_prof_2018=sum(etablissements_prof_2018,na.rm=T),
            total_clubs_2018=sum(total_clubs_2018,na.rm=T)) %>%
  group_by(BV2012) %>% 
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  mutate(txlic=100*licences/pop,
         txfemmes=100*licfemmes/licences,
         txlicf=100*licfemmes/popf,
         txlich=100*lichommes/poph,
         txclub=10000*total_clubs_2018/pop,
         liclub=licences/total_clubs_2018) %>%
  left_join(.,appartenance %>% dplyr::filter(NIVGEO=="BV2012") %>%
              dplyr::select(CODGEO,LIBGEO) ,
            by=c("BV2012" = "CODGEO") ) 



save(lic27bv,lic27epci,lic_dep,lic_dep_bfc,feddepsexe,lic_reg_sexe,lic_reg,fedsexe,fede_sexe_dep,
     file="data/sport/licences.RData")



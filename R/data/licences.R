library(tidyverse)
library(readxl)

licences <- read.csv2("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/2. Données data.gouv.fr/2019/lic-data-2019.csv",as.is = T)
load("data/demo/basecom.RData")

lic_dep <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-departement-2021.xlsx",sheet = 2,skip=2)

lic_reg <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-région-2021.xlsx",sheet = 2,skip=2)
lic_reg_sexe <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-region-et-sexe-2021.xlsx",sheet = 5,skip = 2)
fedsexe <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-sexe-2021.xlsx",sheet = 2,skip = 2)

fedsexeuni <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-departement-et-sexe-2021.xlsx",sheet = 2,skip = 2)
fedsexenoly <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-departement-et-sexe-2021.xlsx",sheet = 3,skip = 2)
fedsexemulti <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-departement-et-sexe-2021.xlsx",sheet = 4,skip = 2)
feddepsexe <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-departement-et-sexe-2021.xlsx",sheet = 5,skip = 2)

lic_reg[118,2] <- "Total général (hors groupements nationaux)"
lic_reg <- lic_reg %>% 
  slice(2:118) %>% 
  filter(!is.na(`Codes régions`)) %>%
  dplyr::select(-c(3,17:21,23:24)) %>% 
  mutate_at(3:16,as.numeric) %>%
  rename(code_fede=...1,fede=`Codes régions`,FM=...22)

lic_reg_sexe <- lic_reg_sexe %>% 
  slice (2:18) %>% 
  dplyr::select(-c(3,17:21,23:24)) %>% 
  mutate_at(3:16,as.numeric) %>%
  rename(code_fede=...1,fede=`Codes régions`,FM=...22)

lic_dep[118,2] <- "Total général (hors groupements nationaux)"
lic_dep_bfc <- lic_dep %>% 
  slice (2:118) %>%
  filter(!is.na(`Codes départements`)) %>%
  dplyr::select(1:2,depbfc,FM="...114") %>%
  bind_cols(BFC=lic_reg$`27`) %>% 
  relocate(BFC,.before=FM) %>% 
  mutate_at(3:12,as.numeric) %>%
  rename(code_fede=...1,fede=`Codes départements`)


feddepsexe <- feddepsexe %>%  
  slice (2:18) %>%
  dplyr::select(1:2,depbfc,France="...114") %>%
  bind_cols(BFC=lic_reg_sexe$`27`)%>%
  relocate(BFC,.before=France) %>% 
  mutate_at(3:12,as.numeric)

feddepsexe <- as.data.frame(t(feddepsexe)) %>% 
  select(16:17) %>% 
  rownames_to_column() %>% 
  mutate(txfem=100*as.numeric(V16)/as.numeric(V17))


fede_sexe_dep <- fedsexeuni %>% 
  select(1:2,depbfc)%>% 
  slice(2:149,151:154) %>% 
  bind_rows(fedsexenoly %>% 
              select(1:2,depbfc)%>% 
              slice(2:201,203:206)) %>%
  bind_rows(fedsexemulti %>% 
              select(1:2,depbfc)%>%
              slice(2:93,95:98)) 

fede_sexe_dep  <- fede_sexe_dep %>% 
  select(-1) %>% 
  bind_cols(
    fede_sexe_dep %>% 
      select(...1) %>% 
      filter(!is.na(...1)) %>%
      slice(rep(1:n(),each=4)) )   %>%    #répétition du code fédé
  
  mutate_at(2:9,as.numeric) %>%
  adorn_totals("col",name = "BFC") %>%
  filter(!is.na(`21`)) %>% pivot_longer(c(2:9,11)) %>%
  pivot_wider(names_from = 1,values_from = value) %>%
  mutate(txfem=round(100*`Licences féminines`/`Sous/Total`,1)) %>%
  rename(DEP=name,fede=...10)

licences27 <- basecom %>% 
  dplyr::filter(CODGEO %in% (basecom %>% 
                               dplyr::filter (EPCI %in% 
                                                basecom$EPCI[basecom$REG=="27"] |
                                                BV2012 %in% 
                                                basecom$BV2022[basecom$REG=="27"]) %>%
                               dplyr::select (CODGEO) %>% 
                               pull ) ) %>%
  dplyr::select(CODGEO,EPCI,BV2022,poph,popf,pop,p20) %>%
  left_join(.,licences %>% 
              dplyr::select (code_commune,fede=fed_2019,licences=l_2019,
                             lic_femmes=l_f_2019,lic_hommes=l_h_2019,
                             lic_qpv=l_qp_2019,l_10_14_2019,l_5_9_2019,l_0_4_2019) %>%
              mutate (lic_jeunes=l_10_14_2019+l_5_9_2019+l_0_4_2019)
            ,by=c("CODGEO"="code_commune") ) %>% 
  left_join(.,clubs %>%
              dplyr::select (code_commune, code_federation,clubs_sportifs_2019,
                             etablissements_prof_2019,total_clubs_2019)
            ,by=c("CODGEO"="code_commune","fede"="code_federation")) 

#rpivotTable(licences)

lic27epci <- licences27 %>% 
  group_by(CODGEO,EPCI,pop,popf,poph,p20) %>%
  dplyr::filter (EPCI %in% basecom$EPCI[basecom$REG=="27"] ) %>%
  summarise(licences=sum(licences,na.rm=T),
            licfemmes=sum(lic_femmes,na.rm=T),
            lichommes=sum(lic_hommes,na.rm=T),
            licjeunes=sum(lic_jeunes,na.rm=T),
            clubs_sportifs=sum(clubs_sportifs_2019,na.rm=T),
            etablissements=sum(etablissements_prof_2019,na.rm=T),
            total_clubs=sum(total_clubs_2019,na.rm=T)) %>%
  group_by(EPCI) %>% 
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  mutate(txlic=100*licences/pop,
         txfemmes=100*licfemmes/licences,
         txjeunes=100*licjeunes/licences,
         txlicjeunes=100*licjeunes/p20,
         txlicf=100*licfemmes/popf,
         txlich=100*lichommes/poph,
         txclub=10000*total_clubs/pop,
         liclub=licences/total_clubs) %>%
  left_join(.,appartenance %>% 
              dplyr::filter(NIVGEO=="EPCI") %>%
              dplyr::select(CODGEO,LIBGEO) ,
            by=c("EPCI" = "CODGEO") ) 

lic27bv <- licences27 %>% 
  group_by(CODGEO,BV2022,pop,popf,poph,p20) %>%
  dplyr::filter ( BV2022 %in% basecom$BV2022[basecom$REG=="27"] ) %>%
  summarise(licences=sum(licences,na.rm=T),
            licfemmes=sum(lic_femmes,na.rm=T),
            lichommes=sum(lic_hommes,na.rm=T),
            licjeunes=sum(lic_jeunes,na.rm=T),
            clubs_sportifs=sum(clubs_sportifs_2019,na.rm=T),
            etablissements_prof=sum(etablissements_prof_2019,na.rm=T),
            total_clubs=sum(total_clubs_2019,na.rm=T)) %>%
  group_by(BV2022) %>% 
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  mutate(txlic=100*licences/pop,
         txfemmes=100*licfemmes/licences,
         txjeunes=100*licjeunes/licences,
         txlicjeunes=100*licjeunes/p20,
         txlicf=100*licfemmes/popf,
         txlich=100*lichommes/poph,
         txclub=10000*total_clubs/pop,
         liclub=licences/total_clubs) %>%
  left_join(.,appartenance %>% dplyr::filter(NIVGEO=="BV2022") %>%
              dplyr::select(CODGEO,LIBGEO) ,
            by=c("BV2022" = "CODGEO") ) 



save(lic27bv,lic27epci,lic_dep,lic_dep_bfc,feddepsexe,lic_reg_sexe,lic_reg,fedsexe,fede_sexe_dep,
     file="data/sport/licences.RData")



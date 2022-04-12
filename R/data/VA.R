library(rio)
library(data.table)
library(rgdal)
library(tidyverse)


RNAimportfr <-  import_list("I:/SUPPORT/04_STATS/engagement/Vie associative/RNA/rna_import_20201201.zip", 
                            rbind = T,setclass = "data.table",colClasses="character") 
RNAwaldecfr <-  import_list("I:/SUPPORT/04_STATS/engagement/Vie associative/RNA/rna_waldec_20201201.zip", 
                            rbind = T,setclass = "data.table",colClasses="character") 

passage <- import("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table_passage_annuelle_2021.xlsx",sheet=1,skip=5)
passage43 <- import("I:/SUPPORT/05_CARTO/Fonds de cartes/communes/table1943_2018.dbf",as.is = T)
passage[5174,]$CODGEO_2018 <- '14666'

RNAimport <- RNAimportfr %>% filter(position=="A") %>%
  filter(as.numeric(adrs_codeinsee)>=01000 & as.numeric(adrs_codeinsee)<95800) %>%
  mutate(adrs_codeinsee = case_when(
    adrs_codeinsee>"75100" & adrs_codeinsee<"75200" ~ "75056",
    adrs_codeinsee>"13200" & adrs_codeinsee<"13300" ~ "13055",
    adrs_codeinsee>"69300" & adrs_codeinsee<"69400" ~ "69123",
  TRUE ~ adrs_codeinsee) ) %>%  #erreur dans la base
  mutate(obj=if_else(objet_social1=='050000',
                     as.numeric(objet_social2),
                     as.numeric(objet_social1) ) ,
         objr=cut(obj,
                  breaks = c(0,1000, 6000, 7000, 10000, 11000, 14000, 15000,
                             16000, 17000, 19000, 22000, 24000, 
                             30000, 32000, 51000),
                  include.lowest = T,right = F,
                  labels = c("NR", "Autres","Culture","Loisirs",
                             "Autres","Sports","Amicales-Entraide",
                             "Enseignement","Autres","Santé","Social",
                             "Economie","Environnement",
                             "Economie","Autres") ) ) %>%
left_join(.,unique (passage43),by=c("adrs_codeinsee"="GEO1943")) %>%
  left_join(.,unique( passage %>% select(2,8) ),by=c("GEO2018"="CODGEO_2018")) 


RNAwaldec <- RNAwaldecfr %>%    filter(as.numeric(adrs_codeinsee)>=01000 & as.numeric(adrs_codeinsee)<95800) %>%
  mutate(obj=if_else(objet_social1=='050000',
                     as.numeric(objet_social2),
                     as.numeric(objet_social1) ) ,
         objr=cut(obj,
                  breaks = c(0,1000, 6000, 7000, 10000, 11000, 14000, 15000,
                             16000, 17000, 19000, 22000, 24000, 
                             30000, 32000, 51000),
                  include.lowest = T,right = F,
                  labels = c("NR", "Autres","Culture","Loisirs",
                             "Autres","Sports","Amicales-Entraide",
                             "Enseignement","Autres","Santé","Social",
                             "Economie","Environnement",
                             "Economie","Autres") ) ) %>%
left_join(.,unique(passage43),by=c("adrs_codeinsee"="GEO1943")) %>%
  left_join(.,unique(passage %>% select(2,8)),by=c("GEO2018"="CODGEO_2018")) 



RNA <- RNAimport %>% 
  filter(position=="A") %>% filter(observation!="DISSOUTE")%>%
  select(id,siret,gestion,date_creat,date_publi,
         nature,groupement,titre,obj,objr,
         adrs_codepostal,adrs_codeinsee,observation,position,
         rup_mi,maj_time,CODGEO_2021) %>%
  bind_rows(
    RNAwaldec %>%
      filter(position=="A") %>%
      select(id,siret,gestion,date_creat,date_publi,
             nature,groupement,titre,obj,objr,
             adrs_codepostal,adrs_codeinsee,observation,position,
             rup_mi,maj_time,CODGEO_2021)
  ) %>%
  mutate(DEP=substr(adrs_codeinsee,1,2))

RNAcom <- RNA %>% group_by(CODGEO_2021,DEP,objr) %>% summarise(total=n())


save(RNA,RNAcom,   file = "C:/Users/plebre/Documents/projets R/DRAJES/data/engagement/asso.RData")

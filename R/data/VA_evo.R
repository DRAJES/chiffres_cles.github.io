library(rio)
library(glue)
library(data.table)
library(rgdal)
library(rvest)
library(httr)
library(purrr)
library(stringr)
library(tidyverse)

passage <- import("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2022/table_passage_annuelle_2022.xlsx",sheet=1,skip=5)
passage43 <- import("I:/SUPPORT/05_CARTO/Fonds de cartes/communes/table1943_2018.dbf",as.is = T)
passage[5174,]$CODGEO_2018 <- '14666'


url_import <- session("https://media.interieur.gouv.fr/rna/")
url_waldec <- "https://media.interieur.gouv.fr/rna/rna_waldec_%s01.zip"

ref <- "201911"
ref <- c(ref,"201912")

for(i in "2020":"2022"){
  for(j in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    ref <- c (ref,paste0(i,j))
  }
}

url <-  "https://media.interieur.gouv.fr/rna/rna_import_20191101.zip"
tmp <- tempfile()
curl_download(url, tmp, quiet = F)
              
for (i in ref)       {
  url <-  glue("https://media.interieur.gouv.fr/rna/rna_import_{i}01.zip")
  tmp <- tempfile()
  curl_download(url, tmp, quiet = F)
  RNAimportfr <-  import_list(tmp, 
                              rbind = T,setclass = "data.table",colClasses="character") 
  
  url <-  glue("https://media.interieur.gouv.fr/rna/rna_waldec_{i}01.zip")
  tmp <- tempfile()
  curl_download(url, tmp, quiet = F)
  RNAwaldecfr <-  import_list(waldec, 
                              rbind = T,setclass = "data.table",colClasses="character") 
  
  
  
  RNAimport <- RNAimportfr %>% 
    filter(position=="A") %>%
    filter(adrs_codeinsee >= '01000' & adrs_codeinsee < '95800') %>%
    mutate(adrs_codeinsee = case_when(
      adrs_codeinsee>"75100" & adrs_codeinsee<"75200" ~ "75056",
      adrs_codeinsee>"13200" & adrs_codeinsee<"13300" ~ "13055",
      adrs_codeinsee>"69300" & adrs_codeinsee<"69400" ~ "69123",
      TRUE ~ adrs_codeinsee) ) %>%  #erreur dans la base
    mutate(objet_social2=as.numeric(objet_social2),
           objet_social1=as.numeric(objet_social1),
           obj=if_else(objet_social1=='050000',
                       objet_social2,
                       objet_social1 ) ,
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
    left_join(.,unique( passage %>% select(2,10) ),by=c("GEO2018"="CODGEO_2018")) 
  
  
  RNAwaldec <- RNAwaldecfr %>%    
    filter(adrs_codeinsee >= '01000' & adrs_codeinsee < '95800') %>%
    filter(objet_social2!='023\xe0\xe0(') %>%
    mutate(objet_social2=as.numeric(objet_social2),
           objet_social1=as.numeric(objet_social1),
           obj=if_else(objet_social1=='050000',
                       objet_social2,
                       objet_social1 ) ,
           objr = cut(obj,
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
    left_join(.,unique(passage %>% select(2,10)),by=c("GEO2018"="CODGEO_2018")) 
  
  
  
  RNA <- RNAimport %>% 
    filter(position=="A") %>% filter(observation!="DISSOUTE")%>%
    select(id,siret,gestion,date_creat,date_publi,
           nature,groupement,titre,obj,objr,
           adrs_codepostal,adrs_codeinsee,observation,position,
           rup_mi,maj_time,CODGEO_2022) %>%
    bind_rows(
      RNAwaldec %>%
        filter(position=="A") %>%
        select(id,siret,gestion,date_creat,date_publi,
               nature,groupement,titre,obj,objr,
               adrs_codepostal,adrs_codeinsee,observation,position,
               rup_mi,maj_time,CODGEO_2022)
    ) %>%
    mutate(DEP=substr(adrs_codeinsee,1,2))
  
  RNAcom <- RNA %>% 
    group_by(CODGEO_2022,DEP,objr) %>% 
    summarise(total=n()) %>% 
    mutate(annee=paste(j,i))
  
  
  base_asso <- bind_rows(RNAcom)
       }
       

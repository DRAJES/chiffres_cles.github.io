library(tidyverse)
library(readODS)

#load("~/projets R/service civique/sc2023.RData")
#mission <- read_ods("C:/Users/plebre/Documents/projets R/service civique/2023/liste_lieux_de_mission_2023_03_29_11_23_17.ods")
load("data/engagement/SC_data.RData")

#reprise des codes postaux erronés
reprise_CP <- function(.tbl,variable) {
  .tbl %>%
    mutate(CP = as.character({{variable}})) %>%
    mutate(Code_postal=case_when(
      nchar(CP)==4 ~ paste0("0",CP),
      "21000"<CP & CP <"21100" ~ "21000",
      "21200"<CP & CP <"21210" ~ "21200",
      "21300"<CP & CP <"21310" ~ "21300",
      "21400"<CP & CP <"21410" ~ "21400",
      "21500"<CP & CP <"21510" ~ "21500",
      "21600"<CP & CP <"21610" ~ "21600",
      "21800"<CP & CP <"21810" ~ "21800",
      
      "25000"<CP & CP <"25100" ~ "25000",
      "25110"<CP & CP <"25120" ~ "25110",
      "25200"<CP & CP <"25210" ~ "25200",
      "25210"<CP & CP <"25220" ~ "25210",
      "25300"<CP & CP <"25310" ~ "25300",
      "25400"<CP & CP <"25410" ~ "25400",
      "25460"<CP & CP <"25470" ~ "25460",
      "25600"<CP & CP <"25610" ~ "25600",
      "25700"<CP & CP <"25710" ~ "25700",
      
      "39000"<CP & CP <"39100" ~ "39000",
      "39100"<CP & CP <"39110" ~ "39100",
      "39200"<CP & CP <"39210" ~ "39200",
      "39600"<CP & CP <"39610" ~ "39600",
      "39800"<CP & CP <"39810" ~ "39800",
      
      "58000"<CP & CP <"58100" ~ "58000",
      "58200"<CP & CP <"58210" ~ "58200",
      "58400"<CP & CP <"58410" ~ "58400",
      
      "70000"<CP & CP <"70100" ~ "70000",
      "70100"<CP & CP <"70110" ~ "70100",
      "70200"<CP & CP <"70210" ~ "70200",
      "70300"<CP & CP <"70310" ~ "70300",
      
      "71000"<CP & CP <"71100" ~ "71000",
      "71100"<CP & CP <"71110" ~ "71100",
      "71200"<CP & CP <"71210" ~ "71200",
      "71300"<CP & CP <"71310" ~ "71300",
      "71320"<CP & CP <"71330" ~ "71320",
      "71330"<CP & CP <"71340" ~ "71330",
      "71400"<CP & CP <"71410" ~ "71400",
      "71500"<CP & CP <"71510" ~ "71500",
      
      "89000"<CP & CP <"89100" ~ "89000",
      "89100"<CP & CP <"89110" ~ "89100",
      "89300"<CP & CP <"89310" ~ "89300",
      
      "90000"<CP & CP <"90100" ~ "90000",
      "90100"<CP & CP <"90110" ~ "90100",
      
      .default = CP) ) }


mission2023 <- mission %>%
  reprise_CP(variable = CODE_POSTAL_CEDEX_LIEU1) %>%
  select(CTV_NUMERO,CTV_DATE_DEBUT,
         COO_NUMERO_VOIE_LIEU1,EXTENTION_VOIE_LIBELLE_LIEU1,TYPE_VOIE_LIBELLE_LIEU1,
         COO_LIBELLE_VOIE_LIEU1,COO_CPLT_ADRESSE_LIEU1, CODE_POSTAL_CEDEX_LIEU1,Code_postal_mis=Code_postal, VILLE_OU_COMMUNE_LIEU1, PAYS_LIEU1) %>%
  mutate_if(is.integer,as.character)  %>%
  mutate(adresse_mis = toupper( glue(" {str_replace_na(COO_NUMERO_VOIE_LIEU1,'')} {str_replace_na(EXTENTION_VOIE_LIBELLE_LIEU1,'')}
  {str_replace_na(TYPE_VOIE_LIBELLE_LIEU1,'')} {str_replace_na(COO_LIBELLE_VOIE_LIEU1,'')}") ) )


volontaire <- base2023 %>%
  reprise_CP(variable = CODEPOSTALCEDEX_VOL) %>%
  select(CTV_NUMERO,CTV_DATE_DEBUT,
         COO_NUMERO_VOIE_VOL,LIBELLE_VOL,COO_LIBELLE_VOIE_VOL,COO_CPLT_ADRESSE_VOL, CODEPOSTALCEDEX_VOL,Code_postal_vol=Code_postal,COMMUNE_VOL) %>%
  mutate_if(is.integer,as.character)  %>%
  mutate(adresse_vol=toupper( glue(" {str_replace_na(COO_NUMERO_VOIE_VOL,'')} {str_replace_na(LIBELLE_VOL,'')}
         {str_replace_na(COO_LIBELLE_VOIE_VOL,'')}  ") ) )      


#save(base2023,mission,file="data/engagement/SC_data.RData")

#on va maintenant géolocaliser les adresses et les lieux de missions 1 (uniquement)
library(banR)
#library(photon)



#premier passage sur les adresses
vol2023 <- volontaire %>% geocode_tbl(adresse_vol,code_postal = Code_postal_vol) 
#on isole les adresses en erreur et on indique le complément d'adresse (erreur de saisie)
vol_geo <- vol2023 %>% 
  filter(result_status!="not-found") %>%
  bind_rows(
    vol2023 %>% 
      filter(result_status=="not-found") %>% 
      select(-c(11:28)) %>%
      mutate(adresse_vol=toupper( glue(" {str_replace_na(COO_NUMERO_VOIE_VOL,'')} {str_replace_na(LIBELLE_VOL,'')}
         {str_replace_na(COO_CPLT_ADRESSE_VOL,'')}  ") ) ) %>%
      geocode_tbl(adresse_vol,code_postal = Code_postal_vol) ) %>%
  select(CTV_NUMERO,CTV_DATE_DEBUT,départ=result_citycode)


miss2023 <- mission2023 %>% 
  filter(!is.na(Code_postal_mis)) %>%
  geocode_tbl(adresse_mis,code_postal = Code_postal_mis)
#on isole les adresses en erreur et on indique le complément d'adresse (erreur de saisie)
miss23 <- miss2023 %>%
  filter(!result_status %in% c("skipped","not-found")) %>%
  bind_rows(
    miss2023 %>% 
      filter(result_status %in% c("skipped","not-found")) %>% 
      select(-c(13:30)) %>%
      mutate(adresse_mis=toupper( glue(" {str_replace_na(COO_CPLT_ADRESSE_LIEU1,'')}  ") ) ) %>%
      geocode_tbl(adresse_mis,code_postal = Code_postal_mis) ) 
  
table(miss2023$result_status)
table(miss23$result_status)

miss23b <- miss23 %>%
  filter(result_status %in% c("skipped","not-found")) %>% 
  select(-c(13:30)) %>%
  mutate(skipped=glue("{Code_postal_mis} {VILLE_OU_COMMUNE_LIEU1} {PAYS_LIEU1}")) %>%
  banR::geocode_tbl(skipped)
  
miss_geo <- miss23 %>% 
  filter(!result_status %in% c("skipped","not-found")) %>% 
  bind_rows(miss23b) %>%
  select(CTV_NUMERO,CTV_DATE_DEBUT,arrivée=result_citycode)
  

#on a maintenant nos lieux de départs et d'arrivée _geo

liens <- vol_geo %>% 
  left_join(.,miss_geo,by=c("CTV_NUMERO","CTV_DATE_DEBUT")) %>% 
  group_by(départ,arrivée) %>% # ajouter CTV_DATE_DEBUT ou filtrer sur une année
  count() %>%
  rename(nombre=n)


load("data/demo/cartes.RData")
library(geosphere)
library(rgdal)

cartexy <- centroid(com27wgs)
cartexy <- cbind(com27wgs,cartexy)

#on ajoute les coordonnées des centroides de communes
oursinxy <- left_join(liens,cartexy@data %>% 
                        select(INSEE_COM,origine=NOM,origine.x=X1,origine.y=X2),
                      by=c("départ"="INSEE_COM"))
oursinxy <- left_join(oursinxy,cartexy@data %>% 
                        select(INSEE_COM,destination=NOM,destination.x=X1,destination.y=X2),
                      by=c("arrivée"="INSEE_COM")) %>%
  filter(!is.na(origine.x) & !is.na(destination.x)) 

oursinxy <- oursinxy %>%  
  group_by(destination) %>%
  mutate(missions=sum(nombre),
         rayon=missions*10)


save(oursinxy,file="data/engagement/flux.RData")


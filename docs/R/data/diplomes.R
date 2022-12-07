library(rio)
library(data.table)
library(rgdal)
library(readxl)
library(janitor)
library(tidyverse)

diplomes <- read_excel("data/PanoFrance2020.xlsx",range = ("A983:DO999") )
diplreg <- diplomes %>% select(1,2,15,24,29,36,39,50,56,65,71,84,98,104,111) %>% slice (4:16) %>%
  mutate_at(vars(2:15), as.numeric) %>%
  adorn_totals("row",name="total") 
#diplreg <-  mutate_all(diplreg,~replace(.,is.na(.),"") )

bafareg <- diplomes %>% select(1,2,15,24,29,36,39,50,56,65,71,84,98,104,111) %>% slice (1:3) %>%
  mutate_at(vars(2:15), as.numeric) %>%
  adorn_totals("row",name="total") 
#bafareg <-  mutate_all(bafareg,~replace(.,is.na(.),"") )

bafa <- diplomes %>% select(1,15:23,111) %>% slice(2,3) %>% mutate(2,as.numeric) %>% mutate(11,as.numeric)


forome <- read.csv2("I:/SUPPORT/04_STATS/Formation/2020/EditionsNominatives - Diplomes.csv",as.is = T)

IDJEPS <- import("I:/SUPPORT/04_STATS/Droef/IDJEPS/2022/résultat enquête/Base_nationale_recodee_redressee pour envoi déf/Base_nationale_recodee_redressee pour envoi revu PN BD V3.xlsx")

## Recodage de IDJEPS$DIPLOME1 en IDJEPS$DIPLOME1_rec
IDJEPS$DIPLOME1_rec <- IDJEPS$DIPLOME1 %>%
  fct_recode(
    "BPJEPS" = "BPJEPS 10 UC",
    "BPJEPS" = "BPJEPS 4 UC"
  )

IDJEPSg <- IDJEPS %>%
  filter(!REGION %in% c("Guadeloupe","Martinique")) %>%
  group_by(REGION, SPECIALITE_D1,DIPLOME1_rec, FormApp_r,LienEmploiDiplome_r,SituationPrincipale_r) %>%
  count()


  save(diplreg,bafa,bafareg,forome,IDJEPS,IDJEPSg, file="data/formation/diplome.RData")

  
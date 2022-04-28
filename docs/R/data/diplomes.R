library(rio)
library(data.table)
library(rgdal)
library(readxl)
library(janitor)
library(tidyverse)

diplomes <- read_excel("C:/Users/plebre/Documents/projets R/DRAJES/data/PanoFrance2020.xlsx",range = ("A983:DO999") )
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

  save(diplreg,bafa,bafareg,forome,file="C:/Users/plebre/Documents/projets R/DRAJES/data/formation/diplome.RData")
  
library(tidyverse)
library(readxl)


clubs <- read.csv2("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/2. Données data.gouv.fr/2019/clubs-data-2019.csv",as.is = T)
load("data/demo/basecom.RData")

club_dep <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/Clubs-2021.xlsx",sheet = 4,skip = 2)
levels(club_dep) <-  club_dep[1,]
club_reg <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/Clubs-2021.xlsx",sheet = 3,skip = 2)
levels(club_reg) <-  club_reg[1,]
club_fede <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/Clubs-2021.xlsx",sheet = 2,skip = 2)
levels(club_reg) <-  club_reg[1,]

club_reg <- club_reg %>% slice (2:120) %>% dplyr::select(-c(3,17:21,23:24)) %>% mutate_at(3:16,as.numeric)

club_dep_bfc <- club_dep %>% slice (2:120) %>%
  dplyr::select(1:2,"21","25","39","58","70","71","89","90",France="...114") %>%
  bind_cols(BFC=club_reg$`27`) %>%
  relocate(BFC,.before=France) %>% mutate_at(3:12,as.numeric)


save(club_dep_bfc, club_dep,club_reg,club_fede,
     file="data/sport/clubs.RData")

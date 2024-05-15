#source("librairies.R")
#construction de la base communale démographie
library(readxl)
library(tidyverse)
passage <- read_excel("data/insee/table_passage_annuelle_2024.xlsx",sheet=1,skip=5)  #https://www.insee.fr/fr/information/7671867
surface <- read_excel("data/insee/base_cc_comparateur.xlsx",sheet=1,skip=5)  #https://www.insee.fr/fr/statistiques/2521169
communes <- read_excel("data/insee/table-appartenance-geo-communes-2024.xlsx",sheet=1,skip=5) #https://www.insee.fr/fr/information/7671844
appartenance <- read_excel("data/insee/table-appartenance-geo-communes-2024.xlsx",sheet=3,skip=5)
BV2022_a <- read_excel("data/insee/BV2022_au_01-01-2024.xlsx",skip = 5,sheet = 1) #https://www.insee.fr/fr/information/6676988
BV2022 <- read_excel("data/insee/BV2022_au_01-01-2024.xlsx",skip = 5,sheet = 2)


appartenance <- rbind(appartenance, 
                      c("DEP","BFC","Bourgogne-Franche-Comté","-"),
                      c("REG","FR","France","-"),
                      c("REG","METRO","France métropolitaine","-") )

appartenance <- appartenance %>%
  bind_rows(BV2022_a %>% 
              dplyr::mutate (NIVGEO="BV2022",NB_COM=as.character(NB_COM)) %>%
              dplyr::select (NIVGEO,CODGEO="BV2022",LIBGEO=LIBBV2022,NB_COM)
  )

#passage$CODGEO_2014[passage$CODGEO_2021 =="55138"] <- "55138" corrigé en 2024


basecom_temp <- communes %>% 
  filter(REG>"10") %>%
  left_join(.,surface %>% 
              select(CODGEO,SUPERF),
            by="CODGEO") 

pop_new <- read_csv2("data/insee/TD_POP1B_2020.csv")  #https://www.insee.fr/fr/statistiques/7631680?sommaire=7632456#consulter

save(pop_new,file = "data/demo/RPage.RData")



pop_new <- pop_new %>% 
  mutate (NB = as.numeric(NB)) %>%
  group_by(CODGEO) %>%
  summarise(pop=sum(NB,na.rm = T),
            poph=sum(NB[SEXE==1],na.rm = T),
            popf=sum(NB[SEXE==2],na.rm = T),
            p1529=sum(NB[AGED100 > "014" & AGED100 < "030"],na.rm = T),
            p1625=sum(NB[AGED100 > "015" & AGED100 < "026"],na.rm = T),
            p617=sum(NB[AGED100 > "005" & AGED100 < "018"],na.rm = T),
            p1825=sum(NB[AGED100 > "017" & AGED100 < "026"],na.rm = T),
            p20=sum(NB[AGED100<"020"],na.rm = T),
            p60=sum(NB[AGED100>"060"],na.rm = T),
            p65=sum(NB[AGED100>"064"],na.rm = T),
            p75=sum(NB[AGED100>"075"],na.rm = T),
            #p1529h=sum(NB[SEXE==1 & AGED100 > "014" & AGED100 < "030"],na.rm = T),
            p1529f=sum(NB[SEXE==2 & AGED100 > "014" & AGED100 < "030"],na.rm = T),
            #p1625h=sum(NB[SEXE==1 & AGED100 > "015" & AGED100 < "026"],na.rm = T),
            p1625f=sum(NB[SEXE==2 & AGED100 > "015" & AGED100 < "026"],na.rm = T),
            #p65h=sum(NB[SEXE==1 & AGED100>"064"],na.rm = T),
            p65f=sum(NB[SEXE==2 & AGED100>"064"],na.rm = T),
            p20h=sum(NB[SEXE==1 & AGED100<"020"],na.rm = T),
            p60h=sum(NB[SEXE==1 & AGED100>"060"],na.rm = T),
            p20f=sum(NB[SEXE==2 & AGED100<"020"],na.rm = T),
            p60f=sum(NB[SEXE==2 & AGED100>"060"],na.rm = T),
  )

pop_ante14 <- read.csv2("data/insee/BTT_TD_POP1B_2014.txt", header=T,sep=";")


pop_ante <- pop_ante14 %>% 
  mutate(NB=as.numeric(NB)) %>%
  left_join(.,passage %>% 
              distinct(CODGEO_2016,CODGEO_2024) ,
            by=c("CODGEO" = "CODGEO_2016") ) %>%
  group_by(CODGEO=CODGEO_2024) %>%
  summarise(pop=sum(NB,na.rm = T),
            poph=sum(NB[SEXE==1],na.rm = T),
            popf=sum(NB[SEXE==2],na.rm = T),
            p1529=sum(NB[AGED100 > 14 & AGED100 < 30],na.rm = T),
            p1625=sum(NB[AGED100 > 15 & AGED100 < 26],na.rm = T),
            p20=sum(NB[AGED100<20],na.rm = T),
            p60=sum(NB[AGED100>60],na.rm = T),
            p65=sum(NB[AGED100>64],na.rm = T),
            p75=sum(NB[AGED100>75],na.rm = T),
            #p1529h=sum(NB[SEXE==1 & AGED100 > "014" & AGED100 < "030"],na.rm = T),
            p1529f=sum(NB[SEXE==2 & AGED100 > 14 & AGED100 < 30],na.rm = T),
            #p1625h=sum(NB[SEXE==1 & AGED100 > "015" & AGED100 < "026"],na.rm = T),
            p1625f=sum(NB[SEXE==2 & AGED100 > 15 & AGED100 < 26],na.rm = T),
            #p65h=sum(NB[SEXE==1 & AGED100>"064"],na.rm = T),
            p65f=sum(NB[SEXE==2 & AGED100>64],na.rm = T) )


basecom_temp <- basecom_temp %>%
  left_join(.,pop_new,by="CODGEO" ,suffix=c("","_new")) %>%
  left_join(.,pop_ante,by=c("CODGEO"),suffix=c("","_ante") ) 

ZRR <- read_excel("data/data.gouv/diffusion-zonages-zrr-cog2021.xls",sheet = 1,skip=5) #https://www.data.gouv.fr/fr/datasets/zones-de-revitalisation-rurale-zrr/
ZRR <-  ZRR %>% 
  select(CODGEO,ZRR_SIMP) %>% 
  mutate(cZRR=str_split_fixed(ZRR_SIMP,"-",2)[,1] ) %>% 
  left_join(.,passage %>% 
              select(CODGEO_2021,CODGEO_2024) %>% 
              distinct(CODGEO_2021,.keep_all = T) ,
            by=c("CODGEO" = "CODGEO_2021") ) 

basecom <- basecom_temp %>% 
  left_join(.,ZRR %>% 
              select(CODGEO_2024,cZRR),
            by=c("CODGEO"="CODGEO_2024")) %>%
  mutate(popZRR=if_else(cZRR=="NC ",0,pop)) %>%
  select(-cZRR)



library(dplyr)
QPV <- read_excel("data/insee/pop_legales_communales_qpv_2018.xlsx",sheet = 1,skip=7)  #https://www.insee.fr/fr/statistiques/5428847?sommaire=2500477

basecomQPV <- basecom %>% 
  left_join(.,QPV %>% 
              select(CODGEO=codeDepcom,popMuniQPV),by="CODGEO")


basecomQPV$popMuniQPV[is.na(basecomQPV$popMuniQPV)] <- 0
#basecomQPV$POP_COM <- if_else(basecomQPV$POP_MUN==0,basecomQPV$pop,basecomQPV$POP_COM)

basecom <- basecomQPV %>% 
  left_join(.,BV2022 %>%
              dplyr::select(CODGEO,BV2022,LIBBV2022),
            by="CODGEO")


save(basecomQPV,basecom,appartenance,file="data/demo/basecom.RData")








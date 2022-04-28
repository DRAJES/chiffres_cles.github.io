source("C:/Users/plebre/Documents/projets R/DRAJES/librairies.R")
#construction de la base communale démographie
library(readxl)
passage <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table_passage_annuelle_2021.xlsx",sheet=1,skip=5)
surface <- read.dbf("I:/SUPPORT/05_CARTO/Fonds de cartes/IGN/COMMUNE GEOFLA2016/COMMUNE.dbf")
communes <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table-appartenance-geo-communes-21.xlsx",sheet=1,skip=5)
appartenance <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table-appartenance-geo-communes-21.xlsx",sheet=3,skip=5)

passage$CODGEO_2014[passage$CODGEO_2021 =="55138"] <- "55138"

#communes <- communes %>%  
#  left_join(.,passage %>% select(CODGEO_2020,CODGEO_2021) %>% 
#             distinct(CODGEO_2020,.keep_all = T),by=c("CODGEO" = "CODGEO_2020") ) %>% 
#  distinct(CODGEO_2021,.keep_all = T)

basecom_temp <- surface %>% filter(as.character( CODE_REG ) >"10") %>%
    select (INSEE_COM,SUPERFICIE) %>%   
    mutate (INSEE_COM=as.character(INSEE_COM)) %>%
    mutate (INSEE_COM = case_when(
      INSEE_COM>"75100" & INSEE_COM<"75200" ~ "75056",
      INSEE_COM>"13200" & INSEE_COM<"13300" ~ "13055",
      INSEE_COM>"69300" & INSEE_COM<"69400" ~ "69123",
      TRUE ~ INSEE_COM) ) %>%
    left_join(.,passage %>% distinct(CODGEO_2016,.keep_all = T),by=c("INSEE_COM" = "CODGEO_2016") ) %>%
    group_by(CODGEO_2021) %>%
    summarise(SUPERFICIE=sum(SUPERFICIE)) %>%
  left_join(communes %>% filter(REG>"10") 
            ,.,by=c("CODGEO" ="CODGEO_2021") )

pop_new <- read_excel("I:/SUPPORT/04_STATS/Sources/INSEE/RP/RP2018/BTX_TD_POP1B_2018.xlsx",sheet = 1,skip = 10)
pop_new <- pop_new %>%
          transmute(CODGEO,
                    poph=rowSums(pop_new[,c(3:103)]),
                    popf=rowSums(pop_new[,c(104:204)]),
                    p1529h=rowSums(pop_new[,c(18:32)]),
                    p1529f=rowSums(pop_new[,c(119:133)]),
                    p1625h=rowSums(pop_new[,c(19:28)]),
                    p1625f=rowSums(pop_new[,c(118:129)]),
                    p65h=rowSums(pop_new[,c(68:103)]),
                    p65f=rowSums(pop_new[,c(169:204)]),
                    p75h=rowSums(pop_new[,c(78:103)]),
                    p75f=rowSums(pop_new[,c(179:204)]),
                    p20=rowSums(pop_new[,c(3:22)])+rowSums(pop_new[,c(104:123)]),
                    p60=rowSums(pop_new[,c(63:103)])+rowSums(pop_new[,c(164:204)]),
                    pop=poph+popf,
                 p1529=p1529h+p1529f,
                 p1625=p1625h+p1625f,
                 p65=p65h+p65f,
                 p75=p75h+p75f     )  %>% 
 # left_join(.,passage %>% distinct(CODGEO_2020,.keep_all = T) ,by=c("CODGEO" = "CODGEO_2020") ) %>%
  group_by(CODGEO) %>%
  summarise_if(is.numeric,sum) %>%mutate(DEP=substr(CODGEO,1,2))

sum(pop_new$pop[pop_new$DEP=="21"])
sum(pop_new$pop[pop_new$DEP!="97"],na.rm=T)


pop_ante <- read_excel("I:/SUPPORT/04_STATS/Sources/INSEE/RP/RP2013/BTX_TD_POP1B_2013.xls",sheet = 1,skip = 10)
pop_ante <- pop_ante %>%
  transmute(CODGEO,
            poph=rowSums(pop_ante[,c(3:103)]),
            popf=rowSums(pop_ante[,c(104:204)]),
            p1529h=rowSums(pop_ante[,c(18:32)]),
            p1529f=rowSums(pop_ante[,c(119:133)]),
            p1625h=rowSums(pop_ante[,c(19:28)]),
            p1625f=rowSums(pop_ante[,c(118:129)]),
            p65h=rowSums(pop_ante[,c(68:103)]),
            p65f=rowSums(pop_ante[,c(169:204)]),
            p75h=rowSums(pop_ante[,c(78:103)]),
            p75f=rowSums(pop_ante[,c(179:204)]),
            p20=rowSums(pop_ante[,c(3:22)])+rowSums(pop_ante[,c(104:123)]),
            p60=rowSums(pop_ante[,c(63:103)])+rowSums(pop_ante[,c(164:204)]),
            pop=poph+popf,
            p1529=p1529h+p1529f,
            p1625=p1625h+p1625f,
            p65=p65h+p65f,
            p75=p75h+p75f     )  %>%
  left_join(.,passage %>% distinct(CODGEO_2015,.keep_all = T) ,by=c("CODGEO" = "CODGEO_2015") ) %>%
  group_by(CODGEO_2021) %>%
  summarise_if(is.numeric,sum) 

basecom_temp <- basecom_temp %>%
  left_join(.,pop_new,by="CODGEO" ,suffix=c("","_new")) %>%
  left_join(.,pop_ante,by=c("CODGEO"="CODGEO_2021"),suffix=c("","_ante") ) 

ZRR <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/ZRR/Diffusion-zonages-ZRR-2020_0.xls",sheet = 1,skip=5)
ZRR <-  ZRR %>% select(CODGEO,ZRR_SIMP) %>% 
  mutate(cZRR=str_split_fixed(ZRR_SIMP,"-",2)[,1] ) %>% 
left_join(.,passage %>% 
            select(CODGEO_2020,CODGEO_2021) %>% 
            distinct(CODGEO_2020,.keep_all = T) ,
            by=c("CODGEO" = "CODGEO_2020") ) 

basecom <- basecom_temp %>% left_join(.,ZRR %>% select(CODGEO_2021,cZRR),by=c("CODGEO"="CODGEO_2021")) %>%
            mutate(popZRR=if_else(cZRR=="NC ",0,pop)) %>%
            select(-cZRR)



library(dplyr)
QPV <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/DEMO_2020_V1.xlsx",sheet = 1,skip=5)
QPV_COM <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/DEMO_2020_V1.xlsx",sheet = 3,skip=5)
QPV_ref <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/DEMO_2020_V1.xlsx",sheet = 7,skip=5)

QPV2 <- QPV %>% select(CODGEO,LIBGEO,POP_MUN,POP_COM,LIB_COM) %>%
  left_join(.,QPV_ref %>% select(QP,ZONE_EPCI_2019_1,ZONE_EPCI_2019_2)
            ,by=c("CODGEO" = "QP") ) %>% 
  mutate (ZONE_EPCI_2019_1 = case_when(
    ZONE_EPCI_2019_1=="200033207" ~ "200090561",
    ZONE_EPCI_2019_1=="200035665" ~ "200089456",
    ZONE_EPCI_2019_1=="246201149" ~ "200090751",
    ZONE_EPCI_2019_1=="200077055" ~ "200090504",
    str_detect(ZONE_EPCI_2019_1,"ZZZ")  ~ "200054781",
    TRUE ~ ZONE_EPCI_2019_1) ) %>%
  left_join(., basecom %>% select(CODGEO,EPCI,pop) %>% group_by(EPCI) %>% dplyr::filter (pop==max(pop,na.rm=T)),
            by=c("ZONE_EPCI_2019_1"="EPCI") ) %>%
  group_by(CODGEO.y) %>% summarise_if(is.numeric,~sum(.x,na.rm = T))




basecomQPV <- basecom %>% left_join(.,QPV2 %>% select(-pop),by=c("CODGEO"="CODGEO.y"))


basecomQPV$POP_MUN[is.na(basecomQPV$POP_MUN)] <- 0
basecomQPV$POP_COM <- if_else(basecomQPV$POP_MUN==0,basecomQPV$pop,basecomQPV$POP_COM)


save(basecomQPV,basecom,appartenance,communes,file="C:/Users/plebre/Documents/projets R/DRAJES/data/demo/basecom.RData")









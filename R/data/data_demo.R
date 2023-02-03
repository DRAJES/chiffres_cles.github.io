source("librairies.R")
#construction de la base communale démographie
library(readxl)
passage <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2022/table_passage_annuelle_2022.xlsx",sheet=1,skip=5)
surface <- read.csv2("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2022/base_cc_comparateur.csv")
communes <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2022/table-appartenance-geo-communes-22.xlsx",sheet=1,skip=5)
appartenance <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2022/table-appartenance-geo-communes-22.xlsx",sheet=3,skip=5)
BV2022_a <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/bv/BV2022/BV2022_au_01-01-2022.xlsx",skip = 5,sheet = 1)
BV2022 <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/bv/BV2022/BV2022_au_01-01-2022.xlsx",skip = 5,sheet = 2)


appartenance <- rbind(appartenance, 
                      c("DEP","BFC","Bourgogne-Franche-Comté","-"),
                      c("REG","FR","France","-"),
                      c("REG","METRO","France métropolitaine","-") )

appartenance <- appartenance %>%
  bind_rows(BV2022_a %>% 
              dplyr::mutate (NIVGEO="BV2022",NB_COM=as.character(NB_COM)) %>%
              dplyr::select (NIVGEO,CODGEO="BV2022",LIBGEO=LIBBV2022,NB_COM)
            )

passage$CODGEO_2014[passage$CODGEO_2021 =="55138"] <- "55138"


basecom_temp <- communes %>% filter(REG>"10") %>%
  left_join(.,surface %>% select(1:4),by="CODGEO") %>% mutate(SUPERF=as.numeric(SUPERF))
                




#pop_new <- read_excel("I:/SUPPORT/04_STATS/Sources/INSEE/RP/RP2018/BTX_TD_POP1B_2018.xlsx",sheet = 1,skip = 10)
pop_new <- read_csv2("I:/SUPPORT/04_STATS/Sources/INSEE/RP/RP2019/BTT_TD_POP1B_2019.csv")

#op_new <- pop_new %>% 
#         transmute(CODGEO,
#                   poph=rowSums(pop_new[,c(3:103)]),
#                   popf=rowSums(pop_new[,c(104:204)]),
#                   p1529h=rowSums(pop_new[,c(18:32)]),
#                   p1529f=rowSums(pop_new[,c(119:133)]),
#                   p1625h=rowSums(pop_new[,c(19:28)]),
#                   p1625f=rowSums(pop_new[,c(118:129)]),
#                   p65h=rowSums(pop_new[,c(68:103)]),
#                   p65f=rowSums(pop_new[,c(169:204)]),
#                   p75h=rowSums(pop_new[,c(78:103)]),
#                   p75f=rowSums(pop_new[,c(179:204)]),
#                   p20=rowSums(pop_new[,c(3:22)])+rowSums(pop_new[,c(104:123)]),
#                   p60=rowSums(pop_new[,c(63:103)])+rowSums(pop_new[,c(164:204)]),
#                   pop=poph+popf,
#                p1529=p1529h+p1529f,
#                p1625=p1625h+p1625f,
#                p65=p65h+p65f,
#                p75=p75h+p75f     )  %>% 
## left_join(.,passage %>% distinct(CODGEO_2020,.keep_all = T) ,by=c("CODGEO" = "CODGEO_2020") ) %>%
# group_by(CODGEO) %>%
# summarise_if(is.numeric,sum) %>%mutate(DEP=substr(CODGEO,1,2))

pop_new <- pop_new %>% 
              mutate (NB = as.numeric(NB)) %>%
              group_by(CODGEO) %>%
              summarise(pop=sum(NB,na.rm = T),
                        poph=sum(NB[SEXE==1],na.rm = T),
                        popf=sum(NB[SEXE==2],na.rm = T),
                        p1529=sum(NB[AGED100 > "014" & AGED100 < "030"],na.rm = T),
                        p1625=sum(NB[AGED100 > "015" & AGED100 < "026"],na.rm = T),
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
                       

  

sum(pop_new$pop[pop_new$DEP=="21"])
sum(pop_new$pop[pop_new$DEP!="97"],na.rm=T)


#pop_ante <- read_excel("I:/SUPPORT/04_STATS/Sources/INSEE/RP/RP2013/BTX_TD_POP1B_2013.xls",sheet = 1,skip = 10)
pop_ante13 <- read.csv2("I:/SUPPORT/04_STATS/Sources/INSEE/RP/RP2013/BTT_TD_POP1B_2013.txt", header=T,sep=";")

#pop_ante <- pop_ante %>%
#  transmute(CODGEO,
#            poph=rowSums(pop_ante[,c(3:103)]),
#            popf=rowSums(pop_ante[,c(104:204)]),
#            p1529h=rowSums(pop_ante[,c(18:32)]),
#            p1529f=rowSums(pop_ante[,c(119:133)]),
#            p1625h=rowSums(pop_ante[,c(19:28)]),
#            p1625f=rowSums(pop_ante[,c(118:129)]),
#            p65h=rowSums(pop_ante[,c(68:103)]),
#            p65f=rowSums(pop_ante[,c(169:204)]),
#            p75h=rowSums(pop_ante[,c(78:103)]),
#            p75f=rowSums(pop_ante[,c(179:204)]),
#            p20=rowSums(pop_ante[,c(3:22)])+rowSums(pop_ante[,c(104:123)]),
#            p60=rowSums(pop_ante[,c(63:103)])+rowSums(pop_ante[,c(164:204)]),
#            pop=poph+popf,
#            p1529=p1529h+p1529f,
#            p1625=p1625h+p1625f,
#            p65=p65h+p65f,
#            p75=p75h+p75f     )  %>%
#  left_join(.,passage %>% distinct(CODGEO_2015,.keep_all = T) ,by=c("CODGEO" = "CODGEO_2015") ) %>%
#  group_by(CODGEO_2021) %>%
#  summarise_if(is.numeric,sum) 

pop_ante <- pop_ante13 %>% 
              mutate(NB=as.numeric(NB)) %>%
              left_join(.,passage %>% distinct(CODGEO_2015,CODGEO_2022) ,by=c("CODGEO" = "CODGEO_2015") ) %>%
              group_by(CODGEO=CODGEO_2022) %>%
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
QPV <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/pop_legales_communales_qpv_2018.xlsx",sheet = 1,skip=7)

basecomQPV <- basecom %>% left_join(.,QPV %>% select(CODGEO=codeDepcom,popMuniQPV),by="CODGEO")


basecomQPV$popMuniQPV[is.na(basecomQPV$popMuniQPV)] <- 0
#basecomQPV$POP_COM <- if_else(basecomQPV$POP_MUN==0,basecomQPV$pop,basecomQPV$POP_COM)

basecom <- basecomQPV %>% left_join(.,BV2022 %>%
                                      dplyr::select(CODGEO,BV2022,LIBBV2022),by="CODGEO")


save(basecomQPV,basecom,appartenance,file="data/demo/basecom.RData")








load("data/demo/basecom.RData")
library(dplyr)
QPV <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/pop_legales_communales_qpv_2018.xlsx",sheet = 1,skip=7)
#QPV_COM <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/DEMO_2020_V1.xlsx",sheet = 3,skip=5)
#QPV_ref <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/QPV/DEMO_2020_V1.xlsx",sheet = 7,skip=5)

#QPV2 <- QPV %>% select(CODGEO,LIBGEO,POP_MUN,POP_COM,LIB_COM) %>%
#        left_join(.,QPV_ref %>% select(QP,ZONE_EPCI_2019_1,ZONE_EPCI_2019_2)
#                    ,by=c("CODGEO" = "QP") ) %>% 
#      mutate (ZONE_EPCI_2019_1 = case_when(
#        ZONE_EPCI_2019_1=="200033207" ~ "200090561",
#        ZONE_EPCI_2019_1=="200035665" ~ "200089456",
#        ZONE_EPCI_2019_1=="246201149" ~ "200090751",
#        ZONE_EPCI_2019_1=="200077055" ~ "200090504",
#        str_detect(ZONE_EPCI_2019_1,"ZZZ")  ~ "200054781",
#        TRUE ~ ZONE_EPCI_2019_1) ) %>%
#        left_join(., basecom %>% select(CODGEO,EPCI,pop) %>% group_by(EPCI) %>% dplyr::filter (pop==max(pop,na.rm=T)),
#                by=c("ZONE_EPCI_2019_1"="EPCI") ) %>%
#        group_by(CODGEO) %>% summarise_if(is.numeric,~sum(.x,na.rm = T))
#



basecomQPV <- basecom %>% left_join(.,QPV %>% select(CODGEO=codeDepcom,popMuniQPV),by="CODGEO")

save(appartenance,basecom,basecomQPV,file = "data/demo/basecom.RData")

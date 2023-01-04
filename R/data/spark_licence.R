library(readxl)
library(janitor)
library(sparkline)
sparkline(0)
lic10 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesrepdpt10.xls",sheet = 2,skip=3  )
lic11 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdpt11_.xls",sheet = 2,skip=1  )
lic12 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesrepdpt12_.xls",sheet = 2,skip=1  )
lic13 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdpt13.xls",sheet = 2,skip=3  )
lic14 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdpt14.xls",sheet = 2,skip=3  )
lic15 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdpt15.xls",sheet = 2,skip=2  )
lic16 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdep16.xlsx",sheet = 2,skip=2  )
lic17 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdep17.xlsx",sheet = 2,skip=2  )
lic18 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licencesdep18.xlsx",sheet = 2,skip=2 )
lic19 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/licences-par-departement-2019.xlsx",sheet = 2,skip=2  )
lic20 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2020/licences-par-departement-2020.xlsx",sheet = 2,skip=2  )
lic21 <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/sport/Recensement licences et clubs sportifs/1. Tableaux injep.fr/2021/licences-par-departement-2021.xlsx",sheet = 2,skip=2  )
depreg <- read.csv("I:/SUPPORT/05_CARTO/Outils et Documentation/departement2021.csv",as.is=T)
depreg[102,1:2] <- "FM"


evofede <- lic10  %>%  filter(...1>100 & ...1<700) %>% select(fed=1,l2010=158) %>% mutate(l2010=as.numeric(l2010)) %>%
            full_join(.,lic11 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2011=158) %>% mutate(l2011=as.numeric(l2011)), by="fed" )%>%
            full_join(.,lic12 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2012=136) %>% mutate(l2012=as.numeric(l2012)), by="fed" )%>%
            full_join(.,lic13 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2013=136) %>% mutate(l2013=as.numeric(l2013)), by="fed" )%>%
            full_join(.,lic14 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2014=136) %>% mutate(l2014=as.numeric(l2014)), by="fed" )%>%
            full_join(.,lic15 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2015=114) %>% mutate(l2015=as.numeric(l2015)), by="fed" )%>%
            full_join(.,lic16 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2016=114) %>% mutate(l2016=as.numeric(l2016)), by="fed" )%>%
            full_join(.,lic17 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2017=114) %>% mutate(l2017=as.numeric(l2017)), by="fed" )%>%
            full_join(.,lic18 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2018=114) %>% mutate(l2018=as.numeric(l2018)), by="fed" )%>%
            full_join(.,lic19 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2019=116) %>% mutate(l2019=as.numeric(l2019)), by="fed" )%>%
            full_join(.,lic20 %>% filter(...1>100 & ...1<700) %>% select(fed=1,l2020=116) %>% mutate(l2020=as.numeric(l2020)), by="fed" ) %>%
            full_join(.,lic21 %>% filter(...1>100 & ...1<700) %>% select(fed=1,fede=2,l2021=116) %>% mutate(l2021=as.numeric(l2021)), by="fed" ) %>%
            pivot_longer(!c(fed,fede), names_to = "année",values_to = "lic") %>% 
            group_by(fed) %>%
            arrange((année),.by_group=T) 

evofedebfc <- lic10  %>% filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2010=10) %>%
    full_join(.,lic11 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2011=10), by="fed" )%>%
    full_join(.,lic12 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2012=10), by="fed" )%>%
    full_join(.,lic13 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2013=10), by="fed" )%>%
    full_join(.,lic14 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2014=10), by="fed" )%>%
    full_join(.,lic15 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2015=10), by="fed" )%>%
    full_join(.,lic16 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2016=10), by="fed" )%>%
    full_join(.,lic17 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2017=10), by="fed" )%>%
    full_join(.,lic18 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2018=10), by="fed" )%>%
    full_join(.,lic19 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2019=10), by="fed" )%>%
    full_join(.,lic20 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col") %>% select(1,l2020=10), by="fed" )%>%
    full_join(.,lic21 %>%  filter(...1>100 & ...1<700) %>% select(fed=1,fede=2,depbfc) %>% mutate_at(3:10,as.numeric) %>% adorn_totals("col") %>% select(1,2,l2021=11), by="fed" )%>%
    pivot_longer(!c(fed,fede), names_to = "année",values_to = "lic") %>% 
    group_by(fed) %>%
    arrange((année),.by_group=T) 

evoreg <- lic10  %>% select (-c(144:157)) %>%  slice(34,94,119,121) %>%  mutate(année="2010",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...158) %>% pivot_longer(-c(145:146),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))  %>% 
  bind_rows(.,lic11 %>% slice(34,95,120,122) %>% select (-c(144:157)) %>% mutate(année="2011",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...158) %>% pivot_longer(-c(145:146),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic12 %>% slice(34,95,120,122) %>% select (-c(122:135)) %>% mutate(année="2012",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...136) %>% pivot_longer(-c(123:124),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic13 %>% slice(34,95,120,121)  %>% select (-c(122:135)) %>% mutate(année="2013",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...136) %>% pivot_longer(-c(123:124),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic14 %>% slice(34,94,119,120) %>% select (-c(122:135)) %>% mutate(année="2014",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...136) %>% pivot_longer(-c(123:124),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic15 %>% slice(34,94,119,120) %>% select (-c(100:113)) %>% mutate(année="2015",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...114) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic16 %>% slice(34,92,117,118) %>% select (-c(100:113)) %>% mutate(année="2016",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...114) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic17 %>% slice(34,92,117,118) %>% select (-c(100:113)) %>% mutate(année="2017",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...114) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic18 %>% slice(39,92,117,118) %>% select (-c(100:113)) %>% mutate(année="2018",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...114) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic19 %>% slice(39,92,117,118) %>% select (-c(100:115)) %>% mutate(année="2019",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...116) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic20 %>% slice(40,92,117,118) %>% select (-c(100:115)) %>% mutate(année="2020",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...116) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  bind_rows(.,lic21 %>% slice(40,92,117,118) %>% select (-c(100:115)) %>% mutate(année="2021",type=c("uni","nonolym","multi","total")) %>% rename("FM"=...116) %>% pivot_longer(-c(101:102),names_to = "DEP",values_to = "lic") %>% left_join(.,depreg[,1:2],by="DEP") %>% group_by(année,type, REG) %>% summarise(lic=sum(as.numeric(lic),na.rm=T))) %>%
  group_by(REG) %>% 
  arrange(REG, année)  %>% 
  filter(!is.na(REG))

evodep <- lic10  %>% slice(34,94,119,121) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2010",type=c("uni","nonolym","multi","total"))  %>%
  bind_rows(.,lic11 %>%  slice(34,95,120,122) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2011",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic12 %>%  slice(34,95,120,122) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2012",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic13 %>%  slice(34,95,120,121) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2013",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic14 %>%  slice(34,94,119,120) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2014",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic15 %>%  slice(34,94,119,120) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2015",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic16 %>%  slice(34,92,117,118) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2016",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic17 %>%  slice(34,92,117,118) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2017",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic18 %>%  slice(39,92,117,118) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2018",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic19 %>%  slice(39,92,117,118) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC") %>% mutate(année="2019",type=c("uni","nonolym","multi","total")) )%>%
  bind_rows(.,lic20 %>%  slice(40,92,117,118) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC")  %>% mutate(année="2020",type=c("uni","nonolym","multi","total")) ) %>%
  bind_rows(.,lic21 %>%  slice(40,92,117,118) %>% select(1,depbfc) %>% mutate_at(2:9,as.numeric) %>% adorn_totals("col",name="BFC")  %>% mutate(année="2021",type=c("uni","nonolym","multi","total")) ) %>%
  select(-1) %>%   
  pivot_longer(!c(année,type), names_to = "dep",values_to = "lic") %>% 
  group_by(dep) %>% 
  arrange(dep,année) 


save(evoreg,evodep,evofede,evofedebfc,
     file="data/sport/spark.RData")


formattable(serie_epci) %>%
  formattable::as.htmlwidget() %>%
  spk_add_deps()

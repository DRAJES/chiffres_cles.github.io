source("C:/Users/plebre/Documents/projets R/librairies.R")
passage <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table_passage_annuelle_2021.xlsx",sheet=1,skip=5)
communes <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table-appartenance-geo-communes-21.xlsx",sheet=1,skip=5)
appartenance <- read_excel("I:/SUPPORT/05_CARTO/Fonds de cartes/fonds insee 2021/table-appartenance-geo-communes-21.xlsx",sheet=3,skip=5)

popevol <- read.csv2("I:/SUPPORT/04_STATS/Sources/INSEE/RP/evolution/base-cc-serie-historique-2018.CSV",as.is = T)
popevol <- popevol %>% select(1:24)


popevol <- popevol %>%  
  left_join(.,communes,by=c("CODGEO"= "CODGEO_2021")) %>%
  filter(!is.na(DEP) & REG >"10")

library(sparkline)
sparkline(0)


#REGION
regevol <- popevol %>% 
  gather("année","pop", 2:9) %>% select(REG,année,pop) %>% 
   group_by(REG,année)  %>% summarise(pop=sum(pop)) %>%
  left_join(.,appartenance %>% filter (NIVGEO=="REG") ,by=c("REG"="CODGEO")) %>%
   group_by(REG,LIBGEO) %>%
    summarise ( evol = spk_chr(
    c(pop), type="line"
    #,chartRangeMin=0, chartRangeMax=max(popevol$pop)
    ) ,
    pop=last(pop) ) %>%

   formattable::formattable(big.mark = " ") %>%
     as.datatable() %>%    
     spk_add_deps() 
regevol

library(kableExtra)
kable(regevol, escape = F) %>%
  kable_styling(full_width = F) 


#DEPARTEMENT
popevol %>% 
  gather("année","pop", 2:9) %>% select(REG,DEP,année,pop) %>% 
  group_by(REG,DEP,année)  %>% summarise(pop=sum(pop)) %>%
  left_join(.,appartenance %>% filter (NIVGEO=="DEP") ,by=c("DEP"="CODGEO")) %>%
  group_by(REG,DEP,LIBGEO) %>%  summarise ( pop = spk_chr(
    pop, type="line"
    #,chartRangeMin=0, chartRangeMax=max(popevol$pop)
  ),
  pop=last(pop) ) %>%
  formattable::formattable() %>%
  formattable::as.htmlwidget() %>%
  spk_add_deps() 

library(kableExtra)
kable(poptot,"html", escape = F, caption = "evol") %>%
  kable_styling("hover",full_width = F) 



#EPCI
popevol %>% 
  gather("année","pop", 2:9) %>% select(EPCI,année,pop) %>% 
  group_by(EPCI,année)  %>% summarise(pop=sum(pop)) %>%
  left_join(.,appartenance %>% filter (NIVGEO=="EPCI") ,by=c("EPCI"="CODGEO")) %>%
  group_by(EPCI,LIBGEO) %>%  summarise ( pop = spk_chr(
    pop, type="line"
    #,chartRangeMin=0, chartRangeMax=max(popevol$pop)
  ),
  pop=last(pop) ) %>%
  formattable::formattable() %>%
  formattable::as.htmlwidget() %>%
  spk_add_deps() 

library(kableExtra)
kable(poptot,"html", escape = F, caption = "evol") %>%
  kable_styling("hover",full_width = F)


#BV
popevol %>% 
  gather("année","pop", 2:9) %>% select(BV2012,année,pop) %>% 
  group_by(BV2012,année)  %>% summarise(pop=sum(pop)) %>%
  left_join(.,appartenance %>% filter (NIVGEO=="BV2012") ,by=c("BV2012"="CODGEO")) %>%
  group_by(BV2012,LIBGEO) %>%  summarise ( pop = spk_chr(
    pop, type="line"
   # ,chartRangeMin=0, chartRangeMax=max(popevol$pop)
  ),
  pop=last(pop) ) %>%
  formattable::formattable() %>%
  formattable::as.htmlwidget() %>%
  spk_add_deps() 

library(kableExtra)
kable(poptot,"html", escape = F, caption = "evol") %>%
  kable_styling("hover",full_width = F)



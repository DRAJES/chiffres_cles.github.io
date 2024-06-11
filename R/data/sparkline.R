library(readxl)
library(sparkline)
sparkline(0)
load("data/demo/basecom.RData")

serie <- read.csv2 ("data/insee/base-cc-serie-historique-2020.CSV",as.is = T) #https://www.insee.fr/fr/statistiques/7632565
serie <- serie %>% 
  dplyr::select(CODGEO:D68_POP) %>% 
  left_join(.,basecom %>%
              dplyr::select(CODGEO,DEP,REG,EPCI,BV2022),by="CODGEO") %>%
  filter(REG>'10')

serie_metro <- serie %>%  
  mutate(REG="METRO")  %>% 
  group_by(REG) %>%  
  summarise_if(is.numeric,sum) %>%
  pivot_longer(!REG, names_to = "année",values_to = "pop") %>% 
  group_by(REG) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_20=spk_chr(pop))

serie_reg <- serie %>% 
  group_by(REG) %>%  
  summarise_if(is.numeric,sum) %>%
  pivot_longer(!REG, names_to = "année",values_to = "pop") %>% 
  group_by(REG) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_20=spk_chr(pop))%>%
  rbind(serie_metro)

serie_dep <- serie %>% 
  group_by(REG,DEP) %>%  
  summarise_if(is.numeric,sum) %>%
  pivot_longer(!c('REG','DEP'), names_to = "année",values_to = "pop") %>% 
  group_by(REG,DEP) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_20=spk_chr(pop)) %>%
  rbind(serie_reg %>% 
          filter (REG=='27') %>% 
          mutate(DEP="BFC"))%>%
  rbind(serie_metro %>% 
          mutate(DEP="METRO"))

serie_epci <- serie %>% 
  group_by(EPCI) %>%  
  summarise_if(is.numeric,sum) %>%
  pivot_longer(!EPCI, names_to = "année",values_to = "pop") %>% 
  group_by(EPCI) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_20=spk_chr(pop))%>%
  rbind(serie_reg %>% 
          filter (REG=='27') %>% 
          mutate(EPCI="BFC") %>% 
          dplyr::select(-REG))

serie_bv <- serie %>% 
  group_by(BV2022) %>%  
  summarise_if(is.numeric,sum)%>%
  pivot_longer(!BV2022, names_to = "année",values_to = "pop") %>% 
  group_by(BV2022) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_20=spk_chr(pop))%>%
  rbind(serie_reg %>% 
          filter (REG=='27') %>% 
          mutate(BV2022="BFC") %>% 
          dplyr::select(-REG))

save(serie_metro,serie_reg,serie_dep,serie_epci,serie_bv,
     file="data/demo/spark.RData")

#formattable(serie_reg) %>%
#  formattable::as.htmlwidget() %>%
#  spk_add_deps()

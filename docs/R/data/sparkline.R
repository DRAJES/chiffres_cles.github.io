library(readxl)
library(sparkline)
sparkline(0)
load("C:/Users/plebre/Documents/projets R/DRAJES/data/demo/basecom.RData")

serie <- read.csv2 ("I:/SUPPORT/04_STATS/Sources/INSEE/RP/evolution/base-cc-serie-historique-2018.CSV",as.is = T)
serie <- serie %>% select(1:9) %>% 
  left_join(.,communes,by="CODGEO") %>%
  filter(REG>'10')

#serie <- read_excel ("I:/SUPPORT/04_STATS/Sources/INSEE/RP/evolution/base-pop-historiques-1876-2018.xlsx",sheet = 1,skip = 5)
#serie <- serie %>% select(1,5:36) %>% 
#  left_join(.,communes,by="CODGEO") %>%
#  filter(REG>'10')


serie_metro <- serie %>%  mutate(REG="METRO")  %>% group_by(REG) %>%  summarise_if(is.numeric,sum) %>%
  pivot_longer(!REG, names_to = "année",values_to = "pop") %>% group_by(REG) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_18=spk_chr(pop))
serie_reg <- serie %>% group_by(REG) %>%  summarise_if(is.numeric,sum) %>%
  pivot_longer(!REG, names_to = "année",values_to = "pop") %>% group_by(REG) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_18=spk_chr(pop))%>%
  rbind(serie_metro)
serie_dep <- serie %>% group_by(REG,DEP) %>%  summarise_if(is.numeric,sum) %>%
  pivot_longer(!c('REG','DEP'), names_to = "année",values_to = "pop") %>% group_by(REG,DEP) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_18=spk_chr(pop)) %>%
  rbind(serie_reg %>% filter (REG=='27') %>% mutate(DEP="BFC"))%>%
  rbind(serie_metro %>% mutate(DEP="METRO"))
serie_epci <- serie %>% group_by(EPCI) %>%  summarise_if(is.numeric,sum) %>%
  pivot_longer(!EPCI, names_to = "année",values_to = "pop") %>% group_by(EPCI) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_18=spk_chr(pop))%>%
  rbind(serie_reg %>% filter (REG=='27') %>% mutate(EPCI="BFC") %>% select(-REG))
serie_bv <- serie %>% group_by(BV2012) %>%  summarise_if(is.numeric,sum)%>%
  pivot_longer(!BV2012, names_to = "année",values_to = "pop") %>% group_by(BV2012) %>%
  arrange((année),.by_group=T) %>%
  summarise(evol68_18=spk_chr(pop))%>%
  rbind(serie_reg %>% filter (REG=='27') %>% mutate(BV2012="BFC") %>% select(-REG))

save(serie_metro,serie_reg,serie_dep,serie_epci,serie_bv,
     file="C:/Users/plebre/Documents/projets R/DRAJES/data/demo/spark.RData")

#formattable(serie_reg) %>%
#  formattable::as.htmlwidget() %>%
#  spk_add_deps()

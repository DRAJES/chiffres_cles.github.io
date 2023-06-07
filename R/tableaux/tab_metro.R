Sys.setlocale("LC_CTYPE","french")
options(encoding = "UTF-8")
#source("librairies.R")
load("data/demo/basecom.RData")
source("R/fonctions/fonctions.R")

#création des tableaux

metro <- basecomQPV %>%
  group_by()%>% 
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  tableau() %>%
  mutate(REG="METRO",LIBGEO="METRO") 

metro_tot <- metro %>% total()

source("R/tableaux/tab_reg.R", encoding="utf-8")
source("R/tableaux/tab_dep.R", encoding="utf-8")
source("R/tableaux/tab_epci.R", encoding="utf-8")
source("R/tableaux/tab_bv.R", encoding="utf-8")


save(region_tab,dep27_tab,dep_tab,epci27_tab,bv27_tab,
     metro,region,departement,EPCI,BV,
     file = "data/demo/tab_demo.RData")

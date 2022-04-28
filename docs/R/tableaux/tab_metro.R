Sys.setlocale("LC_CTYPE","french")
options(encoding = "UTF-8")
source("C:/Users/plebre/Documents/projets R/DRAJES/librairies.R")
load("C:/Users/plebre/Documents/projets R/DRAJES/data/demo/basecom.RData")

#création des tableaux

metro <- basecomQPV %>%
  group_by()%>% 
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  mutate(REG="METRO",
         densite=100*pop/SUPERFICIE,
         evol=100*((pop/pop_ante)^(1/5)-1),
         evol1529=100*((p1529/p1529_ante)^(1/5)-1),
         evol1625=100*((p1625/p1625_ante)^(1/5)-1),
         evol65=100*((p65/p65_ante)^(1/5)-1),
         evol75=100*((p75/p75_ante)^(1/5)-1),
         propF=100*popf/pop,
         prop1529=100*p1529/pop,
         prop1625=100*p1625/pop,
         prop65=100*p65/pop,
         prop75=100*p75/pop,
         ind_jeun=100*p20/p60,
         partZRR=100*popZRR/pop,
         partQPV=100*POP_MUN/pop,
         diffjeun=p1529-p1529_ante,
         LIBGEO="METRO")


 
metro_tot <- metro %>%
          select(LIBGEO,population=pop,densite,pop_15_29=p1529,
                 part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
                 evolution=evol,evol1529,evol1625,evol65,evol75,
                 ind_jeun,propF,partZRR,partQPV,diffjeun) %>%
          mutate_at(vars(matches("pop")),  
                    ~cell_spec(comma(.,digits = 0,big.mark = " ") ) )%>%
        mutate_at(vars(matches("diffjeun")),  
            ~cell_spec(comma(.,digits = 0,big.mark = " ") ) )%>%
          mutate_at(vars(matches("part")), 
                    ~cell_spec(comma(.,digits = 1,big.mark = " ",decimal.mark=",") ) )%>%
          mutate_at(vars(matches("evol")), 
                    ~cell_spec(comma(.,digits = 2,big.mark = " ",decimal.mark=",") ) )%>%
          mutate(densite =cell_spec( comma(densite,digits=1,big.mark = " ",decimal.mark=",") ),
                 rang="-", rang_national="-",
                 ind_jeun=cell_spec(comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",") ),
                 propF=cell_spec(comma(propF,digits=1,big.mark = " ",decimal.mark=",") )
          ) 

source("C:/Users/plebre/Documents/projets R/DRAJES/R/tableaux/tab_reg.R", encoding="utf-8")
source("C:/Users/plebre/Documents/projets R/DRAJES/R/tableaux/tab_dep.R", encoding="utf-8")
source("C:/Users/plebre/Documents/projets R/DRAJES/R/tableaux/tab_epci.R", encoding="utf-8")
source("C:/Users/plebre/Documents/projets R/DRAJES/R/tableaux/tab_bv.R", encoding="utf-8")


save(region_tab,dep27_tab,dep_tab,epci27_tab,bv27_tab,
     metro,region,departement,EPCI,BV,
     file = "C:/Users/plebre/Documents/projets R/DRAJES/data/demo/tab_demo.RData")

library(rio)
library(data.table)
library(rgdal)
library(readxl)
library(janitor)
library(tidyverse)

ACM_SH <- read_excel("data/PanoFrance2020.xlsx",range = ("A748:DO796") )
ACM_colo <- read_excel("data/PanoFrance2020.xlsx",range = ("A802:DO834") )
ACM_scout <- read_excel("data/PanoFrance2020.xlsx",range = ("A840:DO849") )

ACM_SH_dep <- ACM_SH %>% select(1,15:23,111) %>% mutate_at(vars(2:11),as.numeric) 

ACM_SH <- ACM_SH %>% select(1,2,15,24,29,36,39,50,56,65,71,84,98,104,111) %>%
  mutate_at(vars(2:15), as.numeric) 
 # adorn_totals("row",name="total") 
#diplreg <-  mutate_all(diplreg,~replace(.,is.na(.),"") )

ACM_colo_dep <- ACM_colo %>% select(1,15:23,111) %>% mutate_at(vars(2:9),as.numeric) 

ACM_colo <- ACM_colo %>% select(1,2,15,24,29,36,39,50,56,65,71,84,98,104,111) %>%
mutate_at(vars(2:15), as.numeric) 
#  adorn_totals("row",name="total") 

ACM_scout_dep <- ACM_scout %>% select(1,15:23,111) %>% mutate_at(vars(2:9),as.numeric) 

ACM_scout <- ACM_scout %>% select(1,2,15,24,29,36,39,50,56,65,71,84,98,104,111) %>%
mutate_at(vars(2:15), as.numeric) 
 # adorn_totals("row",name="total") 

save(ACM_colo,ACM_SH,ACM_scout,ACM_colo_dep,ACM_SH_dep,ACM_scout_dep,
     file="data/jeunesse/ACM_pano.RData")

library(rio)
library(data.table)
library(rgdal)
library(readxl)
library(janitor)
library(tidyverse)


ACM_PMI <- read_excel("C:/Users/plebre/Documents/projets R/DRAJES/data/PanoFrance2020.xlsx",range = ("A856:DO869") )


ACM_PMI_dep <- ACM_PMI %>% select(1,15:23,111) %>% mutate_at(vars(2:11),as.numeric) 


ACM_PMI <- ACM_PMI %>% select(1,2,15,24,29,36,39,50,56,65,71,84,98,104,111) %>%
mutate_at(vars(2:15), as.numeric) 
#  adorn_totals("row",name="total") 
#diplreg <-  mutate_all(diplreg,~replace(.,is.na(.),"") )


save(ACM_PMI,ACM_PMI_dep,
     file="C:/Users/plebre/Documents/projets R/DRAJES/data/jeunesse/PMI.RData")
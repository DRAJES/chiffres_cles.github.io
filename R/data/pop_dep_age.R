library(janitor)
library(crosstalk)

pop_age <- function(.tbl,age_min,age_max){
  age_min = paste0(if_else(nchar(age_min)==1,"00","0"),age_min-1)
  age_max = paste0(if_else(nchar(age_max)==1,"00","0"),age_max+1)
  
.tbl %>%
  group_by(DEP) %>%
  summarise(pop_tot=sum(NB,na.rm = T),
            pop_tot_H=sum(NB[SEXE==1],na.rm = T),
            pop_tot_F=sum(NB[SEXE==2],na.rm = T),
            pop_age_H=sum(NB[AGED100 > age_min & AGED100 < age_max & SEXE==1],na.rm = T),
            pop_age_F=sum(NB[AGED100 > age_min & AGED100 < age_max & SEXE==2],na.rm = T),
            pop_age=sum(NB[AGED100 > age_min & AGED100 < age_max],na.rm = T)
  ) %>%
  adorn_totals("row") %>%
  mutate(pop_tot_H = round(pop_tot_H),
         pop_tot_F = round(pop_tot_F),
         pop_age_H = round(pop_age_H),
         pop_age_F = round(pop_age_F),
         pop_age = round(pop_age),
         txfem=round(100*pop_age_F/pop_age,1)) %>% 
    DT::datatable()
}

#pop_age(age_min = 6,age_max = 17)

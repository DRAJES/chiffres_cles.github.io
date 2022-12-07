library(rio)
library(tidyverse)

ACMH_var <- function(.tbl){.tbl %>%
  mutate(année=année+2009) %>%
    mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
           `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`)) %>% 
    select(-`6-11 ans`,-`12-17 ans`) %>%
    relocate(c(`6-13 ans`,`14-17 ans`),.after = `Moins de 6 ans`)
}

ACMHreg_ensemble <- import_list(
  "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
  rbind = TRUE, rbind_label = "année",range = "A4:K31") %>% 
  mutate(`Destination`=str_to_title(.[,2] ) ) %>%
  select(-2) %>%
  relocate(Destination,.after = 1) %>%
  ACMH_var()

ACMHreg_vacances <- ACMHreg_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "L4:T31") ) %>% 
  ACMH_var()

ACMHreg_courts <- ACMHreg_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "U4:AC31") ) %>% 
  ACMH_var()

ACMHreg_specifiques <- ACMHreg_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "AD4:AL31") ) %>% 
  ACMH_var()

ACMHreg_accessoires <- ACMHreg_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "AM4:AU31") ) %>% 
  rename("dont séjours de cinq jours ou plus"="dont séjours de cinq jours") %>%
  ACMH_var()




ACMHdep_ensemble <- import_list(
  "C:/Users/plebre/Documents/ACM/2021/ACMH_departemental&pays202011103.xlsx",
  rbind = TRUE, rbind_label = "année",range = "A4:K335") %>% 
  mutate(`Destination`=str_to_title(.[,2] ) ) %>%
  select(-2) %>%
  relocate(Destination,.after = 1) %>%
  ACMH_var()

ACMHdep_vacances <- ACMHdep_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_departemental&pays202011103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "L4:T335") ) %>% 
  ACMH_var()

ACMHdep_courts <- ACMHdep_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_departemental&pays202011103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "U4:AC335") ) %>% 
  ACMH_var()

ACMHdep_specifiques <- ACMHdep_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_departemental&pays202011103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "AD4:AL335") ) %>% 
  ACMH_var()

ACMHdep_accessoires <- ACMHdep_ensemble %>% select(1:2) %>% 
  bind_cols( import_list(
    "C:/Users/plebre/Documents/ACM/2021/ACMH_departemental&pays202011103.xlsx",
    rbind = TRUE, rbind_label = "année",range = "AM4:AU335") ) %>% 
  rename("dont séjours de cinq jours ou plus"="dont séjours de cinq jours") %>%
  ACMH_var()


save(ACMHreg_accessoires,ACMHreg_specifiques,ACMHreg_courts,ACMHreg_vacances,ACMHreg_ensemble,
     ACMHdep_accessoires,ACMHdep_specifiques,ACMHdep_courts,ACMHdep_vacances,ACMHdep_ensemble,
     file = "data/jeunesse/ACMH.RData")

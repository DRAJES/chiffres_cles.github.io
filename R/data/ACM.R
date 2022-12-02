library(rio)
library(tidyverse)

ACMHreg_ensemble <- import_list(
                            "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
                            rbind = TRUE, rbind_label = "année",range = "A4:K31") %>% 
                    mutate(année=année+2009,
                           `Libellé de la région de destination`=str_to_title(`Libellé de la région de destination`) )%>%
                    mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
                           `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`)) %>% 
                    select(-`6-11 ans`,-`12-17 ans`) %>%
                    relocate(c(`6-13 ans`,`14-17 ans`),.after = `Moins de 6 ans`)
ACMHreg_vacances <- ACMHreg_ensemble %>% select(1:2) %>% 
                    bind_cols( import_list(
                                    "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
                                    rbind = TRUE, rbind_label = "année",range = "L4:T31") ) %>% 
                    mutate(année=année+2009,
                          `Libellé de la région de destination`=str_to_title(`Libellé de la région de destination`) )%>%
                    mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
                          `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`)) %>% 
                    select(-`6-11 ans`,-`12-17 ans`) %>%
                    relocate(c(`6-13 ans`,`14-17 ans`),.after = `Moins de 6 ans`)
ACMHreg_courts <- ACMHreg_ensemble %>% select(1:2) %>% 
                    bind_cols( import_list(
                                      "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
                                      rbind = TRUE, rbind_label = "année",range = "U4:AC31") ) %>% 
                    mutate(année=année+2009,
                          `Libellé de la région de destination`=str_to_title(`Libellé de la région de destination`) )%>%
                    mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
                          `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`)) %>% 
                    select(-`6-11 ans`,-`12-17 ans`) %>%
                    relocate(c(`6-13 ans`,`14-17 ans`),.after = `Moins de 6 ans`)
ACMHreg_specifiques <- ACMHreg_ensemble %>% select(1:2) %>% 
                        bind_cols( import_list(
                                        "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
                                        rbind = TRUE, rbind_label = "année",range = "AD4:AL31") ) %>% 
                        mutate(année=année+2009,
                              `Libellé de la région de destination`=str_to_title(`Libellé de la région de destination`) )%>%
                        mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
                               `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`)) %>% 
                        select(-`6-11 ans`,-`12-17 ans`) %>%
                        relocate(c(`6-13 ans`,`14-17 ans`),.after = `Moins de 6 ans`)
ACMHreg_accessoires <- ACMHreg_ensemble %>% select(1:2) %>% 
                        bind_cols( import_list(
                                        "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx",
                                        rbind = TRUE, rbind_label = "année",range = "AM4:AU31") ) %>% 
                        mutate(année=année+2009,
                              `Libellé de la région de destination`=str_to_title(`Libellé de la région de destination`) )%>%
                        mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
                               `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`)) %>% 
                        select(-`6-11 ans`,-`12-17 ans`) %>%
                        relocate(c(`6-13 ans`,`14-17 ans`),.after = `Moins de 6 ans`) %>%
                        rename(`dont séjours de cinq jours ou plus`=`dont séjours de cinq jours`)

save(ACMHreg_accessoires,ACMHreg_specifiques,ACMHreg_courts,ACMHreg_vacances,ACMHreg_ensemble,
     file = "data/jeunesse/ACM.RData")

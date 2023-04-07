library(rio)
library(tidyverse)

ACMH_var <- function(range){
  
  data <- "C:/Users/plebre/Documents/ACM/2021/ACMH_regional20211103.xlsx"
  
  import_list(
    data,
    rbind = TRUE, 
    rbind_label = "année",
    range = "A4:B31") %>% 
    mutate(`Destination`=str_to_title(.[,2] ) ) %>%
    select(REG=1,Destination) %>%
    bind_cols(
      import_list(
        data,
        rbind = TRUE, rbind_label = "année",range = range)  %>%
        mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
               `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`),
               année=année+2009) %>%
        mutate_all(as.numeric) %>% 
        select(1,"dont séjours de cinq jours ou plus"= 2,3:4,
               "6-13 ans", "14-17 ans",7:10)  
      )
}

ACMH_reg <- ACMH_var(range = "C4:K31") %>% mutate(séjour="Ensemble des séjours") %>%
  bind_rows(ACMH_var(range = "L4:T31") %>% mutate(séjour="Séjours de vacances")) %>%
  bind_rows(ACMH_var(range = "U4:AC31") %>% mutate(séjour="Séjours courts")) %>%
  bind_rows(ACMH_var(range = "AD4:AL31") %>% mutate(séjour="Séjours spécifiques")) %>%
  bind_rows(ACMH_var(range = "AM4:AU31") %>% mutate(séjour="Séjours activité accessoire"))


ACMH_var <- function(range){
  
  data <-   "C:/Users/plebre/Documents/ACM/2021/ACMH_departemental&pays202011103.xlsx"
  
  import_list(
    data,
    rbind = TRUE, rbind_label = "année",range = "A4:B114") %>% 
    mutate(`Destination`=str_to_title(.[,2] ) ) %>%
    select(DEP=1,Destination) %>%
    mutate(DEP=if_else(nchar(DEP)==1,paste0('0',DEP),DEP)) %>%
    bind_cols(
      import_list(
        data,
        rbind = TRUE, rbind_label = "année",range = range)  %>%
        mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
               `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`),
               année=année+2009) %>%
        mutate_all(as.numeric) %>% 
        select(1,"dont séjours de cinq jours ou plus"= 2,3:4,
               "6-13 ans", "14-17 ans",7:10)  
    )
}

ACMH_dep <- ACMH_var(range = "C4:K114") %>% mutate(séjour="Ensemble des séjours") %>%
  bind_rows(ACMH_var(range = "L4:T114") %>% mutate(séjour="Séjours de vacances")) %>%
  bind_rows(ACMH_var(range = "U4:AC114") %>% mutate(séjour="Séjours courts")) %>%
  bind_rows(ACMH_var(range = "AD4:AL114") %>% mutate(séjour="Séjours spécifiques")) %>%
  bind_rows(ACMH_var(range = "AM4:AU114") %>% mutate(séjour="Séjours activité accessoire")) 



ALJSH_var <- function(range){
  
  data <- "C:/Users/plebre/Documents/ACM/2021/ALJSH_regional20211103.xlsx"
  
  import_list(
    data,
    rbind = TRUE, 
    rbind_label = "année",
    range = "A5:C31") %>% 
    mutate(Région=str_to_title(.[,2] ) ) %>%
    relocate(Région,.after = 1) %>%
    rename(`Nombre de communes total` = 4) %>%
    select(REG=1, Région, `Nombre de communes total`) %>%
    bind_cols(
      import_list(
        data,
        rbind = TRUE, rbind_label = "année",range = range)  %>%
        mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
               `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`),
               année=année+2009) %>%
        mutate_all(as.numeric) %>% 
        select(1:4,"6-13 ans", "14-17 ans",7:8)  
    )
}


ALJSH_reg <- ALJSH_var(range = "D5:J31") %>% mutate(`type d'accueil`="Accueils de loisirs", `période d'activité`= "année") %>%
  bind_rows(ALJSH_var(range = "K5:Q31") %>% mutate(`type d'accueil`="Accueils de jeunes", `période d'activité`= "année")) %>%
  bind_rows(ALJSH_var(range = "R5:X31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "année")) %>%
  bind_rows(ALJSH_var(range = "Y5:AE31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "semaine")) %>%
  bind_rows(ALJSH_var(range = "AF5:AL31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "semaine sauf mercredi et samedi")) %>%
  bind_rows(ALJSH_var(range = "AM5:AS31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "mercredi")) %>%
  bind_rows(ALJSH_var(range = "AT5:AZ31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "samedi")) %>%
  bind_rows(ALJSH_var(range = "BA5:BG31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés scolaires")) %>%
  bind_rows(ALJSH_var(range = "BH5:BN31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Toussaint")) %>%
  bind_rows(ALJSH_var(range = "BO5:BU31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Noël")) %>%
  bind_rows(ALJSH_var(range = "BV5:CB31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Hiver")) %>%
  bind_rows(ALJSH_var(range = "CC5:CI31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Printemps")) %>%
  bind_rows(ALJSH_var(range = "CJ5:CP31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "Juillet")) %>%
  bind_rows(ALJSH_var(range = "CQ5:CW31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "Août")) %>%
  bind_rows(ALJSH_var(range = "CX5:DD31") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "Autres périodes")) 
  

ALJSH_var <- function(range){
  
  data <- "C:/Users/plebre/Documents/ACM/2021/ALJSH_departemental20211103.xlsx"
  
  import_list(
    data,
    rbind = TRUE, 
    rbind_label = "année",
    range = "A5:C114") %>% 
    mutate(Département=str_to_title(.[,2]) ,
           DEP=if_else(nchar(`Code du département`)==1,
                        paste0('0',`Code du département`),
                        `Code du département`) )  %>%
    rename(`Nombre de communes total` = 3) %>%
    select(DEP, Département, `Nombre de communes total`) %>%
    bind_cols(
      import_list(
        data,
        rbind = TRUE, rbind_label = "année",range = range)  %>%
        mutate(`6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
               `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`),
               année=année+2009) %>%
        mutate_all(as.numeric) %>% 
        select(1:4,"6-13 ans", "14-17 ans",7:8)  
    )
}

ALJSH_dep <- ALJSH_var(range = "D5:J114") %>% mutate(`type d'accueil`="Accueils de loisirs", `période d'activité`= "année") %>%
  bind_rows(ALJSH_var(range = "K5:Q114") %>% mutate(`type d'accueil`="Accueils de jeunes", `période d'activité`= "année")) %>%
  bind_rows(ALJSH_var(range = "R5:X114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "année")) %>%
  bind_rows(ALJSH_var(range = "Y5:AE114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "semaine")) %>%
  bind_rows(ALJSH_var(range = "AF5:AL114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "semaine sauf mercredi et samedi")) %>%
  bind_rows(ALJSH_var(range = "AM5:AS114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "mercredi")) %>%
  bind_rows(ALJSH_var(range = "AT5:AZ114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "samedi")) %>%
  bind_rows(ALJSH_var(range = "BA5:BG114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés scolaires")) %>%
  bind_rows(ALJSH_var(range = "BH5:BN114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Toussaint")) %>%
  bind_rows(ALJSH_var(range = "BO5:BU114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Noël")) %>%
  bind_rows(ALJSH_var(range = "BV5:CB114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Hiver")) %>%
  bind_rows(ALJSH_var(range = "CC5:CI114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "congés Printemps")) %>%
  bind_rows(ALJSH_var(range = "CJ5:CP114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "Juillet")) %>%
  bind_rows(ALJSH_var(range = "CQ5:CW114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "Août")) %>%
  bind_rows(ALJSH_var(range = "CX5:DD114") %>% mutate(`type d'accueil`="Accueils loisirs ou jeunes", `période d'activité`= "Autres périodes")) 



AS_reg <-  import_list(
  "C:/Users/plebre/Documents/ACM/2021/AS_regional20211103.xlsx",
  rbind = TRUE, 
  rbind_label = "année",
  range = "A4:J29") %>% 
  mutate(Région=str_to_title(.[,2] ),
         `6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
         `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`),
         année=année+2009 ) %>%
  rename(`Nombre de communes total` = `Nombre de communes au sein de la région au 1er janvier 2020`) %>%
  select(REG=1, Région, `Nombre de communes total`,4:7,`6-13 ans`,`14-17 ans`,10,année) 


AS_dep <-  import_list(
  "C:/Users/plebre/Documents/ACM/2021/AS_departemental20211103.xlsx",
  rbind = TRUE, 
  rbind_label = "année",
  range = "A4:J112") %>% 
  mutate(Département=str_to_title(.[,2]) ,
         DEP=if_else(nchar(`Code du département`)==1,
                     paste0('0',`Code du département`),
                     `Code du département`),
         `6-13 ans` = ifelse(is.na(`6-13 ans`),`6-11 ans`,`6-13 ans`),
         `14-17 ans` = ifelse(is.na(`14-17 ans`),`12-17 ans`,`14-17 ans`),
         année=année+2009 )  %>%
  rename(`Nombre de communes total` = `Nombre de communes au sein du département au 1er janvier 2020`) %>%
  select(DEP, Département, `Nombre de communes total`,4:7,`6-13 ans`,`14-17 ans`,10,année) 


save(ACMH_reg, ACMH_dep, ALJSH_reg, ALJSH_dep, AS_reg, AS_dep,
     file = "data/jeunesse/ACM_evo.RData")

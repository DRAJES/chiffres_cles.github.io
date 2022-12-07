library(readxl)
library(janitor)
library(tidyverse)

bafareg <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/BAFA-BAFD/Donnees-BAFA-BAFD-2011-2021.xlsx",sheet=9, skip = 6 )
bafdreg <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/BAFA-BAFD/Donnees-BAFA-BAFD-2011-2021.xlsx",sheet=10, skip = 6 )
bafadep <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/BAFA-BAFD/Donnees-BAFA-BAFD-2011-2021.xlsx",sheet=11, skip = 6 )

bafaregage <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/BAFA-BAFD/Donnees-BAFA-BAFD-2011-2021.xlsx",sheet=13, skip = 4 )
bafdregage <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/BAFA-BAFD/Donnees-BAFA-BAFD-2011-2021.xlsx",sheet=14, skip = 4 )
bafadepage <- read_excel("I:/SUPPORT/04_STATS/Sources/MEDES/BAFA-BAFD/Donnees-BAFA-BAFD-2011-2021.xlsx",sheet=17, skip = 4 )


bafaregm <- bafareg %>% slice(1:75) %>% select(sexe=1,region=2,3:13) %>%
                                    rename_with(~substr(.,1,4),3:13) %>%
                  left_join(.,appartenance %>% select(-4) %>%
                                filter(NIVGEO=="REG") %>%
                                select(CODGEO,LIBGEO),
                                by = c(region="LIBGEO") ) %>%
                  mutate (CODGEO =  
                            case_when(region == "France métropolitaine + DROM + COM" ~ "France",
                                      region == "Centre-Val-de-Loire" ~ "24",
                                      region == "Grande Aquitaine" ~ "75",
                                      region == "Ile-de-France" ~ "11",
                                      region == "Pays-de-la-Loire" ~ "52",
                                      T ~ CODGEO) ) %>%
                  mutate(sexe=
                           case_when(is.na(sexe)~rep(c("Ensemble","Hommes","Femmes"),each=25 ),
                                     T ~sexe) ) %>%
                  filter(CODGEO > "10") %>%
                  relocate(CODGEO,.before = region) %>%
                  pivot_longer(-(1:3),names_to = "année",values_to = "BAFA") %>%
  mutate(année = as.numeric(année),
         BAFA = as.numeric(BAFA))

bafdregm <- bafdreg %>% slice(1:75) %>% select(sexe=1,region=2,3:13) %>%
                     rename_with(~substr(.,1,4),3:13) %>%
                   left_join(.,appartenance %>% select(-4) %>%
                           filter(NIVGEO=="REG") %>%
                           select(CODGEO,LIBGEO),
                            by = c(region="LIBGEO") ) %>%
                   mutate (CODGEO =  
                    case_when(region == "France métropolitaine + DROM + COM" ~ "France",
                              region == "Centre-Val-de-Loire" ~ "24",
                              region == "Grande Aquitaine" ~ "75",
                              region == "Ile-de-France" ~ "11",
                              region == "Pays-de-la-Loire" ~ "52",
                              T ~ CODGEO) ) %>%
                   mutate(sexe=
                   case_when(is.na(sexe)~rep(c("Ensemble","Hommes","Femmes"),each=25 ),
                             T ~sexe) ) %>%
                 filter(CODGEO > "10") %>%
                 relocate(CODGEO,.before = region) %>%
               pivot_longer(-(1:3),names_to = "année",values_to = "BAFD") %>%
  mutate(année = as.numeric(année),
         BAFD = as.numeric(BAFD))

          

bafadepm <- bafadep %>% slice(1:324) %>% 
               select(sexe=1,dep=2,deplib=3,4:14) %>% filter(is.na (dep) | nchar(dep)<3) %>%
               rename_with(~substr(.,1,4),4:14) %>%
               mutate (dep = 
                         case_when( nchar(dep)==1 ~ paste0('0',dep),
                                    deplib == "France métropolitaine + DROM + COM" ~ "France",
                                    deplib ==  "France métropolitaine" ~ "METRO" ,
                                    T ~ dep )
                          ) %>% filter( ! is.na(dep) ) %>%
               mutate(sexe=
                        case_when(is.na(sexe)~rep(c("Ensemble","Hommes","Femmes"),each=98 ),
                                  T ~ sexe) ) %>%
               pivot_longer(-(1:3),names_to = "année",values_to = "BAFA") %>%
  mutate(année = as.numeric(année),
         BAFA = as.numeric(BAFA))



bafaregagem <- bafaregage %>% slice(1:75) %>% select(sexe=1,region=2,3:13) %>%
  rename_with(~substr(.,1,4),3:13) %>%
  left_join(.,appartenance %>% select(-4) %>%
              filter(NIVGEO=="REG") %>%
              select(CODGEO,LIBGEO),
            by = c(region="LIBGEO") ) %>%
  mutate (`2016`= as.character(`2016`),
    CODGEO =  
            case_when(region == "France métropolitaine + DROM + COM" ~ "France",
                      region == "Centre-Val-de-Loire" ~ "24",
                      region == "Grande Aquitaine" ~ "75",
                      region == "Ile-de-France" ~ "11",
                      region == "Pays-de-la-Loire" ~ "52",
                      T ~ CODGEO) ) %>%
  mutate(sexe=
           case_when(is.na(sexe)~rep(c("Ensemble","Hommes","Femmes"),each=25 ),
                     T ~sexe) ) %>%
  filter(CODGEO > "10") %>%
  relocate(CODGEO,.before = region) %>%
  pivot_longer(-(1:3),names_to = "année",values_to = "age BAFA") %>%
  mutate(année = as.numeric(année),
         `age BAFA` = as.numeric(`age BAFA`))


bafdregagem <- bafdregage %>% slice(1:75) %>% select(sexe=1,region=2,3:13) %>%
  rename_with(~substr(.,1,4),3:13) %>%
  left_join(.,appartenance %>% select(-4) %>%
              filter(NIVGEO=="REG") %>%
              select(CODGEO,LIBGEO),
            by = c(region="LIBGEO") ) %>%
  mutate (CODGEO =  
            case_when(region == "France métropolitaine + DROM + COM" ~ "France",
                      region == "Centre-Val-de-Loire" ~ "24",
                      region == "Grande Aquitaine" ~ "75",
                      region == "Ile-de-France" ~ "11",
                      region == "Pays-de-la-Loire" ~ "52",
                      T ~ CODGEO) ) %>%
  mutate(sexe=
           case_when(is.na(sexe)~rep(c("Ensemble","Hommes","Femmes"),each=25 ),
                     T ~sexe) ) %>%
  filter(CODGEO > "10") %>%
  relocate(CODGEO,.before = region) %>%
  pivot_longer(-(1:3),names_to = "année",values_to = "age BAFD") %>%
  mutate(année = as.numeric(année),
         `age BAFD` = as.numeric(`age BAFD`))



bafadepagem <- bafadepage %>% slice(1:324) %>% 
  select(sexe=1,dep=2,deplib=3,4:14) %>% filter(is.na (dep) | nchar(dep)<3) %>%
  rename_with(~substr(.,1,4),4:14) %>%
  mutate (`2016`=as.character(`2016`),
    dep = 
            case_when( nchar(dep)==1 ~ paste0('0',dep),
                       deplib == "France métropolitaine + DROM + COM" ~ "France",
                       deplib ==  "France métropolitaine" ~ "METRO" ,
                       T ~ dep )
  ) %>% filter( ! is.na(dep) ) %>%
  mutate(sexe=
           case_when(is.na(sexe)~rep(c("Ensemble","Hommes","Femmes"),each=98 ),
                     T ~ sexe) ) %>%
  pivot_longer(-(1:3),names_to = "année",values_to = "age BAFA") %>%
  mutate(année = as.numeric(année),
    `age BAFA` = as.numeric(`age BAFA`))



bafadepm %>%
  filter(sexe %in% "Ensemble") %>%
  filter(dep %in% depbfc) %>%
  filter(!(dep %in% c("France", "METRO"))) %>%
  ggplot() +
  aes(x = année, y = BAFA, fill = dep, colour = dep) +
  geom_line(size = 1.85) +
  #geom_area() %>%
  scale_fill_hue(direction = 1) +
  scale_color_hue(direction = 1) +
  theme_minimal()


save(bafadepagem,bafadepm,bafaregagem,bafaregm,bafdregagem,bafdregm,
     file = "data/formation/bafa.RData")

BV <- basecomQPV %>%
  group_by(BV2022) %>%
  tableau() %>%
  left_join(.,appartenance %>% 
              dplyr::filter(NIVGEO=="BV2022") %>%
              dplyr::distinct(CODGEO,LIBGEO) ,
            by=c("BV2022" = "CODGEO") )


bv27_tab <- BV %>% 
  filter(BV2022 %in% basecom$BV2022[basecom$REG=="27"]) %>%
  ungroup() %>% 
  mutate(rang=rank(-densite)) %>%
  select(BV2022,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,prop1529F,prop1625F,prop65F,rang,partZRR,partQPV,diffjeun) %>% 
  arrange(desc(population)) %>%
  
  #limites() %>%
  
  forme()   %>%
  filter(!is.na(BV2022))  %>%
  rbind(reg_tot %>% select(-rang_national) %>% mutate(BV2022="BFC") ) %>%
  rbind(metro_tot %>% select(-rang_national) %>% mutate(BV2022="METRO") )  %>%
  noms()

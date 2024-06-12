EPCI <- basecomQPV %>%
  group_by(EPCI)%>%
  tableau() %>%
  left_join(.,appartenance %>% 
              dplyr::filter(NIVGEO=="EPCI") %>%
              dplyr::select(CODGEO,LIBGEO) ,
            by=c("EPCI" = "CODGEO") )


epci27_tab <- EPCI %>% 
  filter(EPCI %in% basecom$EPCI[basecom$REG=="27"]) %>%
  ungroup() %>% 
  mutate(rang=rank(-densite)) %>%
  select(EPCI,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,prop1529F,prop1625F,prop65F,rang,partZRR,partQPV,diffjeun) %>% 
  arrange(desc(population)) %>% 
  
  #limites() %>%
  
  forme() %>% 
  filter(!is.na(EPCI)) %>%
  rbind(reg_tot %>% select(-rang_national) %>% mutate(EPCI="BFC") ) %>%
  rbind(metro_tot %>% select(-rang_national) %>%  mutate(EPCI="METRO") )  %>%
  noms()

region <- basecomQPV %>%
  group_by(REG)%>% 
  tableau() %>%
  left_join(.,appartenance %>% filter(NIVGEO=="REG") %>%
              select(CODGEO,LIBGEO) ,
            by=c("REG" = "CODGEO") ) 

#tableau---- 


region_tab <- region %>% 
  #filter(REG!="METRO")%>%
  select(REG,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,prop1529F,prop1625F,prop65F,partZRR,partQPV,diffjeun) %>%
          arrange((REG)) %>% 
  
   #       limites() %>%
  
          forme() %>%
    filter(!is.na(REG)) %>%
    rbind(metro_tot %>% select(-rang_national) %>% mutate(REG="METRO") ) %>%
    rename (Région=REG) %>%
    noms() 
                


reg_tot <- region %>% 
  mutate(rang_national=as.character(rank(-densite))) %>%
  filter(REG=='27') %>% 
  total() 


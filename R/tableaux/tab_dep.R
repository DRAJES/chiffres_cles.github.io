departement <- basecomQPV %>%
  group_by(REG,DEP)%>%
  tableau() %>%
    left_join(.,appartenance %>% filter(NIVGEO=="DEP") %>%
              select(CODGEO,LIBGEO) ,
            by=c("DEP" = "CODGEO") ) 
departement <- as.data.frame(departement)
#dep27----

dep27_tab <- departement %>% ungroup() %>% mutate(rang_national=as.character(rank(-densite))) %>%
   filter(REG=="27") %>%
    mutate(rang=as.character(rank(-densite))) %>%
    select(REG,DEP,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,prop1529F,prop1625F,prop65F,rang,rang_national,partZRR,partQPV,diffjeun) %>%
  group_by(REG) %>%   arrange(REG,DEP) %>% bind_rows(c(part_15_29=0,part_16_25=0,part_65=0,part_75=0,
                                                       evolution=-2,evol1529=-2,evol1625=-2,evol65=0,evol75=-4),
                                                     c(part_15_29=40,part_16_25=40,part_65=40,part_75=40,
                                                       evolution=2,evol1529=2,evol1625=2,evol65=4,evol75=4)) %>%
  
  forme()   %>% 
  rbind(reg_tot %>% mutate(REG='27',DEP="BFC") ) %>%
  rbind(metro_tot %>%  mutate(REG='METRO',DEP="METRO") )  %>%
  rename(Région=REG, Département=DEP) %>%
  noms() %>%
  filter(!is.na(Région))

#dep france ----
dep_tab <- departement %>% ungroup() %>% mutate(rang_national=as.character(rank(-densite))) %>%
  mutate(rang=as.character(rank(-densite))) %>%
  select(REG,DEP,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,prop1529F,prop1625F,prop65F,rang,rang_national,partZRR,partQPV,diffjeun) %>%
  group_by(REG) %>%   arrange(REG,DEP) %>% bind_rows(c(part_15_29=0,part_16_25=0,part_65=0,part_75=0,
                                                      evolution=-2,evol1529=-2,evol1625=-2,evol65=0,evol75=-4),
                                                    c(part_15_29=40,part_16_25=40,part_65=40,part_75=40,
                                                      evolution=2,evol1529=2,evol1625=2,evol65=4,evol75=4)) %>%
  
  forme()   %>%
  slice(1:96) %>%
  rbind(reg_tot %>% mutate(REG='27',DEP="BFC") ) %>%
  rbind(metro_tot %>%  mutate(REG='METRO',DEP="METRO") )  %>%
  rename(Région=REG, Département=DEP) %>%
  noms() %>% 
  filter(!is.na(Région))

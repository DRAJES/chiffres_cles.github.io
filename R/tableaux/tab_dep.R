departement <- basecomQPV %>%
  group_by(REG,DEP)%>%
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  mutate( densite=100*pop/SUPERFICIE,
          evol=100*((pop/pop_ante)^(1/5)-1),
          evol1529=100*((p1529/p1529_ante)^(1/5)-1),
          evol1625=100*((p1625/p1625_ante)^(1/5)-1),
          evol65=100*((p65/p65_ante)^(1/5)-1),
          evol75=100*((p75/p75_ante)^(1/5)-1),
          propF=100*popf/pop,
          prop1529=100*p1529/pop,
          prop1625=100*p1625/pop,
          prop65=100*p65/pop,
          prop75=100*p75/pop,
          ind_jeun=100*p20/p60,
          partZRR=100*popZRR/pop,
          partQPV=100*POP_MUN/pop,
          diffjeun=p1529-p1529_ante) %>%
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
         ind_jeun,propF,rang,rang_national,partZRR,partQPV,diffjeun) %>%
  group_by(REG) %>%   arrange(REG,DEP) %>% bind_rows(c(part_15_29=0,part_16_25=0,part_65=0,part_75=0,
                                                       evolution=-2,evol1529=-2,evol1625=-2,evol65=-4,evol75=-4),
                                                     c(part_15_29=40,part_16_25=40,part_65=40,part_75=40,
                                                       evolution=2,evol1529=2,evol1625=2,evol65=4,evol75=4)) %>%
  
  mutate_at(vars(starts_with("pop")), ~cell_spec(comma(.,digits = 0,big.mark = " ")))%>%
  mutate(rang= if_else( rang >= "80", 
                        cell_spec(rang,"html",color = "green", bold=T) ,
                        cell_spec(rang,"html",color = "grey", bold=F)  ) ) %>%
  mutate(diffjeun=if_else( diffjeun < 0,
                           cell_spec(comma(round(diffjeun/10,0)*10 ,
                                           digits = 0,big.mark = " "),"html",color = "red"),
                           cell_spec(comma(round(diffjeun/10,0)*10 ,
                                           digits = 0,big.mark = " "),"html",color = "green")
  )) %>%
  mutate( densite =  color_tile('white','grey')
          (comma(densite,digits=1,big.mark = " ",decimal.mark=",")),
          part_15_29=color_bar("#FFD700")
          (comma(part_15_29,digits=1,big.mark = " ",decimal.mark=",")),
          part_16_25=color_bar("#98FB98")
          (comma(part_16_25,digits=1,big.mark = " ",decimal.mark=",")),
          part_65=color_bar("Plum")
          (comma(part_65,digits=1,big.mark = " ",decimal.mark=",")),
          part_75=color_bar("Plum")
          (comma(part_75,digits=1,big.mark = " ",decimal.mark=",")),
          evolution=normalize_bar("lightblue")
          (comma(evolution,digits=2,big.mark = " ",decimal.mark=",")),
          evol1529=normalize_bar("#FFD700")
          (comma(evol1529,digits=2,big.mark = " ",decimal.mark=",")),
          evol1625=normalize_bar("#98FB98")
          (comma(evol1625,digits=2,big.mark = " ",decimal.mark=",")),
          evol65=normalize_bar("Plum")
          (comma(evol65,digits=2,big.mark = " ",decimal.mark=",")),
          evol75=normalize_bar("Plum")
          (comma(evol75,digits=2,big.mark = " ",decimal.mark=",")),
          ind_jeun= color_tile('#9B90CE','#F2F0F7',alpha=0.3)
          (comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",")),
          propF= color_tile('white','pink',alpha=0.5)
          (comma(propF,digits=1,big.mark = " ",decimal.mark=",")),
          partZRR=color_bar('lightgreen')
          (comma(partZRR,digits=1,big.mark = " ",decimal.mark=",")),
          partQPV=color_bar('orange')
          (comma(partQPV,digits=1,big.mark = " ",decimal.mark=","))
  )    %>% 
  rbind(reg_tot %>% mutate(REG='27',DEP="BFC") ) %>%
  rbind(metro_tot %>%  mutate(REG='METRO',DEP="METRO") )  %>%
  dplyr::rename(Département=DEP, Région=REG,Nom=LIBGEO,"Densité"=densite,Population=population,
                "15-29 ans"=pop_15_29,"Taux<br>15/29"=part_15_29,"Taux<br>16/25"=part_16_25,
                "65 ans et +"=pop_65,"Taux<br>>65 ans"=part_65, "Taux<br>>75 ans" = part_75,
                "évolution annuelle"=evolution,"evo15/29"=evol1529,"evo16/25"=evol1625,
                "evo>65"=evol65,"evo>75"=evol75, "delta"=diffjeun,
                "Taux femmes"=propF) %>% filter(!is.na(Région))

#dep france ----
dep_tab <- departement %>% ungroup() %>% mutate(rang_national=as.character(rank(-densite))) %>%
  mutate(rang=as.character(rank(-densite))) %>%
  select(REG,DEP,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,rang,rang_national,partZRR,partQPV,diffjeun) %>%
  group_by(REG) %>%   arrange(REG,DEP) %>%bind_rows(c(part_15_29=0,part_16_25=0,part_65=0,part_75=0,
                                                      evolution=-2,evol1529=-2,evol1625=-2,evol65=-4,evol75=-4),
                                                    c(part_15_29=40,part_16_25=40,part_65=40,part_75=40,
                                                      evolution=2,evol1529=2,evol1625=2,evol65=4,evol75=4)) %>%
  
  mutate_at(vars(starts_with("pop")), ~cell_spec(comma(.,digits = 0,big.mark = " ")))%>%
  mutate(rang= if_else( rang >= "80", 
                        cell_spec(rang,"html",color = "green", bold=T) ,
                        cell_spec(rang,"html",color = "grey", bold=F)  ) ) %>%
  mutate(diffjeun=if_else( diffjeun < 0,
                           cell_spec(comma(round(diffjeun/10,0)*10 ,
                                           digits = 0,big.mark = " "),"html",color = "red"),
                           cell_spec(comma(round(diffjeun/10,0)*10 ,
                                           digits = 0,big.mark = " "),"html",color = "green")
  )) %>%
  mutate( densite =  color_tile('white','grey')
          (comma(densite,digits=1,big.mark = " ",decimal.mark=",")),
          part_15_29=color_bar("#FFD700")
          (comma(part_15_29,digits=1,big.mark = " ",decimal.mark=",")),
          part_16_25=color_bar("#98FB98")
          (comma(part_16_25,digits=1,big.mark = " ",decimal.mark=",")),
          part_65=color_bar("Plum")
          (comma(part_65,digits=1,big.mark = " ",decimal.mark=",")),
          part_75=color_bar("Plum")
          (comma(part_75,digits=1,big.mark = " ",decimal.mark=",")),
          evolution=normalize_bar("lightblue")
          (comma(evolution,digits=2,big.mark = " ",decimal.mark=",")),
          evol1529=normalize_bar("#FFD700")
          (comma(evol1529,digits=2,big.mark = " ",decimal.mark=",")),
          evol1625=normalize_bar("#98FB98")
          (comma(evol1625,digits=2,big.mark = " ",decimal.mark=",")),
          evol65=normalize_bar("Plum")
          (comma(evol65,digits=2,big.mark = " ",decimal.mark=",")),
          evol75=normalize_bar("Plum")
          (comma(evol75,digits=2,big.mark = " ",decimal.mark=",")),
          ind_jeun= color_tile('#9B90CE','#F2F0F7',alpha=0.3)
          (comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",")),
          propF= color_tile('white','pink',alpha=0.5)
          (comma(propF,digits=1,big.mark = " ",decimal.mark=",")),
          partZRR=color_bar('lightgreen')
          (comma(partZRR,digits=1,big.mark = " ",decimal.mark=",")),
          partQPV=color_bar('orange')
          (comma(partQPV,digits=1,big.mark = " ",decimal.mark=","))
  )    %>% slice(1:96) %>%
  rbind(reg_tot %>% mutate(REG='27',DEP="BFC") ) %>%
  rbind(metro_tot %>%  mutate(REG='METRO',DEP="METRO") )  %>%
  dplyr::rename(Département=DEP, Région=REG,Nom=LIBGEO,"Densité"=densite,Population=population,
                "15-29 ans"=pop_15_29,"Taux<br>15/29"=part_15_29,"Taux<br>16/25"=part_16_25,
                "65 ans et +"=pop_65,"Taux<br>>65 ans"=part_65, "Taux<br>>75 ans" = part_75,
                "évolution annuelle"=evolution,"evo15/29"=evol1529,"evo16/25"=evol1625,
                "evo>65"=evol65,"evo>75"=evol75,"delta"=diffjeun,
                "Taux femmes"=propF) %>% filter(!is.na(Région))

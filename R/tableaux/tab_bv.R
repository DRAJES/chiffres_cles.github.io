BV <- basecomQPV %>%
  group_by(BV2012)%>%
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
    left_join(.,appartenance %>% filter(NIVGEO=="BV2012") %>%
              select(CODGEO,LIBGEO) ,
            by=c("BV2012" = "CODGEO") )


bv27_tab <- BV %>% filter(BV2012 %in% basecom$BV2012[basecom$REG=="27"]) %>%
  ungroup() %>% mutate(rang=rank(-densite)) %>%
  select(BV2012,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,rang,partZRR,partQPV,diffjeun) %>% 
  arrange(desc(population)) %>%bind_rows(c(part_15_29=0,part_16_25=0,part_65=0,part_75=0,
                                           evolution=-5,evol1529=-5,evol1625=-5,evol65=0,evol75=-10),
                                         c(part_15_29=30,part_16_25=30,part_65=60,part_75=60,
                                           evolution=5,evol1529=5,evol1625=5,evol65=8,evol75=10)) %>%
  
  mutate_at(vars(starts_with("pop")), ~cell_spec(comma(.,digits = 0,big.mark = " ")))%>%
  mutate(rang= if_else( rank(densite) < 20 , 
                        cell_spec(rank(-densite),"html",color = "green", bold=T) ,
                        cell_spec(rank(-densite),"html",color = "grey", bold=F)  ) ) %>%
  mutate(diffjeun=if_else( diffjeun < 0,
                           cell_spec(comma(round(diffjeun/10,0)*10 ,
                                           digits = 0,big.mark = " "),"html",color = "red"),
                           cell_spec(comma(round(diffjeun/10,0)*10 ,
                                           digits = 0,big.mark = " "),"html",color = "green")
  )) %>%
  mutate( densite =  color_tile('white','grey')
          (digits(densite,1,big.mark = " ",decimal.mark=",")),
          part_15_29=color_bar("#98FB98")
          (digits(part_15_29,1,big.mark = " ",decimal.mark=",")),
          part_16_25=color_bar("#ADFF2F")
          (digits(part_16_25,1,big.mark = " ",decimal.mark=",")),
          part_65=color_bar("Plum")
          (digits(part_65,1,big.mark = " ",decimal.mark=",")),
          part_75=color_bar("Plum")
          (digits(part_75,1,big.mark = " ",decimal.mark=",")),
          evolution=ifelse(evolution < 0,
                           normalize_bar("#ff000050")(digits(evolution,2,decimal.mark=",")),
                           normalize_bar("#00800050")(digits(evolution,2,decimal.mark=",")) ),
          evol1529=ifelse(evol1529 < 0,
                          normalize_bar("#FF7F5050")(digits(evol1529,2,decimal.mark=",")),
                          normalize_bar("#98FB9850")(digits(evol1529,2,decimal.mark=",")) ),
          evol1625=ifelse(evol1625 < 0,
                          normalize_bar("#ff634750")(digits(evol1625,2,decimal.mark=",")),
                          normalize_bar("#ADFF2F50")(digits(evol1625,2,decimal.mark=",")) ),
          evol65=ifelse(evol65 < 0,
                        normalize_bar("#D2B48C50")(digits(evol65,2,decimal.mark=",")),
                        normalize_bar("#dda0dd50")(digits(evol65,2,decimal.mark=",")) ),
          evol75=normalize_bar("#dda0dd50")
          (digits(evol75,2,big.mark = " ",decimal.mark=",")),
          ind_jeun= color_tile('#9B90CE','#F2F0F7',alpha=0.3)
          (digits(ind_jeun,1,big.mark = " ",decimal.mark=",")),
          propF= color_tile('white','pink',alpha=0.5)
          (digits(propF,1,big.mark = " ",decimal.mark=",")),
          partZRR=color_bar('lightgreen')
          (digits(partZRR,1,big.mark = " ",decimal.mark=",")),
          partQPV=color_bar('orange')
          (digits(partQPV,1,big.mark = " ",decimal.mark=","))
  )    %>% slice(1:131) %>%
  rbind(reg_tot %>% select(-rang_national) %>% mutate(BV2012="BFC") ) %>%
  rbind(metro_tot %>% select(-rang_national) %>% mutate(BV2012="METRO") )  %>%
  dplyr::rename(Nom=LIBGEO,"Densité"=densite,Population=population,
                "15-29 ans"=pop_15_29,"Taux<br>15/29"=part_15_29,"Taux<br>16/25"=part_16_25,
                "65 ans et +"=pop_65,"Taux<br>>65 ans"=part_65, "Taux<br>>75 ans" = part_75,
                "évolution annuelle"=evolution,"evo15/29"=evol1529,"evo16/25"=evol1625,
                "evo>65"=evol65,"evo>75"=evol75,"delta"=diffjeun,
                "Taux femmes"=propF)

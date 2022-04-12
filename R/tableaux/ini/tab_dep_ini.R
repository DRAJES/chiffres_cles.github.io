source("P:/projets R/chiffres cles/R/tableaux/tab_reg.R", encoding="utf-8")

departement <- basecom %>%
  group_by(REG,DEP)%>%
  summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
  mutate( densite=100*pop/SUPERFICIE,
          evol=100*((pop/pop_2012)^(1/5)-1),
          evol1529=100*((p1529/p1529_2012)^(1/5)-1),
          evol1625=100*((p1625/p1625_2012)^(1/5)-1),
          evol65=100*((p65/p65_2012)^(1/5)-1),
          evol75=100*((p75/p75_2012)^(1/5)-1),
          propF=100*popf/pop,
          prop1529=100*p1529/pop,
          prop1625=100*p1625/pop,
          prop65=100*p65/pop,
          prop75=100*p75/pop,
          ind_jeun=100*p20/p60 ) %>%
  left_join(.,appartenance %>% filter(NIVGEO=="DEP") %>%
              select(CODGEO,LIBGEO) ,
            by=c("DEP" = "CODGEO") ) 

#dep27----

dep27_tab <- departement %>% filter(REG=="27") %>%
  ungroup() %>% mutate(rang=rank(-densite)) %>%
  select(REG,DEP,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF,rang) %>% 
  group_by(REG) %>%   arrange(REG,DEP) %>%
  mutate_at(vars(starts_with("pop")), ~cell_spec(comma(.,digits = 0,big.mark = " ")))%>%
  mutate( densite = ifelse(rang >= 85, 
                           cell_spec(paste0 (comma(densite,digits=1,big.mark = " ",decimal.mark=",") ,
                                             " (",rang,")"),"html",color = "green", bold=T),
                           cell_spec(paste0 (comma(densite,digits=1,big.mark = " ",decimal.mark=",") ,
                                             " (",rang,")"),"html",color = "grey", bold=F) ) ,
          part_15_29=color_bar("gold")
          (comma(part_15_29,digits=1,big.mark = " ",decimal.mark=",")),
          part_16_25=color_bar("gold")
          (comma(part_16_25,digits=1,big.mark = " ",decimal.mark=",")),
          part_65=color_bar("Plum")
          (comma(part_65,digits=1,big.mark = " ",decimal.mark=",")),
          part_75=color_bar("Plum")
          (comma(part_75,digits=1,big.mark = " ",decimal.mark=",")),
          evolution=normalize_bar("lightblue")
          (comma(evolution,digits=2,big.mark = " ",decimal.mark=",")),
          evol1529=normalize_bar("gold")
          (comma(evol1529,digits=2,big.mark = " ",decimal.mark=",")),
          evol1625=normalize_bar("gold")
          (comma(evol1625,digits=2,big.mark = " ",decimal.mark=",")),
          evol65=normalize_bar("Plum")
          (comma(evol65,digits=2,big.mark = " ",decimal.mark=",")),
          evol75=normalize_bar("Plum")
          (comma(evol75,digits=2,big.mark = " ",decimal.mark=",")),
          ind_jeun= color_tile('LightSkyBlue','lightgreen',alpha=0.5)
          (comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",")),
          propF= color_tile('white','pink',alpha=0.5)
          (comma(propF,digits=1,big.mark = " ",decimal.mark=","))
  )    %>%  select(-rang) %>%
  rbind(region %>% filter(REG=='27') %>%
          select(REG,LIBGEO,population=pop,densite,pop_15_29=p1529,
                 part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
                 evolution=evol,evol1529,evol1625,evol65,evol75,
                 ind_jeun,propF) %>%
          mutate(DEP="BFC")%>%
          mutate_at(vars(matches("pop")),  
                    ~cell_spec(comma(.,digits = 0,big.mark = " ") ) )%>%
          mutate_at(vars(matches("part")), 
                    ~cell_spec(comma(.,digits = 1,big.mark = " ",decimal.mark=",") ) )%>%
          mutate_at(vars(matches("evol")), 
                    ~cell_spec(comma(.,digits = 2,big.mark = " ",decimal.mark=",") ) )%>%
          mutate(densite =cell_spec( comma(densite,digits=1,big.mark = " ",decimal.mark=",") ),
                 ind_jeun=cell_spec(comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",") ),
                 propF=cell_spec(comma(propF,digits=1,big.mark = " ",decimal.mark=",") )
          ) ) %>% 
  rbind(metro %>%
          select(REG,LIBGEO,population=pop,densite,pop_15_29=p1529,
                 part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
                 evolution=evol,evol1529,evol1625,evol65,evol75,
                 ind_jeun,propF) %>%
          mutate(DEP="METRO")%>%
          mutate_at(vars(matches("pop")),  
                    ~cell_spec(comma(.,digits = 0,big.mark = " ") ) )%>%
          mutate_at(vars(matches("part")), 
                    ~cell_spec(comma(.,digits = 1,big.mark = " ",decimal.mark=",") ) )%>%
          mutate_at(vars(matches("evol")), 
                    ~cell_spec(comma(.,digits = 2,big.mark = " ",decimal.mark=",") ) )%>%
          mutate(densite =cell_spec( comma(densite,digits=1,big.mark = " ",decimal.mark=",") ),
                 ind_jeun=cell_spec(comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",") ),
                 propF=cell_spec(comma(propF,digits=1,big.mark = " ",decimal.mark=",") )
          ) ) %>%
  dplyr::rename(Région=REG,Département=DEP,Nom=LIBGEO,"Densité (rang)"=densite,Population=population,
                "Jeunes 15-29"=pop_15_29,"Part 15/29"=part_15_29,"Part 16/25"=part_16_25,
                "Personnes +65a"=pop_65, "évolution annuelle"=evolution,"indice jeunesse (-20a/+60a)"=ind_jeun,
                txfemme=propF)

dep27_tab <- kable(dep27_tab,"html", 
                   align=c("c","c","l","r","r","r","l","l","r",rep("l",7),"c","c"),
                   escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  #column_spec(16, width = "4cm") %>%
  #row_spec(which (departement$REG=='27'),background = "#E0FFFF75" )%>%
  row_spec(9,bold = T,color = "steelblue") %>%
  row_spec(10,bold = T,color = "navy") %>%
  add_header_above(c(" " = 5, "Jeunes" =3, "Pop âgée" = 3, " "= 1, "Jeunes"=2,"Pop âgée"=2," "=2)) %>%
  add_header_above(c(" " = 11, "Evolution 2012-2017" =5, " "= 2)) %>%
  footnote(general = " Insee RP2012, RP2017", general_title = "Source : ", footnote_as_chunk = T) 


#france----


dep_tab <- departement %>% 
  ungroup() %>% mutate(rang=rank(-densite)) %>%
  select(REG,DEP,LIBGEO,population=pop,densite,pop_15_29=p1529,
         part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
         evolution=evol,evol1529,evol1625,evol65,evol75,
         ind_jeun,propF, rang) %>% 
  group_by(REG) %>%   arrange(REG,DEP) %>%
  mutate_at(vars(starts_with("pop")), ~cell_spec(comma(.,digits = 0,big.mark = " ")))%>%
  mutate( densite = ifelse(rang >= 85, 
                           cell_spec(paste0 (comma(densite,digits=1,big.mark = " ",decimal.mark=",") ,
                                             " (",rang,")"),"html",color = "green", bold=T),
                           cell_spec(paste0 (comma(densite,digits=1,big.mark = " ",decimal.mark=",") ,
                                             " (",rang,")"),"html",color = "grey", bold=F) ) ,
          part_15_29=color_bar("gold")
          (comma(part_15_29,digits=1,big.mark = " ",decimal.mark=",")),
          part_16_25=color_bar("gold")
          (comma(part_16_25,digits=1,big.mark = " ",decimal.mark=",")),
          part_65=color_bar("Plum")
          (comma(part_65,digits=1,big.mark = " ",decimal.mark=",")),
          part_75=color_bar("Plum")
          (comma(part_75,digits=1,big.mark = " ",decimal.mark=",")),
          evolution=normalize_bar("lightblue")
          (comma(evolution,digits=2,big.mark = " ",decimal.mark=",")),
          evol1529=normalize_bar("gold")
          (comma(evol1529,digits=2,big.mark = " ",decimal.mark=",")),
          evol1625=normalize_bar("gold")
          (comma(evol1625,digits=2,big.mark = " ",decimal.mark=",")),
          evol65=normalize_bar("Plum")
          (comma(evol65,digits=2,big.mark = " ",decimal.mark=",")),
          evol75=normalize_bar("Plum")
          (comma(evol75,digits=2,big.mark = " ",decimal.mark=",")),
          ind_jeun= color_tile('LightSkyBlue','lightgreen',alpha=0.5)
          (comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",")),
          propF= color_tile('white','pink',alpha=0.5)
          (comma(propF,digits=1,big.mark = " ",decimal.mark=","))
  )    %>%  select(-rang) %>%
  rbind(region %>% filter(REG=='27') %>%
          select(REG,LIBGEO,population=pop,densite,pop_15_29=p1529,
                 part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
                 evolution=evol,evol1529,evol1625,evol65,evol75,
                 ind_jeun,propF ) %>%
          mutate(DEP="BFC")%>%
          mutate_at(vars(matches("pop")),  
                    ~cell_spec(comma(.,digits = 0,big.mark = " ") ) )%>%
          mutate_at(vars(matches("part")), 
                    ~cell_spec(comma(.,digits = 1,big.mark = " ",decimal.mark=",") ) )%>%
          mutate_at(vars(matches("evol")), 
                    ~cell_spec(comma(.,digits = 2,big.mark = " ",decimal.mark=",") ) )%>%
          mutate(densite =cell_spec( comma(densite,digits=1,big.mark = " ",decimal.mark=",") ),
                 ind_jeun=cell_spec(comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",") ),
                 propF=cell_spec(comma(propF,digits=1,big.mark = " ",decimal.mark=",") )
          ) ) %>% 
  rbind(metro %>%
          select(REG,LIBGEO,population=pop,densite,pop_15_29=p1529,
                 part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
                 evolution=evol,evol1529,evol1625,evol65,evol75,
                 ind_jeun,propF) %>%
          mutate(DEP="METRO")%>%
          mutate_at(vars(matches("pop")),  
                    ~cell_spec(comma(.,digits = 0,big.mark = " ") ) )%>%
          mutate_at(vars(matches("part")), 
                    ~cell_spec(comma(.,digits = 1,big.mark = " ",decimal.mark=",") ) )%>%
          mutate_at(vars(matches("evol")), 
                    ~cell_spec(comma(.,digits = 2,big.mark = " ",decimal.mark=",") ) )%>%
          mutate(densite =cell_spec( comma(densite,digits=1,big.mark = " ",decimal.mark=",") ),
                 ind_jeun=cell_spec(comma(ind_jeun,digits=1,big.mark = " ",decimal.mark=",") ),
                 propF=cell_spec(comma(propF,digits=1,big.mark = " ",decimal.mark=",") )
          ) ) %>% 
  dplyr::rename(Région=REG,Département=DEP,Nom=LIBGEO,"Densité (rang)"=densite,Population=population,
                "Jeunes 15-29"=pop_15_29,"Part 15/29"=part_15_29,"Part 16/25"=part_16_25,
                "Personnes +65a"=pop_65, "évolution annuelle"=evolution,"indice jeunesse (-20a/+60a)"=ind_jeun,
                txfemme=propF)%>%
  kable("html", 
        align=c("c","c","l","r","r","r","l","l","r",rep("l",7),"c","c"),
        escape = F) %>%
  kable_styling("hover", full_width = F) %>%
  #column_spec(16, width = "4cm") %>%
  row_spec(which (departement$REG=='27'),background = "#E0FFFF75",  bold = T , color = "orange" )%>%
  row_spec(97,bold = T,color = "steelblue") %>%
  row_spec(98,bold = T,color = "navy") %>%
  add_header_above(c(" " = 5, "Jeunes" =3, "Pop âgée" = 3, " "= 1, "Jeunes"=2,"Pop âgée"=2," "=2)) %>%
  add_header_above(c(" " = 11, "Evolution 2012-2017" =5, " "= 2)) %>%
  footnote(general = " Insee RP2012, RP2017", general_title = "Source : ", footnote_as_chunk = T) 

#  save_kable(dep_tab,file = "P:/projets R/DRAJES/tableaux/dep.html", self_contained = T)
#dep_tab


#  save_kable(dep27_tab,file = "P:/projets R/DRAJES/tableaux/dep27.html", self_contained = T)
# dep27_tab

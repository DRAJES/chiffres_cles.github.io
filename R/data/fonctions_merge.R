pop_basecom <-   function(geo){ 
   basecom %>% 
      group_by({{geo}} ) %>%
     summarise(comm=n(),
              pop=sum(pop,na.rm=T)) %>%
    left_join(appartenance %>%
                filter (NIVGEO == ensym(geo)  )  %>%
 #               select("{{geo}}" := CODGEO,"nom_{{geo}}" := LIBGEO),
                 select("{{geo}}" := CODGEO, "Libellé" := LIBGEO),
              by=names(select(., {{geo}}) )  )  %>%
    relocate("Libellé",.after = 1)
#        adorn_totals("row",name="METRO")
       }

#pop_basecom(REG)

cc_kable <-  function(.tbl,aligne ){ 
  .tbl %>% 
    kable("html", format.args = list(decimal.mark = ",", big.mark = " "),
          align=aligne, escape = F) %>% 
    kable_styling("hover", full_width = F) %>%
    row_spec(str_which (.tbl %>% pull(1) ,"FR|Total|France.|METRO" ) ,
             bold = T,color = "navy")  %>%
    row_spec(str_which (.tbl %>% pull(2) ,"Total|France.|METRO" ) ,
             bold = T,color = "navy")  %>%
    row_spec(str_which (.tbl %>% pull(1) ,"Franche|BFC" ), 
             background = "#E0FFFF75",  bold = T , color = "steelblue") %>%
    row_spec(str_which (.tbl %>% pull(2) ,"Franche|BFC" ), 
             background = "#E0FFFF75",  bold = T , color = "steelblue")

  }


#str_which(epci27_tab$Nom,"Franche")
#which(epci27_tab$Nom == "Bourgogne-Franche-Comté")

#str_which(epci27_tab %>% pull(Nom), c( "BFC")   )

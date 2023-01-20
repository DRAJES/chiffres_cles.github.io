
libelle <- function(.tbl,geo){
  .tbl %>%
    left_join(.,appartenance %>% 
                filter(NIVGEO==rlang::ensym(geo)) %>%
                select("{{geo}}" := CODGEO, "Libellé" := LIBGEO),
              by=names(select(., {{geo}}))  ) %>%
    relocate(Libellé,.after = {{geo}} )
}

pop_basecom <-   function(geo){ 
   basecom %>% 
      group_by({{geo}} ) %>%
     summarise(comm=n(),
              pop=sum(pop,na.rm=T)) %>%
    libelle({{geo}})
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


tab_filtre <- function(geo) {
  
  sd <- tab %>%
    left_join(.,basecom %>%
                dplyr::group_by({{geo}}) %>%
                dplyr::filter(pop==max(pop)) %>% 
                dplyr::select({{geo}},DEP),
              by=names(select(., {{geo}})) 
    )
  sd <- SharedData$new(sd)
  
  filter_select("dep","Département",sd,group=~DEP,allLevels=FALSE,multiple =FALSE)
  
  sd <- sd %>% 
    datatable(escape = FALSE,
              extensions = c('Scroller', 'Buttons'),
              options = list(
                dom = 'Bfrtip',
                buttons = 
                  list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'excel', 'pdf'),
                    text = 'Download'
                  )),
                scroller = TRUE,
                scrollY=600),
              rownames = FALSE) 
    
  return(sd)
}

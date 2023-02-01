
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
    kable("html", escape = FALSE,
          align=aligne,
          format.args = list(decimal.mark = ",", big.mark = " ") ) %>% 
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
                dplyr::filter(REG=='27') %>%
                dplyr::filter(pop==max(pop)) %>% 
                dplyr::select({{geo}},DEP),
              by=names(select(., {{geo}})) 
    ) %>%
    libelle(DEP) %>% select(-DEP)
  
  sd <- SharedData$new(sd)
  return(sd) }
  
datafiltre <- function(sd=sd)  {
  sd %>% 
    datatable(escape = FALSE,
              height = 700,
              extensions = c('Scroller', 'Buttons'),
              options = list(
                dom = 'Bfrtip',
                buttons = 'csv',
                #   list('copy', 'print', list(
                #     extend = 'collection',
                #     buttons = 'csv',
                #     buttons = c('csv', 'excel', 'pdf'),
                #     text = 'Download')),
                # scroller = TRUE,
                #scrollY=600,
                paging = FALSE),
              rownames = FALSE) 
}

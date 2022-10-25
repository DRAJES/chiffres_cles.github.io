basecom <-   function(.tbl){ 
  .tbl %>% 
    left_join(.,basecom %>% 
                group_by(REG) %>%
                summarise(comm=n()) %>% 
                adorn_totals("row",name="METRO"),
              by=c("Région"="REG")) }

appartenance <-   function(.tbl){ 
  .tbl %>% 
    left_join(.,appartenance %>% 
                filter(NIVGEO=="DEP") %>%
                select(CODGEO,département=LIBGEO),
              by=c("depvol"="CODGEO") )  }
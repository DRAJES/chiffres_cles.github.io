RNAcarto <- function(geo=REG){
  RNApop %>% 
    filter (!is.na({{geo}}) & !is.na(objr)) %>% 
    group_by({{geo}},objr) %>% 
    summarise(asso=sum(total,na.rm = T)) %>%
    left_join(.,pop_basecom({{geo}}),
              by=names(select(., {{geo}})) ) %>%
    mutate(txasso=round(asso*100/pop,2)) %>%
    pivot_wider(c({{geo}},Libellé),names_from = objr , values_from = asso) %>%
    adorn_totals("col", name = "total" ) %>%
    select({{geo}},Libellé,total,Culture,Sports,Loisirs,
           Social,Santé,`Amicales-Entraide`,Enseignement,
           Economie,Environnement,Autres,NR)
}

#RNAcarto(REG)

carte_asso <- function(){
carte <-   leaflet(RNAcarto) %>%  
    addPolygons(data=RNAcarto,group  = "régions",
                color = "#2F4F4F", weight = 2, smoothFactor = 2,
                fillColor = ~pal2(100*RNAcarto$total/RNAcarto$pop),fillOpacity  = 0.8,
                popup = sprintf("<center> %s <br>  Nombre d'associations : <b>%i</b> <br>
                               Taux d'association :<b>%.2f</b> </center> ",  
                                RNAcarto$Libellé ,RNAcarto$total,round(100*(RNAcarto$total/RNAcarto$pop ),1) ) )   %>%
    addMinicharts(centroid(RNAcarto)[,1],centroid(RNAcarto)[,2] ,
                  #maxValues = 100,
                  type = "pie",
                  chartdata = RNAcarto@data %>% select("Culture":"NR"),
                  colorPalette = coul,
                  width = 30 *  sqrt(RNAcarto$total / sd(RNAcarto$total)),
                  opacity = 0.7) %>%
    addLegend( pal = pal2 ,values = 100*(RNAcarto$total/RNAcarto$pop),
               position = "bottomright", title = "taux d'associations" )
return(carte)
}

tableau_asso <- function(.tbl){
  .tbl %>% 
    select(1,Libellé,total,txasso,Culture,Sports,Loisirs,
           Social,Santé,`Amicales-Entraide`,Enseignement,
           Economie,Environnement,Autres,NR) %>%
    mutate_at(vars(Culture:NR),~round(.*100/total,1) ) %>%
    mutate_at(vars(Culture:Environnement),~color_tile('transparent','lightgreen')
              (digits(.,1,decimal.mark=",") ) ) %>%
    mutate(txasso=color_bar('gold')
           (digits(txasso ,2,decimal.mark=",")), 
           Autres=color_tile('transparent','lightblue')
           (digits(Autres,1,decimal.mark=",") ),
           NR=color_tile('transparent','lightgrey')
           (digits(NR,1,decimal.mark=",") ) )
}

tableau <- function(.tbl){
  .tbl %>% 
    summarise_if(is.numeric,~sum(.x,na.rm=T)) %>%
    mutate( densite=pop/SUPERF,
            evol=100*((pop/pop_ante)^(1/6)-1),
            evol1529=100*((p1529/p1529_ante)^(1/6)-1),
            evol1625=100*((p1625/p1625_ante)^(1/6)-1),
            evol65=100*((p65/p65_ante)^(1/6)-1),
            evol75=100*((p75/p75_ante)^(1/6)-1),
            propF=100*popf/pop,
            prop1529=100*p1529/pop,
            prop1625=100*p1625/pop,
            prop65=100*p65/pop,
            prop75=100*p75/pop,
            ind_jeun=100*p20/p60,
            partZRR=100*popZRR/pop,
            partQPV=100*popMuniQPV/pop,
            diffjeun=p1529-p1529_ante,
            prop1529F=100*p1529f/p1529,
            prop1625F=100*p1625f/p1625,
            prop65F=100*p65f/p65)
}



bg = function(start, end, color, ...) {
  paste("linear-gradient(90deg,transparent ",percent(start),",",
        color, percent(start), ",", color, percent(end),
        ", transparent", percent(end),")")
} 

pm_color_bar2 <- function(color1 = "lightgreen", color2 = "pink", ...){
  formatter("span",
            style = function(x) style(
              display = "inline-block",
              color = ifelse(x> 0,'green',ifelse(x<0,'red','lightgrey')),
              "text-align" = ifelse(x > 0, 'left', ifelse(x<0, 'right', 'center')),
              "width"='100%',
              "background" = bg(ifelse(x >= 0, 0.5,normalize(x)),
                                ifelse(x >= 0,normalize(x),0.5),
                                ifelse(x >= 0, color1, color2))
            ))
}





# forme <- function(.tbl){
#   .tbl %>% 
#     mutate_at(vars(starts_with("pop")), ~ cell_spec(digits(.,0,big.mark = " ")))%>%
#     mutate(rang= if_else( rank(densite) < 4, 
#                           cell_spec(rank(-densite),"html",color = "green", bold=T) ,
#                           cell_spec(rank(-densite),"html",color = "grey", bold=F)  ) ) %>%
#     mutate(diffjeun=if_else( diffjeun < 0,
#                              cell_spec(digits(round(diffjeun/10,0)*10 ,
#                                               0,big.mark = " "),"html",color = "red"),
#                              cell_spec(digits(round(diffjeun/10,0)*10 ,
#                                               0,big.mark = " "),"html",color = "green")
#     )) %>%
#     mutate( densite =  color_tile('white','grey')
#             (digits(densite,1,big.mark = " ",decimal.mark=",")),
#             part_15_29=color_bar("#98FB98")
#             (digits(part_15_29,1,big.mark = " ",decimal.mark=",")),
#             part_16_25=color_bar("#ADFF2F")
#             (digits(part_16_25,1,big.mark = " ",decimal.mark=",")),
#             part_65=color_bar("Plum")
#             (digits(part_65,1,big.mark = " ",decimal.mark=",")),
#             part_75=color_bar("Plum")
#             (digits(part_75,1,big.mark = " ",decimal.mark=",")),
#             evolution= ifelse(evolution < 0,
#                               normalize_bar("#ff000050")(digits(evolution,2,decimal.mark=",")),
#                               normalize_bar("#00800050")(digits(evolution,2,decimal.mark=",")) ),
#             evol1529=ifelse(evol1529 < 0,
#                             normalize_bar("#FF7F5050")(digits(evol1529,2,decimal.mark=",")),
#                             normalize_bar("#98FB9850")(digits(evol1529,2,decimal.mark=",")) ),
#             evol1625=ifelse(evol1625 < 0,
#                             normalize_bar("#ff634750")(digits(evol1625,2,decimal.mark=",")),
#                             normalize_bar("#ADFF2F50")(digits(evol1625,2,decimal.mark=",")) ),
#             evol65=ifelse(evol65 < 0,
#                           normalize_bar("#D2B48C50")(digits(evol65,2,decimal.mark=",")),
#                           normalize_bar("#dda0dd50")(digits(evol65,2,decimal.mark=",")) ),
#             evol75=normalize_bar("#dda0dd50")
#             (digits(evol75,2,big.mark = " ",decimal.mark=",")),
#             ind_jeun= color_tile('#9B90CE','#F2F0F7',alpha=0.3)
#             (digits(ind_jeun,1,big.mark = " ",decimal.mark=",")),
#             propF= color_tile('white','pink',alpha=0.5)
#             (digits(propF,1,big.mark = " ",decimal.mark=",")),
#             prop65F= color_tile('white','pink',alpha=0.5)
#             (digits(prop65F,1,big.mark = " ",decimal.mark=",")),
#             prop1529F= color_tile('white','pink',alpha=0.5)
#             (digits(prop1529F,1,big.mark = " ",decimal.mark=",")),
#             prop1625F= color_tile('white','pink',alpha=0.5)
#             (digits(prop1625F,1,big.mark = " ",decimal.mark=",")),
#             partZRR=color_bar('lightgreen')
#             (digits(partZRR,1,big.mark = " ",decimal.mark=",")),
#             partQPV=color_bar('orange')
#             (digits(partQPV,1,big.mark = " ",decimal.mark=","))
#     )    %>% relocate(rang, .after=densite )
# }


forme <- function(.tbl){
  .tbl %>% 
    mutate_at(vars(starts_with("pop")), ~ cell_spec(digits(.,0,big.mark = " ")))%>%
    mutate(rang= if_else( rank(densite) < 4, 
                          cell_spec(rank(-densite),"html",color = "green", bold=T) ,
                          cell_spec(rank(-densite),"html",color = "grey", bold=F)  ) ) %>%
    mutate(diffjeun=if_else( diffjeun < 0,
                             cell_spec(digits(round(diffjeun/10,0)*10 ,
                                              0,big.mark = " "),"html",color = "red"),
                             cell_spec(digits(round(diffjeun/10,0)*10 ,
                                              0,big.mark = " "),"html",color = "green")
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
            evolution= pm_color_bar2(color1 = "lightgreen", color2 = "pink")
            (digits(evolution,2,decimal.mark=",")),
            evol1529=pm_color_bar2(color1 = "lightgreen", color2 = "pink")
            (digits(evol1529,2,decimal.mark=",")),
            evol1625=pm_color_bar2(color1 = "lightgreen", color2 = "pink")
            (digits(evol1625,2,decimal.mark=",")),
            evol65=pm_color_bar2(color1 = "lightgreen", color2 = "pink")
            (digits(evol65,2,decimal.mark=",")),
            evol75=normalize_bar("#dda0dd50")
            (digits(evol75,2,big.mark = " ",decimal.mark=",")),
            ind_jeun= color_tile('#9B90CE','#F2F0F7',alpha=0.3)
            (digits(ind_jeun,1,big.mark = " ",decimal.mark=",")),
            propF= color_tile('white','pink',alpha=0.5)
            (digits(propF,1,big.mark = " ",decimal.mark=",")),
            prop65F= color_tile('white','pink',alpha=0.5)
            (digits(prop65F,1,big.mark = " ",decimal.mark=",")),
            prop1529F= color_tile('white','pink',alpha=0.5)
            (digits(prop1529F,1,big.mark = " ",decimal.mark=",")),
            prop1625F= color_tile('white','pink',alpha=0.5)
            (digits(prop1625F,1,big.mark = " ",decimal.mark=",")),
            partZRR=color_bar('lightgreen')
            (digits(partZRR,1,big.mark = " ",decimal.mark=",")),
            partQPV=color_bar('orange')
            (digits(partQPV,1,big.mark = " ",decimal.mark=","))
    )    %>% relocate(rang, .after=densite )
}



noms <- function(.tbl){
  .tbl %>% 
    dplyr::rename(Nom=LIBGEO,"Densité"=densite,Population=population,
                  "15-29 ans"=pop_15_29,"Taux<br>15/29"=part_15_29,"Taux<br>16/25"=part_16_25,
                  "65 ans et +"=pop_65,"Taux<br>>65 ans"=part_65, "Taux<br>>75 ans" = part_75,
                  "évolution annuelle"=evolution,"evo15/29"=evol1529,"evo16/25"=evol1625,
                  "evo>65"=evol65,"evo>75"=evol75,"delta"=diffjeun,
                  "Taux femmes"=propF,"Taux femmes<br>>65 ans"=prop65F,"Taux femmes<br>15/29"=prop1529F,"Taux femmes<br>16/25"=prop1625F)
}          



total <- function(.tbl){
  .tbl %>% 
    select(LIBGEO,population=pop,densite,pop_15_29=p1529,
           part_15_29=prop1529,part_16_25=prop1625,pop_65=p65,part_65=prop65,part_75=prop75,
           evolution=evol,evol1529,evol1625,evol65,evol75,
           ind_jeun,propF,prop1529F,prop1625F,prop65F,partZRR,partQPV,diffjeun) %>%
    mutate_at(vars(matches("pop")),  
              ~cell_spec(digits(.,0,big.mark = " ") ) )%>%
    mutate_at(vars(matches("diffjeun")),  
              ~cell_spec(digits(.,0,big.mark = " ") ) )%>%
    mutate_at(vars(matches("part")), 
              ~cell_spec(digits(.,1,big.mark = " ",decimal.mark=",") ) )%>%
    mutate_at(vars(matches("prop")), 
              ~cell_spec(digits(.,1,big.mark = " ",decimal.mark=",") ) )%>%
    mutate_at(vars(matches("evol")), 
              ~cell_spec(digits(.,2,big.mark = " ",decimal.mark=",") ) )%>%
    mutate(densite =cell_spec( digits(densite,1,big.mark = " ",decimal.mark=",") ),
           rang="-", rang_national="-",
           ind_jeun=cell_spec(digits(ind_jeun,1,big.mark = " ",decimal.mark=",") )
    ) 
}


limites <- function(.tbl){
  .tbl %>% 
  bind_rows(c(part_15_29=0,part_16_25=0,part_65=0,part_75=0,
              evolution=-2,evol1529=-2,evol1625=-2,evol65=2,evol75=-4),
            c(part_15_29=40,part_16_25=40,part_65=40,part_75=40,
              evolution=2,evol1529=2,evol1625=2,evol65=4,evol75=4))
}



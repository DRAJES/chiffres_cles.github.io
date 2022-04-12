


write.xlsx(nature,file = "S:/DRAJES/SUPPORT/04_STATS/Sport/Nature/sportnature2020.xlsx")
write.csv(nature,file = "S:/DRAJES/SUPPORT/04_STATS/Sport/Nature/sportnature2020.csv",fileEncoding = "UTF-8")


icons <- awesomeIconList (
  "407" = makeAwesomeIcon(text =  fa("truck-monster"),markerColor ="darkred",iconColor="#FF0000",squareMarker=F),
  
  "3001" = makeAwesomeIcon(text =  fa("mountain"),iconColor="#2F4F4F",markerColor ="lightgreen"), 
  "3003" = makeAwesomeIcon(text =  fa("child"),iconColor="#2F4F4F",markerColor ="lightgreen"),
  "3004" = makeAwesomeIcon(text  =  fa("walking"),iconColor="#6B8E23",markerColor ="lightgreen")  ,
  "3005" = makeAwesomeIcon(text =  fa("map-signs"),iconColor="#6B8E23",markerColor ="lightgreen")  ,
  "3007" = makeAwesomeIcon(text =  fa("mountain"),iconColor="#2F4F4F",markerColor ="lightgreen"), 
  "3008" = makeAwesomeIcon(text =  fa("icicles"),iconColor="#696969",markerColor ="lightgreen"),
  "3010" = makeAwesomeIcon(text =  fa("mountain"),iconColor="#2F4F4F",markerColor ="lightgreen"), 
  "3011" = makeAwesomeIcon(text =  fa("mountain"),iconColor="#0000ff",markerColor ="lightgreen")  ,
  "3012" = makeAwesomeIcon(text =  fa("hiking"),iconColor="#228B22",markerColor ="lightgreen")  ,
  "3014" = makeAwesomeIcon(text =  fa("child"),iconColor="#2F4F4F",markerColor ="lightgreen"),
  "3015" = makeAwesomeIcon(text =  fa("icicles"),iconColor="#696969",markerColor ="lightgreen"),
  
  "701" = makeAwesomeIcon(text =  fa("skiing"),iconColor="#778899",markerColor ="white"),
  "703" = makeAwesomeIcon(text =  fa("skiing"),iconColor="#696969",markerColor ="white"),
  "705" = makeAwesomeIcon(text =  fa("skiing-nordic"),iconColor="#708090",markerColor ="white"),
  "706" = makeAwesomeIcon(text =  fa("skiing"),iconColor="#696969",markerColor ="white"),
  
  "603" = makeAwesomeIcon(text =  fa("biking"),iconColor="#6A5ACD",markerColor ="orange"),
  "604" = makeAwesomeIcon(text =  fa("biking"),iconColor="#6A5ACD",markerColor ="orange"),
  "605" = makeAwesomeIcon(text =  fa("biking"),iconColor="#6A5ACD",markerColor ="orange"),
  "606" = makeAwesomeIcon(text =  fa("biking"),iconColor="#6A5ACD",markerColor ="orange"),
  
  "906" = makeAwesomeIcon(text =  fa("horse"),iconColor="#F4A460",markerColor ="black"),
  "909" = makeAwesomeIcon(text =  fa("horse"),iconColor="#F4A460",markerColor ="black"),
  
  "2401" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2402" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2403" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2404" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2405" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2406" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2407" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2408" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2409" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2410" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2414" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2416" = makeAwesomeIcon(text =  fa("swimmer"),iconColor="#4169E1",markerColor ="lightblue"),
  "2417" = makeAwesomeIcon(text =  fa("anchor"),iconColor="#0000CD",markerColor ="lightblue"),
  "2418" = makeAwesomeIcon(text =  fa("anchor"),iconColor="#0000CD",markerColor ="lightblue"),
  "2419" = makeAwesomeIcon(text =  fa("water"),iconColor="#4169E1",markerColor ="lightblue"),
  "2420" = makeAwesomeIcon(text =  fa("fish"),iconColor="#0000CD",markerColor ="lightblue"),
  
  "2301" = makeAwesomeIcon(text =  fa("paper-plane"),iconColor="#FFFF00",markerColor ="purple"),
  "2302" = makeAwesomeIcon(text =  fa("paper-plane"),iconColor="#FFFF00",markerColor ="purple"),
  "2303" = makeAwesomeIcon(text =  fa("paper-plane"),iconColor="#FFFF00",markerColor ="purple"),
  "2305" = makeAwesomeIcon(text =  fa("paper-plane"),iconColor="#FFFF00",markerColor ="purple"),
  "2306" = makeAwesomeIcon(text =  fa("paper-plane"),iconColor="#FFFF00",markerColor ="purple") )

#carte des sites de sport nature----
leaflet(nature) %>% 
  setView((min(nature$EquGPSX)+max(nature$EquGPSX))/2,
          (min(nature$EquGPSY)+max(nature$EquGPSY))/2, zoom = 8) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addAwesomeMarkers(data=nature,lng = ~EquGPSX,lat = ~EquGPSY,
                    icon = ~icons[(as.character(EquipementTypeCode) )],
                    popup=popupTable(nature,
                                     row.numbers = F,feature.id = F) ) %>%
  addPolygons(data=reg27carto,fill=F,weight = 4,color = "#008000") %>%
  addPolygons(data=dep27carto,fillOpacity = 0.01, weight = 3,color = "#008000",
              popup = popupTable(dep27carto[,60:68],row.numbers = F,feature.id = F) ) 


#carte des sites de sport nature cluster----
nature.df <- split(nature,nature$EquipementFamille)

depnature <- table(nature$DEP,nature$EquipementFamille) 
depnature <- as.data.frame(addmargins(depnature))
depnature <- spread(depnature,Var2,Freq)
dep27carto <- merge(dep27carto,depnature,by.x="INSEE_DEP",by.y="Var1")

l <- leaflet() %>% 
  setView((min(nature$EquGPSX)+max(nature$EquGPSX))/2,
          (min(nature$EquGPSY)+max(nature$EquGPSY))/2, zoom = 8) %>%
  addProviderTiles("CartoDB.Positron") 

names(nature.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addAwesomeMarkers(data=nature.df[[df]],
                        lng=~EquGPSX, lat=~EquGPSY,
                        label=~as.character(EquipementFamille),
                        icon = ~icons[(as.character(EquipementTypeCode) )],
                        popup=~popupTable(nature.df[[df]],row.numbers = F,feature.id = F),
                        group = df,
                        clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                        labelOptions = labelOptions(noHide = F,
                                                    direction = 'auto'))
  })


l %>%
  addPolygons(data=reg27carto,fill=F,weight = 4,color = "#008000") %>%
  addPolygons(data=dep27carto,fillOpacity = 0.01, weight = 3,color = "#008000", 
              popup = popupTable(dep27carto[,60:68],row.numbers = F,feature.id = F) )%>%
  addLayersControl(
    overlayGroups = names(nature.df),
    options = layersControlOptions(collapsed = FALSE)
  )



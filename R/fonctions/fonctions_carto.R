contour_bfc <-  function(.map){ 
  .map %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 5.1, lat = 47.27, zoom = 6) %>%
    addPolygons(data=reg27carto,group  = "BFC",
                color="#9370DB",weight = 5, opacity = 1, 
                fill = F, smoothFactor = 5) }

contour_depbfc <-   function(.map,zoom = 8){ 
  .map %>% 
    contour_bfc() %>%
    setView(lng = 5.1, lat = 47.27, zoom = zoom) %>%
    addPolygons(data=dep27carto,group  = "BFC",
                color="#9370DB",weight = 3, opacity = 1, 
                fill = F, smoothFactor = 2) }
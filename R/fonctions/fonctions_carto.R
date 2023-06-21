contour_bfc <-  function(.map){ 
  .map %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = 5.1, lat = 47.27, zoom = 6) %>%
    addMapPane("departement", zIndex = 430) %>%
    addPolygons(data=reg27carto,group  = "BFC",
                color="#8b008b",weight = 5, opacity = 1, 
                fill = F, smoothFactor = 5,
                options = pathOptions(pane = "departement")) }

contour_depbfc <-   function(.map,zoom = 8){ 
  .map %>% 
    contour_bfc() %>%
    setView(lng = 5.1, lat = 47.27, zoom = zoom) %>%
    addMapPane("region", zIndex = 420) %>%
    addPolygons(data=dep27carto,group  = "BFC",
                color="#800080",weight = 3, opacity = 1, 
                fill = F, smoothFactor = 2,
                options = pathOptions(pane = "region")) }

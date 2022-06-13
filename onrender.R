leaflet() %>% addTiles() %>%
  onRender("
    function(el, x) {
      // Navigate the map to the user's location
      this.locate({setView: true});
    }
  ")



meh <- "&#x1F610;";
yikes <- "&#x1F628;";

df <- data.frame(
  lng = quakes$long,
  lat = quakes$lat,
  html = ifelse(quakes$mag < 5.5, meh, yikes),
  stringsAsFactors = FALSE
)

leaflet() %>% addTiles() %>%
  fitBounds(min(df$lng), min(df$lat), max(df$lng), max(df$lat)) %>%
  onRender("
    function(el, x, data) {
      for (var i = 0; i < data.lng.length; i++) {
        var icon = L.divIcon({className: '', html: data.html[i]});
        L.marker([data.lat[i], data.lng[i]], {icon: icon}).addTo(this);
      }
    }
  ", data = df)



library(leaflet)
library(htmltools)
library(htmlwidgets)

curvejs <- htmlDependency("Leaflet.curve", "0.9.1",
  src = c(href="https://github.com/elfalem/Leaflet.curve/tree/gh-pages/src/"),
  script = "leaflet.curve.js")


registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

leaflet() %>%   addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 5.1, lat = 47.27, zoom = 3) %>%
  addPolylines(data = flows, 
               weight = ~ifelse(nombre>3,nombre^(1/2), 0),
               label = hover,
               stroke = T, 
               fill = F, fillOpacity = 0.8, dashArray = NULL,
               smoothFactor = 1,
               fillColor = colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(8),
               color = scales::seq_gradient_pal(low = "lightblue", high = "lightgreen", space = "Lab")(seq(0, 1, length.out = 25))
  ) %>%
  registerPlugin(curvejs) %>%
  onRender("function(el, x) {
    var path = L.curve(['M',[50.54136296522163,28.520507812500004],
					'C',[52.214338608258224,28.564453125000004],
						[48.45835188280866,33.57421875000001],
						[50.680797145321655,33.83789062500001],
					'V',[48.40003249610685],
					'L',[47.45839225859763,31.201171875],
						[48.40003249610685,28.564453125000004],'Z'],
					{color:'red',fill:true}).addTo(map);
 }")


migrationLayer <- htmlDependency("Leaflet.curve", "0.9.1",
                          src = c(href="https://github.com/lit-forest/leaflet.migrationLayer/tree/master/dist/"),
                          script = "leaflet.migrationLayer.js")


registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

leaflet() %>%    addProviderTiles(providers$CartoDB.Positron) %>% 
  setView(lng = 5.1, lat = 47.27, zoom = 3) %>%
  addPolylines(data = flows, 
               weight = ~ifelse(nombre>3,nombre^(1/2), 0),
               label = hover,
               stroke = T, 
               fill = F, fillOpacity = 0.8, dashArray = NULL,
               smoothFactor = 1,
               fillColor = colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(8),
               color = scales::seq_gradient_pal(low = "lightblue", high = "lightgreen", space = "Lab")(seq(0, 1, length.out = 25))
  ) %>%
  registerPlugin(migrationLayer) %>%
  onRender("function(el, x) {
 var migrationLayer = new L.migrationLayer({
    map: map,
    data: flows
})
 }")

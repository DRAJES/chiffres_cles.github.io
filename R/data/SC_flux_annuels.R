library(geosphere)
library(rgdal)
library(RColorBrewer)
library(leaflet)
library(tidyverse)

#load("data/engagement/flux.RData")
library(shiny)

#shiny UI
ui <- fluidPage(
  
  sidebarPanel(width = 2,
               
               sliderInput("debut","date de début de contrat",
                           min = 2010,max = 2022,
                           value = c(2010,2022)
               )
               
  ),
  
  splitLayout(
    style = "border: 1px solid silver;",
    cellWidths = 800,
    cellArgs = list(style = "padding: 6px"),
    
    leafletOutput("map", width = "70%", height = "750px"),
    leafletOutput("map2", width = "70%", height = "750px"),
    
    
  )
)



#shiny server
server <- function(input, output, session) {
  
  
  #filter data depending on selected date
  liensfiltre <- reactive({
    req(input$debut)
    
    vol_geo %>% 
      left_join(.,miss_geo,by=c("CTV_NUMERO","CTV_DATE_DEBUT")) %>% 
      filter(as.numeric(substr(CTV_DATE_DEBUT,7,10)) > input$debut[[1]]-1 & 
               as.numeric(substr(CTV_DATE_DEBUT,7,10)) < input$debut[[2]]+1 )  %>%
      group_by(départ,arrivée) %>% # ajouter CTV_DATE_DEBUT ou filtrer sur une année
      count() %>%
      rename(nombre=n)
    
  })
  
  
  #create the base leaflet map
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 5.1, lat = 47.27, zoom = 7) %>%
      addPolygons(data=reg27carto,group  = "BFC",
                  color="#9370DB",weight = 5, opacity = 1, 
                  fill = F, smoothFactor = 5)
    
  })
  
  
  output$map2 <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 5.1, lat = 47.27, zoom = 7) %>%
      addPolygons(data=reg27carto,group  = "BFC",
                  color="#9370DB",weight = 5, opacity = 1, 
                  fill = F, smoothFactor = 5)
    
  })
  
  #prepare data depending on selected date and draw either markers or update polygons depending on the selected map type
  observe({
    
    oursinxy <- left_join(liensfiltre(),cartexy@data %>% 
                            select(INSEE_COM,origine=NOM,origine.x=X1,origine.y=X2),
                          by=c("départ"="INSEE_COM"))
    oursinxy <- left_join(oursinxy,cartexy@data %>% 
                            select(INSEE_COM,destination=NOM,destination.x=X1,destination.y=X2),
                          by=c("arrivée"="INSEE_COM")) %>%
      filter(!is.na(origine.x) & !is.na(destination.x)) 
    
    oursinxy <- oursinxy %>%  
      group_by(destination) %>%
      mutate(missions=sum(nombre),
             rayon=missions*10)
    
    flows <- gcIntermediate(oursinxy[,5:6],oursinxy[,8:9],n=5, sp=T,addStartEnd = T)
    flows$nombre <- oursinxy$nombre
    flows$origine <- oursinxy$origine
    flows$destination <- oursinxy$destination
    
    hover <- paste0(flows$origine, " à ", 
                    flows$destination, ': ', 
                    as.character(flows$nombre))
    
    
    leafletProxy("map") %>%
      clearGroup(group = "SC") %>%
      addCircles(data=oursinxy, ~destination.x, ~destination.y,
                 weight =~oursinxy %>% 
                   group_by(destination) %>% 
                   mutate(sum(nombre)^(1/2)) %>% pull, 
                 label = ~as.character(destination),
                 color="#ffa500", stroke = TRUE, 
                 fillOpacity = 0.5,
                 group = "SC") %>%
      addPolylines(data = flows, 
                   group = "SC",
                   weight = ~ifelse(nombre>3,nombre^(1/2), 0),
                   label = hover,
                   stroke = T,   smoothFactor = 1,
                   fill = F, fillOpacity = 0.8, dashArray = NULL,
                   fillColor = colorRampPalette(c(rgb(0,0,1,1), rgb(0,0,1,0)), alpha = TRUE)(8),
                   color = scales::seq_gradient_pal(low = "lightblue", high = "lightgreen", space = "Lab")
                   (seq(0, 1, length.out = 25)) )
    
    
    leafletProxy("map2") %>%
      clearMinicharts() %>%
      clearFlows %>%
      addFlows( oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(origine.x),
                oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(origine.y),
                oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(destination.x),
                oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(destination.y),
                flow = oursinxy %>% filter(destination!=origine & nombre>3) %>% pull(nombre),
                color = "lightgreen", opacity = 0.6 ) %>%
      addMinicharts( oursinxy %>% filter(destination==origine) %>% pull(destination.x),
                     oursinxy %>% filter(destination==origine) %>% pull(destination.y),
                     chartdata =  oursinxy %>% 
                       filter(destination==origine) %>%
                       ungroup() %>% 
                       mutate(entrant=missions-nombre) %>%
                       select(stable=nombre,entrant),
                     opacity = 0.7,              
                     type = "pie",
                     colorPalette = c("#ffa500", "lightgreen"),
                     width = oursinxy %>% 
                       filter(destination==origine) %>%
                       mutate(missions^(1/2)) %>% pull(12)    )
    
    
  })
  
}



shinyApp(ui, server)  

     
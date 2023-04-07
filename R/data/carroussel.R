### Carroussel
#ne fonctionne pas sur 1.1démo
library (swipeR)
wrapper <- swipeRwrapper(HTML(tab %>%
                                cc_kable(aligne = "clrrrccll") %>%
                                credits() ),
                         leaflet() %>%
                           carto_ZRR() %>%
                           carto() %>%
                           contour_bfc() )
swipeR(
  wrapper, height = "400px", width = "70%", thumbs = TRUE)



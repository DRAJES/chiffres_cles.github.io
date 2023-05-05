thm <- hc_theme_merge(
  hc_theme_gridlight( yAxis = list(
    title = list(
      style = list(
        textTransform = "normal") ) ) ),
  hc_theme(
    #    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  ) 
)
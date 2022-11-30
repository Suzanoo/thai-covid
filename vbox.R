valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

library(highcharter)
vbox_render <- function(value, data, label, chart_type, subtitle, info, icon, color){
  df <- data %>% 
    select(date, matches(label)) %>% 
    set_names(c("x", "y"))
  
  #chart type -> "area", "line", "column"
  hc <- hchart(df, chart_type, hcaes(x, y), name = label)  %>% 
    hc_size(height = 75) %>% 
    hc_credits(enabled = FALSE) %>% 
    hc_add_theme(hc_theme_sparkline_vb()) 
  
  valueBoxSpark(
    value = value,
    title = toupper(label),
    sparkobj = hc,
    subtitle = tagList(HTML("&uarr;"), subtitle),
    info = info,
    icon = icon,
    width = 4,
    color = color,
    href = NULL
  )
}
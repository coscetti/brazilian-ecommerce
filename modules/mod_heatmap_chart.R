################################################################################
# Shiny module heatmap_chart
#
# Author: Simone Coscetti
# Created: 2021-05-25 
################################################################################


# Module constants --------------------------------------------------------



# Module UI ---------------------------------------------------------------

heatmap_chart_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 6, echarts4rOutput(ns("plot_monthly_blogs"), 
                                      height = "500px", 
                                      width = "100%")),
    column(width = 6, echarts4rOutput(ns("orders_location"), 
                                      height = "500px",
                                      width = "100%"))
  )
  
}



# Module logic ------------------------------------------------------------

heatmap_chart <- function(input, output, session, d_data_model) {
  
  output$plot_monthly_blogs <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$data_hm %>%
      e_charts(wd) %>%
      e_heatmap(hour, count, 
                itemStyle = list(emphasis = list(shadowBlur = 10))) %>%
      e_visual_map(count, 
                   inRange = list(color=c("#a5e7f0", 
                                          "#59c4e6", 
                                          "#516b91")),
                   orient = 'vertical', right = '0%', bottom = 'center') %>%
      e_theme("vintage") %>%
      e_title("When and Where",
              subtext = "Hourly distribution of orders on each day of the week", 
              left = "center")
    
  })
  
  my_scale <- function(x){
    scales::rescale(x, to = c(3, 7))
  }
  
  output$orders_location <- renderEcharts4r({
    req(d_data_model())
    
    orders_location <- d_data_model()$orders_location
    
    orders_location <- orders_location[sample(nrow(orders_location), 2000),]
    
    orders_location %>% 
      e_charts(lng) %>% 
      em_map("Brazil") %>%
      e_geo(roam = TRUE, 
            map = "Brazil") %>%
      e_scatter(lat, tot_ord, 
                bind = city,
                coord_system = "geo", 
                legend = FALSE,
                scale=my_scale) %>% 
      e_visual_map(tot_ord, 
                   scale=my_scale, 
                   inRange = list(color=c("#a5e7f0", 
                                          "#59c4e6", 
                                          "#516b91")),
                   orient = 'vertical', 
                   right = '0%', 
                   bottom = 'center') %>%
      e_tooltip(formatter = htmlwidgets::JS("
              function(params){
                return('<strong>' + params.name + 
                        '</strong><br />Orders: ' + params.value[2])
                } 
              ")) %>%
      e_theme("vintage") %>%
      e_title(subtext = "Where does most orders come from?",
              left = "center")
    
  })
  
 
}  

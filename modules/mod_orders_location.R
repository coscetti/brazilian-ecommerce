################################################################################
# Shiny module orders_location
#
# Author: Simone Coscetti
# Created: 2021-05-25 
################################################################################


# Module UI ---------------------------------------------------------------

orders_location_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width=12, echarts4rOutput(ns("orders_location"), 
                                     height = "500px",
                                     width = "100%"))
  )
  
}



# Module logic ------------------------------------------------------------

orders_location <- function(input, output, session, d_data_model) {
  
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
                return('Orders: ' + params.value[2])
                } 
              ")) %>%
      e_theme("vintage") %>%
      e_title(subtext = "Where does most orders come from?",
              left = "center")
    
  })
  
  
}  
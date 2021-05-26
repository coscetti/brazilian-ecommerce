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
    column(width = 12, echarts4rOutput(ns("plot_monthly_blogs"), 
                                       height = "500px")),
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
  
 
}  

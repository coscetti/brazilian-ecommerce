################################################################################
# Shiny module insights_charts
#
# Author: Stefan Schliebs
# Created: 2020-03-16 09:13:12
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

heatmap_chart_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 12, echarts4rOutput(ns("plot_monthly_blogs"), 
                                       height = "800px")),
    # column(width = 4, echarts4rOutput(ns("plot_n_words"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_day_of_week"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_acc_author"), height = "250px")),
  )
  
}



# Module logic ------------------------------------------------------------

heatmap_chart <- function(input, output, session, d_data_model) {
  
  output$plot_monthly_blogs <- renderEcharts4r({
    req(d_data_model())

    d_data_model()$data_hm %>%
      e_charts(wd) %>%
      e_heatmap(hour, count, itemStyle = list(emphasis = list(shadowBlur = 10))) %>%
      e_visual_map(count)
    
  })
  
 
}  

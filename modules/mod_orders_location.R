################################################################################
# Shiny module insights_charts
#
# Author: Stefan Schliebs
# Created: 2020-03-16 09:13:12
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

orders_location_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 12, rbokehOutput(ns("orders_location"),
                                    height = "500px",
                                    width = "100%"))
    # column(width = 12, echarts4rOutput(ns("plot_monthly_blogs"), 
    #                                    height = "800px")),
    # column(width = 4, echarts4rOutput(ns("plot_n_words"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_day_of_week"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_acc_author"), height = "250px")),
  )
  
}



# Module logic ------------------------------------------------------------

orders_location <- function(input, output, session, d_data_model) {
  
  output$orders_location <- renderRbokeh({
    req(d_data_model())
    
    orders_location <- d_data_model()$orders_location
    
    factpal <- colorNumeric("viridis", orders_location$tot_ord)
    figure(width = 800, height = 700, padding_factor = 0) %>%
      ly_map("world", regions = 'brazil', col = "gray") %>%
      ly_points(lng, lat, data = orders_location, size = 5,
                color = factpal(tot_ord),
                hover = c(city, customer_state, tot_ord))
      
    
  })
  
  
}  
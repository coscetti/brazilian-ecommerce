################################################################################
# Shiny module insights_charts
#
# Author: Stefan Schliebs
# Created: 2020-03-16 09:13:12
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

wordcloud_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 4, echarts4rOutput(ns("wordcloud"), 
                                       height = "500px")),
    column(width = 4, echarts4rOutput(ns("wordcloud_good"), 
                                      height = "500px")),
    column(width = 4, echarts4rOutput(ns("wordcloud_bad"), 
                                      height = "500px")),
    # column(width = 4, echarts4rOutput(ns("plot_n_words"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_day_of_week"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_acc_author"), height = "250px")),
  )
  
}



# Module logic ------------------------------------------------------------

wordcloud <- function(input, output, session, d_data_model) {
  
  output$wordcloud <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$all_reviews %>%
      e_color_range(freq, color) %>%
      e_charts() %>%
      # e_cloud(term, beta, shape = "circle", sizeRange = c(10, 48), width = "90%", height = "70%", rotationRange = c(0, 0)) %>%
      e_cloud(word, freq, 
              shape = "circle", 
              sizeRange = c(10, 30), 
              rotationRange = c(0, 0)) %>%
      e_title(subtext = "Word frequencies - all reviews", left = "center") %>% 
      e_theme("walden")
  })
  
  output$wordcloud_good <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$good_reviews %>%
      e_color_range(freq, color) %>%
      e_charts() %>%
      # e_cloud(term, beta, shape = "circle", sizeRange = c(10, 48), width = "90%", height = "70%", rotationRange = c(0, 0)) %>%
      e_cloud(word, freq, 
              shape = "circle", 
              sizeRange = c(10, 30), 
              rotationRange = c(0, 0)) %>%
      e_title(subtext = "Word frequencies - good reviews", left = "center") %>% 
      e_theme("walden")
  })
  
  output$wordcloud_bad <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$bad_reviews %>%
      e_color_range(freq, color) %>%
      e_charts() %>%
      # e_cloud(term, beta, shape = "circle", sizeRange = c(10, 48), width = "90%", height = "70%", rotationRange = c(0, 0)) %>%
      e_cloud(word, freq, 
              shape = "circle", 
              sizeRange = c(10, 30), 
              rotationRange = c(0, 0)) %>%
      e_title(subtext = "Word frequencies - bad reviews", left = "center") %>% 
      e_theme("walden")
  })
}  
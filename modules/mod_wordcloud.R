################################################################################
# Shiny module wordcloud
#
# Author: Simone Coscetti
# Created: 2021-05-25 
################################################################################


# Module constants --------------------------------------------------------



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
  )
  
}



# Module logic ------------------------------------------------------------

wordcloud <- function(input, output, session, d_data_model) {
  
  output$wordcloud <- renderEcharts4r({
    req(d_data_model())
    
    d_data_model()$all_reviews %>%
      e_color_range(freq, color) %>%
      e_charts() %>%
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
      e_cloud(word, freq, 
              shape = "circle", 
              sizeRange = c(10, 30), 
              rotationRange = c(0, 0)) %>%
      e_title(subtext = "Word frequencies - bad reviews", left = "center") %>% 
      e_theme("walden")
  })
}  
################################################################################
# Shiny module insights_charts
#
# Author: Stefan Schliebs
# Created: 2020-03-16 09:13:12
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

orders_time_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 2, sliderInput(ns("date_slider"), label = "Cut Date", 
                                  min = 0.6, max = 0.95, value = 0.7)),
    column(width = 5, plotlyOutput(ns("orders_over_time"), 
                                 width="100%", height="500px")),
    column(width = 5, plotlyOutput(ns("orders_over_time_weekly"), 
                                    width="100%", height="500px")),
    # column(width = 10, plotlyOutput(ns("orders_over_time_monthly"), 
    #                                 width="100%", height=500)),
    
    # column(width = 4, echarts4rOutput(ns("plot_n_words"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_day_of_week"), height = "250px")),
    # column(width = 4, echarts4rOutput(ns("plot_acc_author"), height = "250px")),
  )
  
}



# Module logic ------------------------------------------------------------

orders_time <- function(input, output, session, d_data_model) {
  
      
  output$orders_over_time <- renderPlotly({
    req(d_data_model())
    
    ts_days = d_data_model()$ts_days
    
    ts_train = ts_days$Frequency[1:floor(nrow(ts_days)*input$date_slider)]
    ts_test = ts_days$Frequency[(floor(nrow(ts_days)*input$date_slider)+1) : 
                                  nrow(ts_days)]
    
    fit <- arima(
      x = ts_train, 
      order = c(6, 2, 6)
    )
    
    pred <- predict(
      object = fit, 
      n.ahead = nrow(ts_days)-length(ts_train))
    
    ts_days$Pred = c(ts_train, pred$pred) 
    
    gg_days = ggplot(ts_days) + 
      geom_smooth(aes(x=Days, y=Frequency, color="Data"), span=0.3, size=1.5, alpha=0.3) +
      geom_smooth(aes(x=Days, y=Pred, color="Prediction"), span=0.3, size=1.5, alpha=0.3, se=FALSE) + 
      geom_vline(xintercept=ts_days$Days[floor(length(ts_train)*0.8)]) +
      labs(
        title="Daily purchases time series",
        subtitle="",
        caption = "",
        x="Date",
        y="Frequency") + 
      theme(legend.position="top")
    # grid.arrange(ggdays, nrow=1)
    ggplotly(gg_days)
  })
  
  
  output$orders_over_time_weekly <- renderPlotly({
    req(d_data_model())
    
    ts_weeks = d_data_model()$ts_weeks
    
    ts_train = ts_weeks$Frequency[1:floor(nrow(ts_weeks)*input$date_slider)]
    ts_test = ts_weeks$Frequency[(floor(nrow(ts_weeks)*input$date_slider)+1) : 
                                  nrow(ts_weeks)]
    
    fit <- arima(
      x = ts_train, 
      order = c(6, 2, 6)
    )
    
    pred <- predict(
      object = fit, 
      n.ahead = nrow(ts_weeks)-length(ts_train))
    
    ts_weeks$Pred = c(ts_train, pred$pred) 
    
    gg_weeks = ggplot(ts_weeks) + 
      geom_smooth(aes(x=Weeks, y=Frequency, color="Data"), span=0.3, size=1.5, alpha=0.3) +
      geom_smooth(aes(x=Weeks, y=Pred, color="Prediction"), span=0.3, size=1.5, alpha=0.3, se=FALSE) + 
      geom_vline(xintercept=ts_weeks$Weeks[floor(length(ts_train)*0.85)]) +
      labs(
        title="Weekly purchases time series and prediction",
        subtitle="",
        caption = "",
        x="Date",
        y="") + 
      theme(legend.position="top")
    ggplotly(gg_weeks)
    
  })

  
  
}  

################################################################################
# Shiny module score_prediction
#
# Author: Simone Coscetti
# Created: 2021-05-25 
################################################################################


# Module constants --------------------------------------------------------

# F_COUNT_ICON_TEMPLATE <- "www/modules/count_icon/index.html"


# Module UI ---------------------------------------------------------------

score_prediction_ui <- function(id, icon = "icon-wallet", icon_text = "") {
  ns <- NS(id)
  
  fluidRow(
    column(width = 4, 
           sliderInput(ns("delivered"), label = "Delivering time: ", 
                                  min = 1, max = 250, value = 120),
           sliderInput(ns("photos"), label = "Photos: ", 
                       min = 0, max = 20, value = 3),
           sliderInput(ns("payment"), label = "Payment sequential: ", 
                       min = 1, max = 13, value = 4),
           sliderInput(ns("length"), label = "Review length: ", 
                       min = 0, max = 300, value = 100)),
    
    column(width = 8, 
           tableOutput(ns("score_prediction1")),
           plotlyOutput(ns("plot_ly")))
  )
}



# Module logic ------------------------------------------------------------

score_prediction <- function(input, output, session, d_data_model) {
  
  tbl_glmer_no_scale <- reactive({
    req(d_data_model())
    
    tbl_glmer_no_scale <- d_data_model()$tbl_glmer_no_scale
    return(tbl_glmer_no_scale)
    
  })
    
    
  
  
  
  
  df_pred <- reactive ({
    req(d_data_model())
    
    tbl_glmer <- d_data_model()$tbl_glmer
    df_glmer <- as.data.frame(tbl_glmer)
  
    dlv <- mean(tbl_glmer_no_scale()$delivered)
    pqt <- mean(tbl_glmer_no_scale()$photos_qty)
    tlg <- mean(tbl_glmer_no_scale()$text_length)
    psq <- mean(as.numeric(tbl_glmer_no_scale()$payment_sequential))
    sd_dlv <- sd(tbl_glmer_no_scale()$delivered)
    sd_pqt <- sd(tbl_glmer_no_scale()$photos_qty)
    sd_tlg <- sd(tbl_glmer_no_scale()$text_length)
    sd_psq <- sd(as.numeric(tbl_glmer_no_scale()$payment_sequential))
    
      
    df_pred = df_glmer[1, ]  
    df_pred$delivered = (input$delivered - dlv)/sd_dlv
    df_pred$photos_qty = (input$photos - pqt)/sd_pqt
    df_pred$payment_sequential = (input$payment - psq)/sd_psq
    df_pred$text_length = (input$length - tlg)/sd_tlg
    return(df_pred)
  })
  
  pred <- reactive ({
    req(d_data_model())
    
    mod_glmer <- d_data_model()$mod_glmer
    xx <- predict(
      object = mod_glmer,
      newdata = df_pred(),
      type="response",
      allow.new.levels = TRUE)
    out = paste0(round(100*xx, digits=2), " %") #ifelse(xx>0.5, 1, 0) #output binario da visualizzare
    newlist <- list("out"=out, "xx"=xx)
    return(newlist)
    
  })
  
  output$score_prediction1 <- renderText({
    req(d_data_model())
    
    paste(h3("Score prediction: ", pred()$out))
  })
  
  
  output$plot_ly <- renderPlotly({
    req(d_data_model())
    tbl_pred <- d_data_model()$tbl_pred
    
    plot_ly(
      tbl_pred, 
      x = ~delivered, 
      y = ~photos_qty, 
      z = ~payment_sequential, 
      color = ~xx) %>%
      add_markers(size = 2) %>%
      layout(scene = list(
        xaxis = list(title = 'Delivered (days)'),
        yaxis = list(title = 'Photos Nr'),
        zaxis = list(title = 'Payment Installments')
      )
      )
  })
  
  
  
}  
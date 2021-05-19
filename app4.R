#For the heatmap see
# https://www.datanovia.com/en/lessons/heatmap-in-r-static-and-interactive-visualization/


##### PREAMBLE #####

cat("\f")
rm(list=ls())

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(lme4) #questo forse non serve qui
library(lubridate)
library(tm)
library(wordcloud)
library(memoise)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(tsibble)
library(gridExtra)
library(rbokeh)
library(visNetwork)

load("shiny_app5.RData")

# load("./prediction_new.RData")
# load("./m13m45.RData")

dlv <- mean(tbl_glmer_no_scale$delivered)
pqt <- mean(tbl_glmer_no_scale$photos_qty)
tlg <- mean(tbl_glmer_no_scale$text_length)
psq <- mean(as.numeric(tbl_glmer_no_scale$payment_sequential))
sd_dlv <- sd(tbl_glmer_no_scale$delivered)
sd_pqt <- sd(tbl_glmer_no_scale$photos_qty)
sd_tlg <- sd(tbl_glmer_no_scale$text_length)
sd_psq <- sd(as.numeric(tbl_glmer_no_scale$payment_sequential))
# dly <- mean(tbl_glmer_no_scale$delay)
# sd_dly <- sd(tbl_glmer_no_scale$delay)

df_glmer <- as.data.frame(tbl_glmer)

# orders_by_day <- data.table(x = orders_by_day)

# tbl_orders_df <- as.data.frame(tbl_orders)

# getTermMatrix <- function(){
#   sort(rowSums(m), decreasing = TRUE) 
# }
# 
# getTermMatrix13 <- function() {
#   sort(rowSums(m13), decreasing = TRUE) 
# }
# 
# getTermMatrix45 <- function() {
#   sort(rowSums(m45), decreasing = TRUE) 
# }


### preparo i dati per prediction
tbl_orders$order_purchase_timestamp = 
   as.Date(tbl_orders$order_purchase_timestamp)

ts_days = tbl_orders %>%
   ungroup() %>%
   dplyr::select(Days = order_purchase_timestamp) %>% 
   group_by(Days) %>%
   summarise(Frequency = n()) %>%
   arrange(Days)

# library(tsibble) #yearweek
ts_weeks = tbl_orders %>%
   ungroup() %>%
   dplyr::select(order_purchase_timestamp) %>%
   mutate(Weeks=yearweek(tbl_orders$order_purchase_timestamp)) %>% 
   group_by(Weeks) %>%
   summarise(Frequency = n()) %>%
   arrange(Weeks)

ts_months = tbl_orders %>%
   ungroup() %>%
   dplyr::select(order_purchase_timestamp) %>%
   mutate(Months=yearmonth(tbl_orders$order_purchase_timestamp)) %>% 
   group_by(Months) %>%
   summarise(Frequency = n()) %>%
   arrange(Months)





##### UI #####

ui <- dashboardPage(
   dashboardHeader(title = "Olist e-commerce Data"),
   dashboardSidebar(
      sidebarMenu(
         menuItem("Overview", tabName = "overview"),
         menuItem("Data", tabName = "rawdata"), 
         # menuItem("Geo", tabName = "geo"),
         menuItem("Prediction", tabName = "prediction")
      )
   ),
   dashboardBody(
      tabItems(
         tabItem(
            "overview",
            fluidRow(
               valueBoxOutput("tot_orders", width = 3), # ordini totali
               valueBoxOutput("avg_trans_day", width = 3), # valore transazioni al giorno
               valueBoxOutput("tot_cust", width = 3), # numero di customer
               valueBoxOutput("tot_sell", width = 3)), # numero di seller
            fluidRow(
               box( # heat map vendite per ora del giorno per giorno della settimana --> nb width = 6
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Frequency of orders during the hour over the day",
                  plotOutput(
                     "heat_map_hd", width="100%", height=400))),
            fluidRow(
               box(
                  width = 3, 
                  radioButtons(
                     "radio", "Review Score",
                     choices = list("1-2-3" = 1, "4-5" = 2),
                     selected = 1)),
               box( # word cloud reviews --> nb width = 6
                  width = 6, status = "info", solidHeader = TRUE,
                  title = "Customer Reviews word cloud",
                  plotOutput("cloud", width="100%", height=400)),
               box(
                  width = 3,
                  sliderInput("freq",
                              "Minimum Frequency:",
                              min = 1,  max = 50, value = 25)),
               box(
                  width = 3,
                  sliderInput("max",
                              "Maximum Number of Words:",
                              min = 1,  max = 500,  value = 100)))
         ),
         tabItem(
            "rawdata",
            fluidRow(
               box(
                  width = 8, status = "info", 
                  solidHeader = TRUE,
                  title = "Orders over time", 
                  plotOutput(
                     "orders_time", width="100%", height=500)),
               box(
                  width = 4,
                  sliderInput(
                     "slider1", label = "Cut Date",#, label = h3("Cut Date"), 
                     min = 0.6, max = 0.95, value = 0.7)),
               box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Orders volume distribution", 
                  # leafletOutput("map", height = 500)  
                  rbokehOutput("rbokeh")))
         ),
         tabItem(
            "prediction",
            fluidRow(
               box( # network
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Customer Network",
                  footer = "Customers are linked if they both buy at least the same 2 products",
                  visNetworkOutput("network"))),
            
            sidebarLayout(
               sidebarPanel(
                  sliderInput("delivered",
                              "Delivering time: ",
                              min=1, max=250, value=120), 
                  sliderInput("photos",
                              "Photos: ",
                              min=0, max=20, value=3),
                  sliderInput("payment", 
                              "Payment sequential: ",
                              min=1, max=13, value=4),
                  sliderInput("length",
                              "Review length",
                              min = 0,
                              max = 300, 
                              value = 100)),
               mainPanel(
                  tableOutput("score_prediction"),    
                  plotlyOutput("plot_ly"))
            )
            
         )
         
      )
   )
)



##### SERVER #####  

server <- function(input, output) { 
   
   output$tot_orders <- renderValueBox({
      n_orders <- length(unique(df_hm$order_id)) #103652 #sum(heat_map$count)
      
      valueBox(
         value = formatC(n_orders, digits = 0, format = "f"),
         subtitle = "Total Orders (2016/09 - 2018/09)",
         icon = icon("inbox"),
         color = "red")})    
   
   output$avg_trans_day <- renderValueBox({
      t1 <- min(df_hm$order_purchase_timestamp)
      t2 <- max(df_hm$order_purchase_timestamp)
      delta_t <- difftime(t2, t1, units = "days")
      avg_price <- sum(df_hm$price) / 
         as.numeric(delta_t, units="days")
      valueBox(
         value = formatC(avg_price, digits = 2, format = "f"),
         subtitle = "Avg transaction per day (in R$)",
         icon = icon("dollar-sign"),
         color = "yellow")})
   
   output$tot_cust <- renderValueBox({
      tot_customers <- length(unique(df_hm$customer_id))
      valueBox(
         value = formatC(tot_customers, digits = 0, format = "f"),
         subtitle = "Unique customers",
         icon = icon("user"),
         color = "green")})
   
   output$tot_sell <- renderValueBox({
      tot_sellers <- 3095 #nrow(db_sellers)
      valueBox(
         value = formatC(tot_sellers, digits = 0, format = "f"),
         subtitle = "Sellers",
         icon = icon("warehouse"),
         color = "blue")})

   output$heat_map_hd <- renderPlot({
      print(pp_heat)})

   terms <- reactive({
      # Change when the "update" button is pressed...
      # input$update
      # ...but not for anything else
      if(input$radio == 1) {
         isolate({
            withProgress({
               setProgress(message = "Processing corpus...")
               d13})})
      }
      else d45})
   
   wordcloud_rep <- repeatable(wordcloud)
   
   output$cloud <- renderPlot({
      d <- terms()
      wordcloud_rep(d$word, d$freq, scale=c(4,0.5),
                    min.freq = input$freq, max.words=input$max,
                    random.order=FALSE, 
                    colors=brewer.pal(8, "Dark2"))})
   
   subData <- reactive({
      tbl_orders_df %>%
         filter(
            as.Date(tbl_orders$order_purchase_timestamp) >= 
               as.Date(input$range[1], "%Y-%m-%d"),
            as.Date(tbl_orders$order_purchase_timestamp) <= 
               as.Date(input$range[2], "%Y-%m-%d"))
      })
   
   output$rate_par <- reactive({
      input$slider1})
   
   output$orders_time <- renderPlot({
      ts_train = 
         ts_days$Frequency[1:floor(nrow(ts_days)*input$slider1)]
      ts_test = 
         ts_days$Frequency[(floor(nrow(ts_days)*input$slider1)+1) : nrow(ts_days)]
      fit <- arima(
         x = ts_train, 
         order = c(6, 2, 6)
         # seasonal = list(order=c(0, 1, 1), period=12)
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
      
      #Weekly
      ts_train = ts_weeks$Frequency[1:floor(nrow(ts_weeks)*input$slider1)]
      ts_test = ts_weeks$Frequency[(floor(nrow(ts_weeks)*input$slider1)+1) : nrow(ts_weeks)]
      fit <- arima(
         x = ts_train, 
         order = c(6, 2, 6)
         # seasonal = list(order=c(0, 1, 1), period=12)
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
      
      #monthly
      ts_train = ts_months$Frequency[1:floor(nrow(ts_months)*input$slider1)]
      ts_test = ts_months$Frequency[(floor(nrow(ts_months)*input$slider1)+1) : nrow(ts_months)]
      fit <- arima(
         x = ts_train, 
         order = c(6, 2, 6)
         # seasonal = list(order=c(0, 1, 1), period=12)
      )
      pred <- predict(
         object = fit, 
         n.ahead = nrow(ts_months)-length(ts_train))
      ts_months$Pred = c(ts_train, pred$pred) 
      gg_months = ggplot(ts_months) + 
         geom_smooth(aes(x=Months, y=Frequency, color="Data"), span=0.3, size=1.5, alpha=0.3) +
         geom_smooth(aes(x=Months, y=Pred, color="Prediction"), span=0.3, size=1.5, alpha=0.3, se=FALSE) + 
         geom_vline(xintercept=ts_months$Months[floor(length(ts_train)*0.85)]) +
         labs(
            title="Monthly purchases time series and prediction",
            subtitle="",
            caption = "",
            x="Date",
            y="") + 
         theme(legend.position="top")
      grid.arrange(gg_days, gg_weeks, gg_months, nrow = 1)
      
   })  
   
   data <- reactive({
      x <- orders_location
   })

   output$rbokeh <- renderRbokeh({
      # invalidateLater(1000, session)
      factpal <- colorNumeric("viridis", orders_location$tot_ord)
      figure(width = 800, height = 700, padding_factor = 0) %>%
         ly_map("world", regions = 'brazil', col = "gray") %>%
         ly_points(lng, lat, data = orders_location, size = 5,
                   color = factpal(tot_ord),
                   hover = c(city, customer_state, tot_ord))})
   
   output$network <- renderVisNetwork({
      print(pp_network)})
   
   df_pred <- reactive ({
      df_pred = df_glmer[1, ]  
      df_pred$delivered = (input$delivered - dlv)/sd_dlv
      df_pred$photos_qty = (input$photos - pqt)/sd_pqt
      df_pred$payment_sequential = (input$payment - psq)/sd_psq
      df_pred$text_length = (input$length - tlg)/sd_tlg
      return(df_pred)
   })
   
   pred <- reactive ({
      xx <- predict(
         object = mod_glmer,
         newdata = df_pred(),
         type="response",
         allow.new.levels = TRUE)
      out = paste0(round(100*xx, digits=2), " %") #ifelse(xx>0.5, 1, 0) #output binario da visualizzare
      newlist <- list("out"=out, "xx"=xx)
      return(newlist)
      
   })
   
   
   output$score_prediction <- renderText({
      paste(h3("Score prediction: ", pred()$out))
   })
   
   output$plot_ly <- renderPlotly({
      plot_ly(
         tbl_pred, 
         x = ~delivered, 
         y = ~photos_qty, 
         z = ~payment_sequential, 
         color = ~xx) %>%
         add_markers(size = 3) %>%
         layout(scene = list(
            xaxis = list(title = 'Delivered (days)'),
            yaxis = list(title = 'Photos Nr'),
            zaxis = list(title = 'Payment Installments')))})

}





##### SHINY APP #####
shinyApp(ui, server)

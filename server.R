server <- function(input, output, session) { 
  
  d_data_model <- reactive({ readRDS("app-data-model4.rds")})
  
  callModule(count_icon, "blog_counter", d_data_model, "n_orders")
  callModule(count_icon, "n_items", d_data_model, "n_items")
  callModule(count_icon, "avg_price", d_data_model, "avg_price")
  callModule(count_icon, "n_sellers", d_data_model, "n_sellers")
  
  
  callModule(heatmap_chart, "heatmap_chart", d_data_model)
  callModule(orders_time, "orders_time", d_data_model)
  callModule(orders_location, "orders_location", d_data_model)
  callModule(wordcloud, "wordcloud", d_data_model)
  
  
}
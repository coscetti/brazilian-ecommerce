server <- function(input, output, session) { 
  
  d_data_model <- reactive({ readRDS("app-data-model.rds")})
  
  callModule(count_icon, "blog_counter", d_data_model, "n_orders")
  callModule(count_icon, "avg_price", d_data_model, "avg_price")
  callModule(count_icon, "tot_customers", d_data_model, "tot_customers")
  
  
  callModule(heatmap_chart, "heatmap_chart", d_data_model)
  
}
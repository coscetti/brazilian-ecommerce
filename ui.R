## ui.R ##
htmlTemplate(filename = "www/index.html", 
             name="component1",
             blog_counter = count_icon_ui("blog_counter", 
                                          icon="icon-basket", 
                                          icon_text="Orders"),
             avg_price = count_icon_ui("avg_price", 
                                       icon="icon-wallet", 
                                       icon_text="Average cost (R$)"),
             tot_customers = count_icon_ui("tot_customers", 
                                           icon="icon-profile-female", 
                                           icon_text="Customers"),
             heatmap_chart = heatmap_chart_ui("heatmap_chart")
             
             )

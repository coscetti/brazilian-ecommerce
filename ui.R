## ui.R ##
htmlTemplate(filename = "www/index.html", 
             name="component1",
             blog_counter = count_icon_ui("blog_counter", 
                                          icon="icon-basket", 
                                          icon_text="Orders"),
             n_items = count_icon_ui("n_items", 
                                  icon="icon-tools", 
                                  icon_text="Items sold"),
             avg_price = count_icon_ui("avg_price", 
                                       icon="icon-wallet", 
                                       icon_text="Avg. daily exp. (R$)"),
             n_sellers = count_icon_ui("n_sellers", 
                                           icon="icon-profile-female", 
                                           icon_text="Sellers"),
             heatmap_chart = heatmap_chart_ui("heatmap_chart"),
             
             orders_time = orders_time_ui("orders_time"),
             
             orders_location = orders_location_ui("orders_location"),
             
             wordcloud = wordcloud_ui("wordcloud")
)

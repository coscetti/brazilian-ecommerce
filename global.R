library(dplyr)
library(echarts4r)
library(echarts4r.maps)
library(ggplot2)
library(htmlwidgets)
library(leaflet)
library(lme4)
library(plotly)
library(shiny)
library(tidyr)
library(tm)
library(tsibble)


# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")


# Modules -----------------------------------------------------------------

source("modules/mod_count_icon.R")
source("modules/mod_heatmap_chart.R")
source("modules/mod_orders_time.R")
# source("modules/mod_orders_location.R")
source("modules/mod_wordcloud.R")
source("modules/mod_score_prediction.R")
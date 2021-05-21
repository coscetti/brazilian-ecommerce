library(shiny)
library(echarts4r)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(rbokeh)
library(leaflet)
library(tsibble)
library(tm)

# load("./shiny_app5.RData")

# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")

# Modules -----------------------------------------------------------------

source("modules/mod_count_icon.R")
source("modules/mod_insights_charts.R")
source("modules/mod_orders_time.R")
source("modules/mod_orders_location.R")
source("modules/mod_wordcloud.R")
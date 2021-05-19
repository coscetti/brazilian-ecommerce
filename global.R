library(shiny)
library(echarts4r)
library(dplyr)
library(tidyr)

# load("./shiny_app5.RData")

# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")

# Modules -----------------------------------------------------------------

source("modules/mod_count_icon.R")
source("modules/mod_insights_charts.R")
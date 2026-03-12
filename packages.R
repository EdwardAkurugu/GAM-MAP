
# Loading Packages

library(shiny)
library(sf)
library(shinydashboard)
library(shinyjs)
library(shinyBS)        # bsTooltip() used in UI
library(DT)             # DTOutput, renderDataTable
library(plotly)         # ggplotly, renderPlotly, plotlyOutput
library(ggplot2)        # all ggplot calls
library(dplyr)          # filter, mutate, arrange, group_by, summarise
library(tidyr)          # pivot_wider in seasonality test
library(lubridate)      # year(), month()
library(mgcv)           # gam, qq_plot
library(gratia)         # draw() for GAM smooth plots
library(gridExtra)      # grid.arrange in heatmaps
library(markdown)       # includeMarkdown
library(htmltools)      # tags$caption
library(stats)          # ts(), stl(), kruskal.test(), friedman.test()
library(tidyverse)      # covers ggplot2, dplyr, tidyr, lubridate, readr



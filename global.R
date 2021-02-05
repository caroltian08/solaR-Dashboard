library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(zoo)
library(lattice)
library(leaflet)
library(solaR)
library(tidyverse)
library(reshape2)
library(TSstudio)
library(plotly)
library(tseries)
library(rjson)
library(ggplot2)
library(ggthemes)
library(shinycssloaders)
library(shinybusy)
library(curl)
library(utils)
library(fresh)


month.days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
names(month.days) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)



## Select Meteo Data and hit submit
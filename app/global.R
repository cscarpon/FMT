# Load libraries
library(shiny)
library(rgl)
library(terra)
library(lidR)
library(sf)
library(ggplot2)
library(scales)
library(leaflet)
library(concaveman)
library(reticulate)

# library(shinymaterial)
# library(future)
# library(viridisLite)

source("r/pc_obj.R")
options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 1073741824)

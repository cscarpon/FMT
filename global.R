# Load libraries
library(shiny) # builds the apps 
library(dplyr) # data manipulation
library(rgl) # used for colour gradients
library(terra) # used for raster processing
library(lidR) # used for lidar processing
library(sf) # used for vector manipulation
library(ggplot2) # used for plotting
library(dplyr) # used for data manipulation
library(scales) # used for percent
library(leaflet) # used for interactive map
library(leaflet.extras) # used for dynamic legends
library(htmlwidgets)
library(reticulate) # python in r
library(zip) # zip files
library(leafem)
library(stringr)
options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 1073741824)
lidR::set_lidr_threads(6)

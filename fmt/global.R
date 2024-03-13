# Load libraries
library(shiny) # builds the apps 
library(rgl) # used for colour gradients
library(terra) # used for raster processing
library(lidR) # used for lidar processing
library(sf) # used for vector maniputlation
library(ggplot2) # used for plotting
library(dplyr) # used for data manipulation
library(scales) # used for percent
library(leaflet) # used for interactive map
library(reticulate) # python in r
library(gridExtra) # creates the grid index for density

options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 1073741824)

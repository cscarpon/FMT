# Load libraries
library(shiny)
library(rgl)
library(terra)
library(lidR)
library(sf)
library(shinymaterial)
library(ggplot2)
library(scales)


source("r/pc_obj.R")
options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 1073741824)

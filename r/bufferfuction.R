# Load libraries
library(sf)
library(dplyr)
library(tidyverse)

sample_points <- st_read("D:/Old_PC/Thesis/TTP/Data/Validation Data/TTP_FieldSampleLocations_Fall2021.shp")
tree_data <- read_csv("D:/Old_PC/Thesis/TTP/Data/Validation Data/TTP_Tree_Data_Fall2021.csv")
names(tree_data)[2] <- "treeID"
names(tree_data)[9] <- "distance"
names(tree_data)[10] <- "angle"

names(tree_data)


# Merge tree data with central points
tree_datam <- sample_points %>%
  left_join(tree_data, by = "treeID")

count(tree_datam)

tree_datam$x <- st_coordinates(tree_datam)[,1]
tree_datam$y <- st_coordinates(tree_datam)[,2]


# Function to convert angle and distance to new points
create_points <- function(x, y, angle, distance) {
  # Convert angle to radians
  angle_rad <- angle * pi / 180
  
  # Calculate new coordinates
  new_x <- x + distance * cos(angle_rad)
  new_y <- y + distance * sin(angle_rad)
  
  return(c(new_x, new_y))
}

# Apply the function to each tree
new_points <- mapply(create_points, tree_datam$x, tree_datam$y, tree_datam$angle, tree_datam$distance)

# Convert result into a data frame
new_points_df <- data.frame(
  id = tree_data$central_id,
  x = new_points[1,],
  y = new_points[2,]
)

# Create an sf object for the new points
tree_sf <- st_as_sf(new_points_df, coords = c("x", "y"), crs = 4326)

# Save as shapefile
st_write(tree_sf, "tree_points.shp")

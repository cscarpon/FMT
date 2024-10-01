library(lidR)
library(terra)
library(sf)

sunny_bound <- st_read("G:/EMC/Projects/Sunnybrooke/Data/Boundary/Sunnybrook.shp")
sunny_bound <- st_transform(sunny_bound, crs = 26917)

# Lidar 


# laz15 <- readLAS("G:/Thesis/Sunnybrook/DataDir/pc_14_aligned.laz")
# laz19 <- readLAS("G:/Thesis/Sunnybrook/DataDir/pc_19_aligned.laz")


# none aligned source - 2015

start.time <- Sys.time()
pc_14 <- spatial_container$new("G:/Thesis/Sunnybrook/DataDir/pc_14.laz")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



# Source - 2015
start.time <- Sys.time()
pc_14A <- spatial_container$new("G:/Thesis/Sunnybrook/DataDir/pc_14_aligned.laz")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
pc_14$set_crs(32617)

# path_19 <- "data/TTP_2019_decimate.laz"

# Target - 2019

start.time <- Sys.time()
pc_19 <- spatial_container$new("G:/Thesis/Sunnybrook/DataDir/pc_19.laz")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

pc_19$set_crs(32617)

#Plotting the mask

plot(pc_14$mask)
plot(pc_19$mask)

#Generating the DTM and CHM

# DTM

# Not Aligned

pc_14$to_dtm(1)

writeRaster(pc_14$DTM, "G:/Thesis/Sunnybrook/Rasters/Outputs/DTM_NA_14.tif", overwrite = TRUE)

# Aligned

pc_14A$to_dtm(1)

writeRaster(pc_14A$DTM, "G:/Thesis/Sunnybrook/Rasters/Outputs/DTM_Aligned_14.tif", overwrite = TRUE)

# 2019 

pc_19$to_dtm(1)

writeRaster(pc_19$DTM, "G:/Thesis/Sunnybrook/Rasters/Outputs/DTM_19.tif", overwrite = TRUE)

# CHM

# Not Aligned

pc_14$to_chm(1)

writeRaster(pc_14$CHM, "G:/Thesis/Sunnybrook/Rasters/Outputs/CHM_NA_14.tif", overwrite = TRUE)

# Aligned

pc_14A$to_chm(1)

writeRaster(pc_14A$CHM, "G:/Thesis/Sunnybrook/Rasters/Outputs/CHM_Aligned_14.tif", overwrite = TRUE)


# 2019 

pc_19$to_chm(1)

writeRaster(pc_19$CHM, "G:/Thesis/Sunnybrook/Rasters/Outputs/CHM_19.tif", overwrite = TRUE)

plot(pc_19$CHM)

# DTM Validation 

sample_heights <- vect("G:/Thesis/Sunnybrook/Points/Building_Points.shp")

# wrangling rasters

DTM_14 <- project(pc_14$DTM, "EPSG:26917")
pc_14M <- mask(DTM_14 , sunny_bound)
pc_14M <- resample(pc_14M, pc_19$DTM)
names(pc_14M) <- "DTM_14"

DTM_14A <- project(pc_14A$DTM, "EPSG:26917")
pc_14AM <- mask(DTM_14A , sunny_bound)
pc_14AM <- resample(pc_14AM, pc_19$DTM)
names(pc_14AM) <- "DTM_14"

DTM_19 <- project(pc_19$DTM, "EPSG:26917")
pc_19M <- mask(DTM_19 , sunny_bound)
pc_19M <- resample(pc_19M, pc_19$DTM)
names(pc_19M) <- "DTM_19"

DTM_Stack <- c(pc_14M, pc_14AM, pc_19M)

heights <- extract(DTM_Stack, sample_heights)

write.csv(heights, "G:/Thesis/Sunnybrook/Points/Building_Points_Height.csv")


# load 2015 data

sunny_tiles <- st_read("G:/Thesis/Sunnybrook/Boundaries/2015_tiles.shp")

tile_label <- sunny_tiles$Label

laz_dir15 <- "D:/Thesis_Data/2015 LIDAR/2014-15/LAS_v1.2_ASPRS"

las_file <- list.files(laz_dir15, pattern = ".las$", full.names = TRUE)

for (name in tile_label) {
  las_str <- paste0("D:/Thesis_Data/2015 LIDAR/2014-15/LAS_v1.2_ASPRS/", name, ".las")
  las <- readLAS(las_str)
  writeLAS(las, paste0("G:/Thesis/Sunnybrook/Laz/Raw_15/SB_", name, ".laz"))
}

sb_15_files <- list.files("G:/Thesis/Sunnybrook/Laz/Raw_15", pattern = ".laz$", full.names = TRUE)
ctg <- readLAScatalog(sb_15_files)

laz_15 <- readLAS(ctg)

st_crs(laz_15) <- 26917


# sunny_bound <- st_read("G:/EMC/Projects/Sunnybrooke/Data/Boundary/Sunnybrook.shp")
# sunny_bound <- st_transform(sunny_bound, crs = 26917)
# 
# sunny_laz15 <- lidR::clip_roi(laz_15, sunny_bound)
# 
# plot(sunny_laz15, color = "Classification")
# 
# writeLAS(sunny_laz15, "G:/Thesis/Sunnybrook/Laz/Sunnybrook_15.laz")



?project

# load 2019 lidar data


sun_dir19 <- "G:/Thesis/Sunnybrook/Laz/2019"

sunny_laz_files <- list.files(sun_dir19, pattern = ".laz$", full.names = TRUE)

ctg_19 <- readLAScatalog(sunny_laz_files)

laz19 <- readLAS(ctg_19)

plot(laz19, color = "Classification")

writeLAS(laz19, "G:/Thesis/Sunnybrook/Laz/Sunnybrook_19.laz")



sunny_laz_dir15 <- "D:/Thesis_Data/2019 LIDAR/LAZ/Bridle Path-Sunnybrook-York Mills"
sunny_laz_files15 <- list.files(sunny_laz_dir, pattern = ".laz$", full.names = TRUE)

output_dir <- "D:/Data/Thesis/Sunnybrook/Laz"

# Loop through each LAS file and check intersection with the polygon
for (las_file in sunny_laz_files) {
  # Load the LAS file
  las <- readLAS(las_file)
  
  # Get the extent of the LAS file as an sf polygon
  las_extent <- st_as_sfc(st_bbox(las))
  st_crs(las_extent) <- st_crs(sunny_bound)  # Ensure CRS matches
  
  # Check if the LAS extent intersects with the polygon boundary
  if (st_intersects(las_extent, sunny_bound, sparse = FALSE)) {
    # Clip the LAS file to the boundary
    las_clipped <- clip_roi(las, sunny_bound)  # Check if the LAS extent intersects with the polygon boundary
    
    # Define the output filename
    output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(las_file)), "_clipped.laz"))
    
    # Save the clipped LAS file
    writeLAS(las_clipped, output_file)
  }
}



ttp15 <- readLAS("C:/Users/cscar/FMT/data/TTP_2015.laz")
ttp19 <- readLAS("C:/Users/cscar/FMT/data/TTP_2019.laz")

plot(ttp19)


library(furrr)


decimate <- lidR::decimate_points(ttp19, random(1))

plot(decimate)
pc_decimated <- lidR::classify_ground(decimate, csf())

pc_ground <- lidR::filter_poi(decimate, Classification == 2)

coords_sf <- sf::st_as_sf(decimate@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(decimate))
grid <- sf::st_make_grid(coords_sf, cellsize = 1, what = "polygons")

# set the radius for the plots
radius <- 2  # radius in meters

# Function to create square polygons
create_square_polygon <- function(point, radius, crs) {
  # Extract coordinates
  coords <- st_coordinates(point)
  x <- coords[1]
  y <- coords[2]
  
  # Define the four corners of the square
  square_coords <- matrix(c(
    x - radius, y + radius,  # NW
    x + radius, y + radius,  # NE
    x + radius, y - radius,  # SE
    x - radius, y - radius,  # SW
    x - radius, y + radius   # Close back to NW
  ), ncol = 2, byrow = TRUE)
  
  # Create a polygon from the coordinates
  square_polygon <- st_polygon(list(square_coords))
  
  # Return the polygon as an sf object
  return(st_sfc(square_polygon, crs = st_crs(crs)))
}

# Use parallel processing to create square polygons for each centroid
plan(multisession)  # Set up parallel backend with furrr

start.time <- Sys.time()

centroid_polygons <- future_map(coords_sf$geometry, create_square_polygon, radius = radius)

centroid_aggregate <- aggregate(centroid_polygons)
centroids_list <- st_sfc(centroid_polygons)
test <- st_combine(do.call("c", centroid_polygons))
str(test)
str(centroid_polygons)

test_union <- st_union(test)
st_write(test_union, "C:/Users/cscar/FMT/data/test_union.shp")

?st_union
polygons_sf <- st_as_sf(centroid_polygons)
centroid_union <- st_union(st_sfc(centroid_polygons))

?do.call


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Combine all the square polygons into an sf object
polygons_sf <- st_as_sf(data.frame(id = centroids$ID), geometry = st_sfc(centroid_polygons))

# Check result
print(polygons_sf)

ttp_buffer <- st_buffer(coords_sf, 3)
ttp_union <- st_union(ttp_buffer)
ttp_buffer <- st_buffer(coords_sf, -3)
plot(ttp_union)


mask <- st_union(grid)
plot(mask)


bbox <- sf::st_bbox(pc_14$LPC)
grid <- sf::st_make_grid(sf::st_as_sfc(bbox), cellsize = 100, what = "polygons")
grid_sf <- sf::st_as_sf(grid)


plot(bbox)

lidR::read


mask_pc <- function(pc) {
  decimate <- lidR::decimate_points(pc, random(1))
  
  # Check if there is a ground classification
  if (!"2" %in% unique(pc$Classification)) {
    
    # Classify ground
    pc_decimated <- lidR::classify_ground(pc, csf())
    pc_ground <- lidR::filter_poi(pc_decimated, Classification == 2)
  } else {
    pc_ground <- lidR::filter_poi(pc, Classification == 2)
  }
  coords_sf <- sf::st_as_sf(pc_ground@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(pc))
  grid <- sf::st_make_grid(coords_sf, cellsize = 1, what = "polygons")
  mask <- st_union(grid)
  
  
  # Step 1: Create a 1m grid over the extent of the points
  bbox <- sf::st_bbox(coords_sf)
  grid <- sf::st_make_grid(sf::st_as_sfc(bbox), cellsize = 1, what = "polygons")
  
  # Step 2: Snap points to the nearest grid cell
  grid_sf <- sf::st_as_sf(grid)
  coords_sf$grid_id <- sf::st_nearest_feature(coords_sf, grid_sf)
  
  # Step 3: Remove duplicate points within the same grid cell
  unique_coords_sf <- coords_sf %>%
    group_by(grid_id) %>%
    slice(1) %>%
    ungroup()
  
  # Step 4: Buffer the points by 3 meters
  buffered_points <- sf::st_buffer(unique_coords_sf, dist = 3)
  
  # Step 5: Union the buffered geometries into a single geometry
  unioned_buffer <- sf::st_union(buffered_points)
  
  # Step 6: Apply a negative buffer (shrink by 3 meters)
  final_polygon <- sf::st_buffer(unioned_buffer, dist = -3, endCapStyle = "SQUARE")
  
  # Step 7: Ensure the polygon is valid
  final_polygon <- sf::st_make_valid(final_polygon)
  
  # Step 8: Remove holes and simplify the final polygon
  # Initialize an empty list to store the results
  final_result <- list()
  
  # Iterate over each geometry in final_polygon
  for (i in seq_along(final_polygon)) {
    # Remove holes
    no_holes <- nngeo::st_remove_holes(final_polygon[i])
    # Append to the final result list
    final_result[[i]] <- no_holes
  }
  
  
  # Combine all the results into a single sf object
  final_sf <- do.call(sf::st_sfc, final_result)
  final_sf <- rmapshaper::ms_simplify(final_sf, keep = 0.03, weighting = 2.0, keep_shapes = TRUE)
  final_sf <- sf::st_as_sf(final_sf)
  
  sf::st_crs(final_sf) <- lidR::projection(pc)
  
  return(final_sf)
}
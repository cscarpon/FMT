source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

set_lidr_threads(12)

# creating the spatial container objects

las <- readLAS("F:/Thesis/Sunnybrook/DataDir/Sunnybrook_15_clean.laz")

las_sf <- st_as_sf(las@data, coords = c("X", "Y"))

# Step 3: Get the bounding box
las_extent <- st_as_sfc(st_bbox(las_sf))



ground <- filter_ground(las) 
decimate <- lidR::decimate_points(ground, random(1))
coords_sf <- sf::st_as_sf(ground@data[,c("X", "Y")], coords = c("X", "Y"), crs = 26917)
buffered_points <- sf::st_buffer(coords_sf, dist = 3, endCapStyle = "FLAT")


vect_coords <- terra::vect(coords_sf)


# Step 1: Generate 100 random points within a given extent
set.seed(123)  # For reproducibility
x_coords <- runif(300, min = 0, max = 2)  # Random X coordinates between 0 and 100
y_coords <- runif(300, min = 0, max = 2)  # Random Y coordinates between 0 and 100

coords <- cbind(x_coords, y_coords)

df <- data.frame(coords)
names(df) <- c("X", "Y")

# Create an sf object from the random coordinates
points_sf <- st_as_sf(df, coords = c("X", "Y"))

# Step 2: Create round buffers around the points (with radius = 3)
round_buffers <- st_buffer(points_sf, dist = 1)

plot(round_buffers)

unioned_buffer <- st_union(round_buffers)

plot(unioned_buffer)

round_buffers <- st_buffer(unioned_buffer, dist = 0.1, endCapStyle = "SQUARE")

plot(round_buffers)

# Buffer 

start.time <- Sys.time()
buffered_points <- terra::buffer(vect_coords, 3, capstyle = "FLAT")
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

plot(buffered_points)

# Union

start.time <- Sys.time()
buffered_points <- sf::st_union(st_as_sfc(vect_coords, 3, capstyle = "FLAT")
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)



start.time <- Sys.time()
buffered_union <- terra::union(buffered_points)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

plot(buffered_union)

# Step 5: Union the buffered geometries into a single geometry
buffered_mask <- terra::buffer(vect_coords, -3, capstyle = "FLAT")

# # Step 6: Apply a negative buffer (shrink by 3 meters)
# final_polygon <- sf::st_buffer(unioned_buffer, dist = -3, endCapStyle = "SQUARE")

final_polygon <- st_as_sf(buffered_mask, crs = lidR::projection(pc))

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
final_sf <- sf::st_transform(final_sf, crs = lidR::projection(pc))



# Step 4: Plot the bounding box as a polygon
plot(las_extent, col = NA, border = "red", lwd = 2, main = "Bounding Box of LAS File")

# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new("F:/Thesis/Sunnybrook/DataDir/Sunnybrook_19_clean.laz")

plot(pc_19$mask)

#Generating the DTM and CHM

pc_14$to_dtm(1)

pc_19$to_dtm(1)

pc_14$to_chm(1)

pc_19$to_chm(1)

plot(pc_19$CHM)

# buildings <- sf::st_read("F:/Thesis/Sunnybrook/Points/SB_Clip.shp")
# buildings <- sf::st_transform(buildings, crs = 26917)

buildings_FP <- vect("F:/Thesis/Sunnybrook/Points/Buildings_Rectified.shp", crs = "EPSG:26917")

buildings_FP <- buffer(buildings_FP, 2, capstyle="flat")

buildings_fp_union <- union(buildings_FP)

# Create a new raster with the same extent as SB_Change and 1m resolution
template_raster <- rast(extent = ext(pc_19$CHM_raw), resolution = 1, crs = "EPSG:26917")


# Step 3: Rasterize the building polygons onto the new 1m raster grid
buildings_raster <- rasterize(buildings_FP, template_raster, background = NA)

plot(buildings_raster)

# Step 4: Set the overlapping raster cells in SB_Change to 0 where buildings exist
pc_14$CHM_raw[!is.na(buildings_raster)] <- 0
pc_19$CHM_raw[!is.na(buildings_raster)] <- 0

plot(pc_14$CHM_raw)

#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14$CHM_raw, pc_19$CHM_raw, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")

union <- sf::st_union(pc_14$mask, pc_19$mask)
union <- terra::vect(union)

# Crop the source to match the target raster's extent.

# source <- transform_raster_crs(source, target, crs)
source <- terra::crop(pc_14$CHM_raw, terra::ext(union))
target <- terra::crop(pc_19$CHM_raw, terra::ext(union))

source <- terra::resample(source, target, method = "bilinear")

# Apply masks to each of the raster layers which will be used for the difference.
source <- terra::mask(source, union)
target <- terra::mask(target, union)

aligned_chm <- list(source = source, target = target, mask = union)

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

plot(source_chm)

# Function to generate CHM and classify the differences

chm_diff_test <- CHM_diff_classify(source_chm, target_chm)

plot_stats(chm_diff_test)

# Display the outputs/. 

displayMap(pc_14$DTM,pc_14$CHM, chm_diff_test, chm_mask)


# Function to for preprocessing of a raster. It aligns the source to the target, resamples and then masks the output so they have the same extent as the target
process_raster <- function(source, target, source_mask, target_mask,  method = "bilinear") {
  
  aligned <- FALSE
  aligned <- terra::compareGeom(source, target, stopOnError = FALSE)
  
  if (!aligned) {
    # Assuming you have predefined masks for source and target
    
    # source_mask <- transform_polygon_crs(source_mask, target_mask, crs)
    
    union <- sf::st_union(source_mask, target_mask)
    union <- terra::vect(union)
    
    # Crop the source to match the target raster's extent.
    
    # source <- transform_raster_crs(source, target, crs)
    source <- terra::crop(source, terra::ext(union))
    target <- terra::crop(target, terra::ext(union))
    
    source <- terra::resample(source, target, method = method)
  }
  
  # Apply masks to each of the raster layers which will be used for the difference.
  source <- terra::mask(source, union)
  target <- terra::mask(target, union)
  
  return(list(source = source, target = target, mask = union))
}

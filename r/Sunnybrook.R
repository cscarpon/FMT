source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

set_lidr_threads(10)


sunny_bound <- st_read("G:/EMC/Projects/Sunnybrooke/Data/Boundary/Sunnybrook.shp")
sunny_bound <- st_transform(sunny_bound, crs = 26917)


buildings <- sf::st_read("F:/Thesis/Sunnybrook/Points/SB_Clip.shp")
buildings <- sf::st_transform(buildings, crs = 26917)


# Lidar 


pc_14A <- count_time(spatial_container$new("C:/Users/cscar/FMT/data/Sunnybrook_15_aligned.laz"))
plot(pc_14A$index)

pc_14A$set_crs(32617)

pc_mask <- count_time(mask_pc(pc_14A$LPC))
pc_14A$mask <- pc_mask

pc_19 <- count_time(spatial_container$new("C:/Users/cscar/FMT/data/Sunnybrook_19.laz"))
plot(pc_19$index)

pc_mask_19 <- count_time(mask_pc(pc_19$LPC))
pc_19$mask <- pc_mask_19

buildings <- st_read("C:/Users/cscar/FMT/data/SB_Buildings.shp")

pc_19_clean <- count_time(noise_filter_buildings(pc_19$LPC, pc_19$mask, buildings,  k_sor1 = 5, m_sor1 = 2, k_sor2 = 15, m_sor2 = 4))

writeLAS(pc_19_clean , "C:/Users/cscar/FMT/data/Sunnybrook_19_clean.laz")

laz15_denoise <- count_time(noise_filter_buildings(pc_14A$LPC, pc_14A$mask, buildings,  k_sor1 = 5, m_sor1 = 2, k_sor2 = 15, m_sor2 = 4))

writeLAS(laz15_denoise, "C:/Users/cscar/FMT/data/Sunnybrook_15_clean.laz")

plot(laz15_denoise)

renv::use_python("C:/Users/cscar/anaconda3/envs/EMT_conda/python.exe")

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

reticulate::source_python(icp_module)

# Create instance of the ICP class
icp_aligner <- pdal_icp("C:/Users/cscar/FMT/data/Sunnybrook_15_clean.laz", "C:/Users/cscar/FMT/data/Sunnybrook_19_clean.laz")

# Call the align method
aligned_file_path <- icp_aligner$align()

# Process the source point cloud
pc_14A <- spatial_container$new(as.character(aligned_file_path))
pc_14A$set_crs(32617)

lidR::writeLAS(pc_14A$LPC, "C:/Users/cscar/FMT/data/Sunnybrook_15_clean_aligned.laz")




# Making a negative of the mask by cutting out the building footprints


laz19 <- readLAS("F:/Thesis/Sunnybrook/DataDir/Sunnybrook_19.laz")

laz19 <- st_transform(laz19, crs = 26917)

run_noise_classification_fixed <- function(las, k_range = 1:30, fixed_m = 4, res_range = 1:5, fixed_n = 10) {
  
  # Step 1: Read and transform the LAS file
  
  # Initialize lists to store cleaned LAS files for both `sor` and `ivf` iterations
  sor_clean_list <- list()
  ivf_clean_list <- list()
  
  # Step 2: Loop over k for the `sor` function (m stays fixed)
  for (k in k_range) {
    # Apply the `sor` noise classification with fixed m
    las_sor <- classify_noise(las, sor(k = k, m = fixed_m))
    # Clean the LAS by filtering out the noise (Classification != 18)
    las_sor_clean <- filter_poi(las_sor, Classification != 18)
    # Save the result in the list
    sor_clean_list[[paste0("sor_k", k, "_m", fixed_m)]] <- las_sor_clean
  }
  
  # Step 3: Loop over res for the `ivf` function (n stays fixed)
  for (res in res_range) {
    # Apply the `ivf` noise classification with fixed n
    las_ivf <- classify_noise(las, ivf(res = res, n = fixed_n))
    # Clean the LAS by filtering out the noise (Classification != 18)
    las_ivf_clean <- filter_poi(las_ivf, Classification != 18)
    # Save the result in the list
    ivf_clean_list[[paste0("ivf_res", res, "_n", fixed_n)]] <- las_ivf_clean
  }
  
  # Return a list containing all cleaned LAS point clouds from `sor` and `ivf`
  return(list(sor_clean = sor_clean_list, ivf_clean = ivf_clean_list))
}

# Usage example
las_file <- "F:/Thesis/Sunnybrook/DataDir/Sunnybrook_19.laz"
result <- run_noise_classification_fixed(laz19, k_range = 1:30, fixed_m = 4, res_range = 1:5, fixed_n = 10)

result2 <- run_noise_classification_fixed(laz19, k_range = 1:30, fixed_m = 10, res_range = 1:5, fixed_n = 15)

# Accessing a specific cleaned LAS point cloud from the results
sor_result_k5_m4 <- result$sor_clean[["sor_k5_m4"]]
ivf_result_res2_n15 <- result$ivf_clean[["ivf_res2_n15"]]

plot(result$sor_clean[[1]])
plot(result$sor_clean[[5]])
plot(result$sor_clean[[10]])
plot(result$sor_clean[[15]])
plot(result$sor_clean[[20]])
plot(result$sor_clean[[25]])
plot(result$sor_clean[[30]])

plot(result$ivf_clean[[1]])
plot(result$ivf_clean[[5]])


plot(result2$sor_clean[[1]])
plot(result2$sor_clean[[5]])
plot(result2$sor_clean[[10]])
plot(result2$sor_clean[[15]])
plot(result2$sor_clean[[20]])
plot(result2$sor_clean[[25]])
plot(result2$sor_clean[[30]])

plot(result2$ivf_clean[[1]])
plot(result2$ivf_clean[[5]])


las19_clean <- filter_poi(laz19, Classification != 18)  # Noise points are typically classified as 18

# Step 1: Classify ground points using the progressive morphological filter (PMF)
las_clean_19 <- classify_ground(las19_clean, algorithm = pmf(ws = 2, th = 0.2))

# Step 2: Filter low points (for example, points below 120m)
las_clean_19 <- filter_poi(las_clean_19, Z > 120)

las_19 <-  clip_roi(las_clean_19, buildings)

# Step 5: Segment transmission cables (linear features)
las_cable19 <- segment_shapes(las_19, shp_line(k = 5, th1 = 30), "Cables")
las_no_cable19 <- filter_poi(las_cable19, Cables == "FALSE")
plot(las_cable19, color = "Cables")
plot(las_no_cable19)


# Step 6: Segment vertical poles (using vertical line segmentation)
las_poles19 <- segment_shapes(las_no_cable19, shp_vline(th1 = 5, k = 4), "Poles")
las_no_poles19 <- filter_poi(las_poles19, Poles == "FALSE")
plot(las_poles19, color = "Poles")
plot(las_no_poles19)


laz19_noiseless <- classify_noise(las_no_poles19, sor(k = 4, m = 4))

las19_noiseless_clean <- filter_poi(laz19_noiseless, Classification != 18)  # Noise points are typically classified as 18
plot(las19_noiseless_clean)

writeLAS(las19_noiseless_clean, "F:/Thesis/Sunnybrook/DataDir/Sunnybrook_19_clean.laz")




# Step 4: Segment walls (vertical surfaces)
las_walls19 <- segment_shapes(las_no_cable19, shp_plane(k = 40, th1 = 10, th2 = 85), "Walls")
plot(las_walls19, color = "Walls")

las_no_walls <- filter_poi(las_walls, Walls == "FALSE")
plot(las_no_walls)

# Segment horizontal roofs (using low slope threshold)
las_roof <- segment_shapes(las_no_walls, shp_plane(k = 5, th1 = 10, th2 = 5), "Roofs")
las_no_roof <- filter_poi(las_roof, Roofs == "FALSE")
plot(las_no_roof)

plot(las_19)

# Filtering to remove buildings


# Step 3: Filter out ground points (typically classified as 2)
las_no_ground <- filter_poi(las_clean_15, Classification != 2)

# Step 4: Segment walls (vertical surfaces)
las_walls <- segment_shapes(las_no_ground, shp_plane(k = 40, th1 = 10, th2 = 85), "Walls")
las_no_walls <- filter_poi(las_walls, Walls == "FALSE")
plot(las_no_walls)

# Segment horizontal roofs (using low slope threshold)
las_roof <- segment_shapes(las_no_walls, shp_plane(k = 5, th1 = 10, th2 = 5), "Roofs")
las_no_roof <- filter_poi(las_roof, Roofs == "FALSE")
plot(las_no_roof)

# Step 5: Segment transmission cables (linear features)
las_cable <- segment_shapes(las_no_roof, shp_line(k = 5, th1 = 30), "Cables")
las_no_cable <- filter_poi(las_cable, Cables == "FALSE")
plot(las_cable, color = "Cables")
plot(las_no_cable)


# Step 6: Segment vertical poles (using vertical line segmentation)
las_poles <- segment_shapes(las_no_cable, shp_vline(th1 = 5, k = 4), "Poles")
las_no_poles <- filter_poi(las_poles, Poles == "FALSE")
plot(las_poles, color = "Poles")
plot(las_no_poles)


# Segment additional flat structures like terraces or other features
las_flat_structures <- segment_shapes(las_no_poles, shp_hline(k = 5, th1 = 5), "FlatStructures")
las_no_flat_structures <- filter_poi(las_flat_structures, FlatStructures == "FALSE")
plot(las_no_flat_structures)


# Step 4: Segment walls (vertical surfaces)
las_extra <- segment_shapes(las_no_flat_structures, shp_line(k = 5, th1 = 10), "Extra")
las_no_extra <- filter_poi(las_extra, Extra == "FALSE")
plot(las_no_extra)

# Classify noise (e.g., isolated points) and filter them out
las_clean <- classify_noise(las_no_extra, sor(k = 5, m = 2))
las_no_noise <- filter_poi(las_clean, Classification != 18)  # Noise is typically classified as 18


# Create extent from the last object
box_test <- st_as_sfc(st_bbox(las_no_noise), crs = 32617)

# Create a grid with a specified cell size

grid <- st_make_grid(box_test, cellsize = c(300, 300), what = "polygons")

library(lmom)


cloud_metrics(las_no_noise , func = .stdmetrics)
cloud_metrics(las_no_noise , func = ~as.list(lmom::samlmu(Z)))


las_test <- las_no_noise

# Initialize an empty list to store processed point clouds
point_clouds <- list()

# Loop through each grid cell, calculate metrics, run HDBSCAN, and store the results
for (i in 1:length(grid)) {
  start.time <- Sys.time()
  print(paste("Processing grid cell", i, "of", length(grid)))
  
  # Add tryCatch block to handle empty or error cases
  tryCatch({
    # Clip the point cloud to the current grid cell
    clip_las <- clip_roi(las_no_noise, grid[i])
    
    # If there are no points, skip to the next iteration
    if (nrow(clip_las@data) > 0) {
      
      # Step 1: Calculate cloud metrics for Z and Intensity
      cloud_metrics_result <- cloud_metrics(clip_las, func = ~c(
        stdmetrics_z(Z),          # Standard metrics for Z (elevation)
        stdmetrics_i(Intensity),  # Standard metrics for Intensity
        as.list(lmom::samlmu(Z)),   # L-moments for Z
        as.list(lmom::samlmu(Intensity)) # L-moments for Intensity
      ))
      
      # Step 2: Extract metrics to use for clustering
      z_mean <- cloud_metrics_result$zmean
      z_sd <- cloud_metrics_result$zsd
      z_q90 <- cloud_metrics_result$zq90
      intensity_mean <- cloud_metrics_result$imean
      intensity_sd <- cloud_metrics_result$isd
      l1_z <- cloud_metrics_result$l_1  # L-moment 1 for Z
      l2_z <- cloud_metrics_result$l_2  # L-moment 2 for Z
      
      # Step 3: Add metrics to the original point cloud for clustering
      clip_las@data$Z_Mean <- z_mean
      clip_las@data$Z_SD <- z_sd
      clip_las@data$Z_Q90 <- z_q90
      clip_las@data$Intensity_Mean <- intensity_mean
      clip_las@data$Intensity_SD <- intensity_sd
      clip_las@data$L1_Z <- l1_z
      clip_las@data$L2_Z <- l2_z
      
      # Step 4: Prepare the data for HDBSCAN by combining XYZ and computed metrics
      coords <- as.matrix(clip_las@data[, c("X", "Y", "Z", "Z_Mean", "Z_SD", "Z_Q90", 
                                            "Intensity_Mean", "Intensity_SD", "L1_Z", "L2_Z")])
      
      # Step 5: Run HDBSCAN with the extended feature set
      db <- hdbscan(coords, minPts = 30)
      
      # Step 6: Add cluster labels back to the point cloud for this grid cell
      clip_las@data$Cluster <- db$cluster
      
      # Step 7: Append the processed point cloud to the list
      point_clouds[[i]] <- clip_las
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      print(time.taken)
      
    } else {
      print(paste("No points found in grid cell", i, "- skipping."))
    }
  }, error = function(e) {
    # Handle errors (e.g., if grid cell is empty or any other issue)
    print(paste("Error in grid cell", i, "- skipping."))
  })
}

# Combine all processed point clouds back into one
if (length(point_clouds) > 0) {
  las_combined <- do.call(rbind, point_clouds)
  
  # Step 8: Visualize the final combined point cloud with clustering results
  plot(las_combined, color = "Cluster")
} else {
  print("No valid point clouds were processed.")
}

point_list <- unlist(point_clouds)

# Combine all processed point clouds back into one
las_combined <- do.call(rbind, point_list)

# Step 8: Visualize the final combined point cloud with clustering results
plot(las_combined, color = "Cluster")

writeLAS(las_combined, "F:/Thesis/Sunnybrook/DataDir/cluster15.laz")


unique(las_combined@data$Cluster)


library(dbscan)
coords <- as.matrix(las_no_noise@data[, c("X", "Y", "Z")])

kmeans_result <- kmeans(coords, centers = 10)

# Add cluster labels back to the point cloud
las_no_noise@data$Cluster <- kmeans_result$cluster

# Visualize the clustering result
plot(las_no_noise, color = "Cluster")

# Apply DBSCAN clustering
# You may need to tune eps and minPts based on your point cloud
db <- dbscan(coords, eps = 2, minPts = 30)

db <- hdbscan(coords, minPts = 30)

# Add cluster labels back to the point cloud
las_no_noise@data$Cluster <- db$cluster

# Visualize the clusters
plot(las_no_noise, color = "Cluster")

las_clean <- filter_poi(las, Buildings == 0 & Poles == 0 & Walls == 0 & Bridges == 0 & TransmissionCables == 0)

# Step 4: Segment bridges
las <- segment_shapes(las, shp_plane(k = 10, th1 = 0.1, th2 = 10), "Bridges")

# Visualize the final cleaned point cloud
plot(las_clean)

# Extract and visualize buildings (planar surfaces)
buildings <- filter_poi(las, Classification == 6)  # 6 is the common classification code for buildings
plot(buildings, color = "Classification")

plot(las_clean_15)

# Segment objects using shapes
las <- lidR::segment_shapes(las_clean, algorithm = "shape", radius = 2, connectivity = 8)

# Extract and visualize buildings, street poles, and transmission lines
buildings <- filter_poi(las, Classification == 6)
poles <- filter_poi(las, Classification == 15)
transmission_lines <- filter_poi(las, Classification == 18)

# Visualize buildings
plot(buildings, color = "Classification")

# Visualize poles
plot(poles, color = "Classification")

# Visualize transmission lines
plot(transmission_lines, color = "Classification")


laz19 <- readLAS("F:/Thesis/Sunnybrook/DataDir/pc_19.laz")
laz_19 <- st_transform(laz19, crs = 26917)

laz_15_clip <- lidR::clip_roi(laz_15, buildings)
laz_19_clip <- lidR::clip_roi(laz_19, buildings)

writeLAS(laz_15_clip, "F:/Thesis/Sunnybrook/DataDir/pc_14_aligned_clip.laz"))

plot(laz_15_clip)

writeLAS(laz_15_clip, )

# none aligned source - 2015

start.time <- Sys.time()
pc_14 <- spatial_container$new("G:/Thesis/Sunnybrook/DataDir/pc_14.laz")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(pc_14$mask)

# Source - 2015
start.time <- Sys.time()
pc_14A <- spatial_container$new("G:/Thesis/Sunnybrook/DataDir/pc_14_aligned.laz")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
pc_14A$set_crs(32617)


las <- decimate_points(pc_14A$LPC, random(1))

writeLAS(las, "C:/Users/cscar/FMT/data/SB_decimate_15.laz")

# path_19 <- "data/TTP_2019_decimate.laz"

# Target - 2019

start.time <- Sys.time()
pc_19 <- spatial_container$new("G:/Thesis/Sunnybrook/DataDir/pc_19.laz")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

pc_19$set_crs(32617)

#Plotting the mask

plot(pc_14A$mask)
plot(pc_19$mask)

#Generating the DTM and CHM

# DTM

# Not Aligned

pc_14$to_dtm(1)

writeRaster(pc_14$DTM, "G:/Thesis/Sunnybrook/Rasters/Outputs/DTM_NA_14.tif", overwrite = TRUE)

# Aligned

pc_14A$to_dtm(1)

writeRaster(pc_14A$DTM, "G:/Thesis/FMT_Outputs/DTM_2015.tif", overwrite = TRUE)

# 2019 

pc_19$to_dtm(1)

writeRaster(pc_19$DTM, "G:/Thesis/FMT_Outputs/DTM_2019.tif", overwrite = TRUE)

# CHM

# Not Aligned

pc_14$to_chm(1)

# Aligned

pc_14A$to_chm(1)

writeRaster(pc_14A$CHM,"G:/Thesis/FMT_Outputs/CHM_2015.tif", overwrite = TRUE)


# 2019 

pc_19$to_chm(1)

writeRaster(pc_19$CHM,"G:/Thesis/FMT_Outputs/CHM_2019.tif", overwrite = TRUE)

plot(pc_19$CHM)


#This function aligns the two rasters and returns aligned raster objects.

################################################

################################################

#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14A$CHM_raw, pc_19$CHM_raw, source_mask = pc_14A$mask, target_mask = pc_19$mask, method = "bilinear")

st_write(pc_14A$mask, "G:/Thesis/FMT_Outputs/mask_2015.shp")
st_write(pc_19$mask,  "G:/Thesis/FMT_Outputs/mask_2019.shp")


source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

# Function to generate CHM and classify the differences

chm_diff_test <- CHM_diff_classify(source_chm, target_chm)

writeRaster(chm_diff_test, "G:/Thesis/FMT_Outputs/Difference15_19.tif")

plot(classified_diff)

raster_values <- terra::values(classified_diff)

# Remove NA values if necessary
raster_values <- raster_values[!is.na(raster_values)]

# Get the count of each class
class_counts <- table(raster_values)

# Calculate the total number of cells
total_cells <- sum(class_counts)

# Calculate the percentage of each class
class_percentages <- (class_counts / total_cells) * 100

plot(class_percentages)

# Calculate the percentages of each class




diff_values <- function(raster) {
  # Get the values of the raster
  
  # Get the frequency of each class
  class_freq <- freq(raster)
  
  # Calculate the total number of cells
  total_cells <- sum(class_freq[, "count"])
  
  # Calculate percentage for each class
  class_freq$percentage <- (class_freq$count / total_cells) * 100
  
  # Set the class values as row names
  rownames(class_freq) <- class_freq$value
  
  # Remove the 'value' column since it's now the row name
  class_freq <- class_freq[, -1]
  
  return(class_percentages)
}
  

# Get the values of the raster
raster_values <- terra::values(chm_diff_test)

# Remove NA values if necessary
raster_values <- raster_values[!is.na(raster_values)]

# Get the count of each class
class_counts <- table(raster_values)

# Calculate the total number of cells
total_cells <- sum(class_counts)

# Calculate the percentage of each class
class_percentages <- (class_counts / total_cells) * 100

str(class_percentages)

# Display the results
print(class_percentages)

# Calculate the percentages of each class

# Get the frequency of each class
class_freq <- freq(chm_diff_test)

# Calculate the total number of cells
total_cells <- sum(class_freq[, "count"])

# Calculate percentage for each class
class_freq$percentage <- (class_freq$count / total_cells) * 100

# Set the class values as row names
rownames(class_freq) <- class_freq$value

# Remove the 'value' column since it's now the row name
class_freq <- class_freq[, -1]

# Display the class frequencies with percentages
print(class_freq)


write.csv(class_freq, "G:/Thesis/FMT_Outputs/CHM_diff_freq.csv")

# Plot the statistics

plot_stats(chm_diff_test)

# DTM Validation 

# sample_heights <- vect("G:/Thesis/Sunnybrook/Points/Building_Points.shp")
road_points <- vect("G:/EMC/Projects/Aecon/Data/Shapefiles/Road_Points.shp")
road_points <- project(road_points, "EPSG:32617")

dtm15_raw <- rast(file.path("./saves/dtm_raw15.tif"))
dtm15_clean <- rast("C:/Users/cscar/FMT/saves/Sunnybrook_15_clean_aligned_dtm.tif")
dtm19_clean <- rast("C:/Users/cscar/FMT/saves/Sunnybrook_19_clean_dtm.tif")

mask_vect <- vect("C:\\Users\\cscar\\FMT\\data\\SB_Mask19.shp")



dtm15_raw_mask <- mask(dtm15_raw, mask_vect)
# dtm_raw_re <- resample(dtm15_raw_mask, dtm19_clean)
names(dtm15_raw_mask) <- "Raw_15"
dtm15_clean_mask <- mask(dtm15_clean, mask_vect)
# dtm15_clean_re <- resample(dtm15_clean_mask, dtm19_clean)
names(dtm15_clean_mask) <- "Clean_15"
dtm19_clean_mask <- mask(dtm19_clean, mask_vect)
names(dtm19_clean_mask) <- "Clean_19"

# raster_stack <- c(dtm_raw_re, dtm15_clean_re, dtm19_clean_mask)

heights_15_raw <- extract(dtm15_raw_mask, road_points)
heights_15_clean <- extract(dtm15_clean_mask, road_points)
heights_19_clean <- extract(dtm19_clean_mask, road_points)

heights <- cbind(heights_15_raw, heights_15_clean, heights_19_clean)
heights_rm <- heights[,c(2,4,6)]

heights_df <- as.data.frame(heights_rm)

write.csv(heights_df, file.path("./saves/heights_no_resample.csv"))

# 
# # wrangling rasters
# 
# DTM_14 <- project(pc_14$DTM, "EPSG:26917")
# pc_14M <- mask(DTM_14 , sunny_bound)
# pc_14M <- resample(pc_14M, pc_19$DTM)
# names(pc_14M) <- "DTM_14"
# 
# DTM_14A <- project(pc_14A$DTM, "EPSG:26917")
# pc_14AM <- mask(DTM_14A , sunny_bound)
# pc_14AM <- resample(pc_14AM, pc_19$DTM)
# names(pc_14AM) <- "DTM_14"
# 
# DTM_19 <- project(pc_19$DTM, "EPSG:26917")
# pc_19M <- mask(DTM_19 , sunny_bound)
# pc_19M <- resample(pc_19M, pc_19$DTM)
# names(pc_19M) <- "DTM_19"
# 
# DTM_Stack <- c(pc_14M, pc_14AM, pc_19M)
# 
# heights <- extract(raster_stack, road_points)
# 
# heights_df <- as.data.frame(heights)
# 
# write.csv(heights_df, file.path("./saves/heights.csv"))
# 
# heights_df <- read_csv("G:/Thesis/Sunnybrook/Points/Building_Points_Height.csv")

# Height_valid <- heights_df[2:4]
# 
# names(Height_valid) <- c("DTM_14", "DTM_14A", "DTM_19")

valid_model <- lm(Clean_19 ~ Clean_15, data = heights_df)
summary(valid_model)


valid_model_Aligned <- lm(Clean_19 ~ Raw_15, data = heights_df)
summary(valid_model_Aligned)


#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14A$CHM_raw, pc_19$CHM_raw, source_mask = pc_14A$mask, target_mask = pc_19$mask, method = "bilinear")


st_write(pc_14A$mask, paste0(getwd(), "/Data/mask_2015.shp"))
st_write(pc_19$mask, paste0(getwd(), "/Data/mask_2019.shp"))

################################################

################################################

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

# Function to generate CHM and classify the differences

chm_diff_test <- CHM_diff_classify(source_chm, target_chm)

plot_stats(chm_diff_test)

# Display the outputs/. 

displayMap(pc_14A$DTM,pc_14A$CHM, chm_diff_test, chm_mask)

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
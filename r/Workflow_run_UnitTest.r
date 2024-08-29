source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

#test

#Methods

# to_dtm()        save_las()
# to_chm()        save_dtm()
# save_mask()     save_chm()
# save_pc()

#Functions

# process_raster(source, target, mask_layer)
# CHM_diff_classify(earlier, later)
# raster_stats(raster)
# mask_pc()


dir <- "./data/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)

pc_14 <- spatial_container$new(mo_dir$metadata$file_path[1])

pc_14$set_crs(32617)

# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[2])
pc_19$set_crs(32617)

pc_21 <- spatial_container$new("G:/Thesis/GoogleDrive/2021-DronePtCloud.las")

rm(pc_21)

las_21 <- lidR::readLAS("G:/Thesis/GoogleDrive/2021-DronePtCloud.las")

plot(las_21)


las_21_decimate <- decimate_points(las_21, random(10))
plot(las_21_decimate)

# Classify ground

# mycsf <- csf(sloop_smooth = TRUE, class_threshold = 1, cloth_resolution = 1, time_step = 1)
# las <- classify_ground(las_21_decimate, mycsf)

mycsf <- csf(sloop_smooth = TRUE, class_threshold = 2, cloth_resolution = 1, time_step = 1)
las <- classify_ground(las_21_decimate, mycsf)


# lidR::plot_crossection(las, p1 = p1, p2 = p2, colour_by = factor(Classification))
# pc_decimated <- lidR::classify_ground(las_21_decimate, csf())
pc_ground <- lidR::filter_poi(las, Classification == 2)

decimate <- lidR::decimate_points(pc_ground, random(1))

coords_sf <- sf::st_as_sf(pc_ground@data[,c("X", "Y")], coords = c("X", "Y"), crs = lidR::projection(decimate))

# Step 1: Create a 1m grid over the extent of the points
bbox <- sf::st_bbox(coords_sf)
grid <- sf::st_make_grid(sf::st_as_sfc(bbox), cellsize = 1, what = "centers")



# Step 2: Snap points to the nearest grid cell
grid_sf <- sf::st_as_sf(grid)
coords_sf$grid_id <- sf::st_nearest_feature(coords_sf, grid_sf)

# Step 3: Remove duplicate points within the same grid cell
unique_coords_sf <- coords_sf %>%
  group_by(grid_id) %>%
  slice(1) %>%
  ungroup()

nearby_pt <- nearby(vect(unique_coords_sf ), distance = 3)

nearby_pt <- nearest(vect(unique_coords_sf ), distance = 3)

plot(pc_ground)

mask_21 <- mask_pc(pc_ground)
     
plot(las_21)
plot(mask_21)
conda_create("EMT_conda", python = "3.9.13", packages = c("pdal", "numpy", "scipy"))

#Activate conda environment
reticulate::use_condaenv("EMT_conda", required = TRUE)

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

reticulate::source_python(icp_module)

# Create instance of the ICP class
icp_aligner <- pdal_icp(pc_14$filepath, pc_19$filepath)

# Call the align method
aligned_file_path <- icp_aligner$align()

# Process the source point cloud
pc_14 <- spatial_container$new(as.character(aligned_file_path))
pc_14$set_crs(32617)

#Plotting the mask

plot(pc_14$mask)

#Generating the DTM and CHM

pc_14$to_dtm(0.5)

pc_19$to_dtm(0.5)

pc_14$to_chm(0.5)

pc_19$to_chm(0.5)

plot(pc_19$CHM)



#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14$CHM_raw, pc_19$CHM_raw, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")


st_write(pc_14$mask, paste0(getwd(), "/Data/mask_2015.shp"))
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

displayMap(pc_14$DTM,pc_14$CHM, chm_diff_test, chm_mask)


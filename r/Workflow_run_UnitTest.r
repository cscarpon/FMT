source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

set_lidr_threads(10)

writeLAS(las19_noiseless_clean, "F:/Thesis/Sunnybrook/DataDir/Sunnybrook_19_clean.laz")

writeLAS(las15_noiseless_clean, "F:/Thesis/Sunnybrook/DataDir/Sunnybrook_15_clean.laz")

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
# dir <- "G:/Thesis/Sunnybrook/DataDir/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)

pc_14 <- spatial_container$new(mo_dir$metadata$file_path[5])
pc_14$set_crs(32617)

# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[7])
pc_19$set_crs(32617)


writeLAS(pc_14$LPC, "G:/Thesis/Sunnybrook/DataDir/pc_14.laz")
writeLAS(pc_19$LPC, "G:/Thesis/Sunnybrook/DataDir/pc_19.laz")

conda_create("EMT_conda", python = "3.9.13", packages = c("pdal", "numpy", "scipy"))

#Activate conda environment
reticulate::use_condaenv("EMT_conda", required = TRUE)
reticulate::import("pdal")

renv::use_python("C:/Users/cscar/anaconda3/envs/EMT_conda/python.exe")

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

reticulate::source_python(icp_module)

# Create instance of the ICP class
icp_aligner <- pdal_icp(pc_14$filepath, pc_19$filepath)

# Call the align method
aligned_file_path <- icp_aligner$align()

# Process the source point cloud
pc_14A <- spatial_container$new(as.character(aligned_file_path))
pc_14$set_crs(32617)

#Plotting the mask

plot(pc_14$mask)

#Generating the DTM and CHM

pc_14$mask <- mask_pc(pc_14$LPC)
pc_19$mask <- mask_pc(pc_19$LPC)


pc_14$mask <- st_read("C:\\Users\\cscar\\FMT\\data\\SB_Mask15.shp")
pc_19$mask <- st_read("C:\\Users\\cscar\\FMT\\data\\SB_Mask19.shp")


st_write(pc_14$mask,"C:\\Users\\cscar\\FMT\\data\\SB_Mask15.shp")
st_write(pc_19$mask,"C:\\Users\\cscar\\FMT\\data\\SB_Mask19.shp")
plot(pc_19$LPC)


ground <- filter_ground(pc_14$LPC)

plot(ground)

pc_14$to_dtm(1)

plot(pc_14$DTM)

pc_19$to_dtm(1)

pc_14$to_chm(1)
plot(pc_14$CHM)

pc_19$to_chm(1)

plot(pc_19$CHM)


#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(source = pc_14$CHM_raw, target = pc_19$CHM_raw, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")

source <- pc_14$CHM_raw
target <- pc_19$CHM_raw
source_mask <- pc_14$mask
target_mask <- pc_19$mask
method <- "bilinear"

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

footprints <- sf::st_read("C:/Users/cscar/FMT/data/SB_Buildings.shp")

# Create a new raster with the same extent as SB_Change and 1m resolution
template_raster <- terra::rast(extent = terra::ext(source), resolution = 1)


# Step 3: Rasterize the building polygons onto the new 1m raster grid
buildings_raster <- terra::rasterize(terra::vect(footprints), template_raster, background = NA)

terra::crs(buildings_raster) <- terra::crs(source)

# Step 4: Set the overlapping raster cells in SB_Change to 0 where buildings exist
source[!is.na(buildings_raster)] <- 0
target[!is.na(buildings_raster)] <- 0



classified_diff <- diff_classify(source, target)
diff_class <- terra::mask(classified_diff, union)
plot(diff_class)


# Save the processed raster in the rv list so it can be accessed elsewhere
rv$classified_diff <- diff_class



st_write(pc_14$mask, paste0(getwd(), "/Data/mask_2015.shp"))
st_write(pc_19$mask, paste0(getwd(), "/Data/mask_2019.shp"))

################################################

################################################

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

# Function to generate CHM and classify the differences

chm_diff_test <- diff_classify(source_chm, target_chm)

plot_stats(diff_class)

plot(diff_class)

# Display the outputs/. 

displayMap(pc_14$DTM, pc_14$CHM, diff_class, chm_mask)


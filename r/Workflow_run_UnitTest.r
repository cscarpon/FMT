source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

#change this based on CPU
set_lidr_threads(10)

sunny_bound <- st_read("G:/EMC/Projects/Sunnybrooke/Data/Boundary/Sunnybrook.shp")
sunny_bound <- st_transform(sunny_bound, crs = 26917)

buildings <- sf::st_read(file.path("./data/SB_Buildings.shp"))
buildings <- sf::st_transform(buildings, crs = 26917)

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

# pc_14 <- spatial_container$new(mo_dir$metadata$file_path[2])
# pc_14$set_crs(32617)


# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[2])
pc_19$set_crs(32617)

# path_19 <- "data/TTP_2019_decimate.laz"
pc_23 <- spatial_container$new(mo_dir$metadata$file_path[4])
pc_23$set_crs(32617)

#Generating the Masks

# pc_14$mask <- mask_pc(pc_14$LPC)
pc_19$mask <- mask_pc(pc_19$LPC)
pc_23$mask <- mask_pc(pc_23$LPC)

# adding the buildings

if (sf::st_crs(buildings) != sf::st_crs(pc_19$mask)) {
  pc_19$buildings <- sf::st_transform(buildings, sf::st_crs(pc_19$mask))
} else {
  pc_19$buildings <- buildings
}

if (sf::st_crs(buildings) != sf::st_crs(pc_23$mask)) {
  pc_23$buildings <- sf::st_transform(buildings, sf::st_crs(pc_23$mask))
} else {
  pc_23$buildings <- buildings
}


#Denoising

pc_19$LPC <- noise_filter_buildings(pc_19$LPC, pc_19$mask, buildings)

pc_23$LPC <- noise_filter_buildings(pc_23$LPC, pc_23$mask, buildings)

# conda_create("EMT_conda", python = "3.9.13", packages = c("pdal", "numpy", "scipy"))

#Activate conda environment
reticulate::use_condaenv("fmt_conda", required = TRUE)
reticulate::import("pdal")

# renv::use_python("C:/Users/cscar/anaconda3/envs/EMT_conda/python.exe")

# Source the Python script
icp_module <- paste0(getwd(), "/py/icp_pdal.py")

reticulate::source_python(icp_module)

# Create instance of the ICP class
icp_aligner <- pdal_icp(pc_14$filepath, pc_19$filepath)

# Call the align method
aligned_file_path <- icp_aligner$align()

# Process the source point cloud
pc_19A <- spatial_container$new(as.character(aligned_file_path))
pc_19$set_crs(32617)


#Generating the DTM and nDSM

# pc_14$to_dtm(1)
pc_19$to_dtm(1)
pc_23$to_dtm(1)

# pc_14$to_chm(1)
pc_19$to_chm(1, buildings)
pc_23$to_chm(1, buildings)

plot(pc_19$CHM)

#This function aligns the two rasters and returns aligned raster objects.
aligned_ndsm <- process_raster(source = pc_19$CHM_raw, target = pc_23$CHM_raw, source_mask = pc_19$mask, target_mask = pc_23$mask, method = "bilinear")

################################################
################################################

source_ndsm <- aligned_ndsm[[1]]
target_ndsm <- aligned_ndsm[[2]]
chm_mask <- aligned_ndsm[[3]]


source("r/functions.R")
# Function to generate CHM and classify the differences

diff_class <- diff_classify(source_ndsm, target_ndsm)

plot_stats(diff_class)

difference_values <- diff_values(diff_class)

# Display the outputs. 


displayMap(pc_23$DTM, pc_23$CHM, diff_class, chm_mask)

# Get the values of the raster

# Get the frequency of each class
class_freq <- freq(diff_class)

# Calculate the total number of cells
total_cells <- sum(class_freq[, "count"])

# Calculate percentage for each class
class_freq$percentage <- (class_freq$count / total_cells) * 100

# Set the class values as row names
rownames(class_freq) <- class_freq$value

# Remove the 'value' column since it's now the row name
class_freq <- class_freq[, -1]

return(class_percentages)

pc_23$LPC@header@EVLR

source("global.R")
source("server.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

#change this based on CPU
set_lidr_threads(10)

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

pc_14 <- spatial_container$new(mo_dir$metadata$file_path[2])
pc_14$set_crs(32617)


# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[4])
pc_19$set_crs(32617)


# conda_create("EMT_conda", python = "3.9.13", packages = c("pdal", "numpy", "scipy"))

#Activate conda environment
reticulate::use_condaenv("cf_conda", required = TRUE)
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
pc_14A <- spatial_container$new(as.character(aligned_file_path))
pc_14$set_crs(32617)

#Generating the Masks

pc_14$mask <- mask_pc(pc_14$LPC)
pc_19$mask <- mask_pc(pc_19$LPC)

#Generating the DTM and nDSM

pc_14$to_dtm(1)
pc_19$to_dtm(1)

pc_14$to_chm(1)
pc_19$to_chm(1)

#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(source = pc_14$CHM_raw, target = pc_19$CHM_raw, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")

################################################
################################################

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

# Function to generate CHM and classify the differences

diff_class <- diff_classify(source_chm, target_chm)

plot_stats(diff_class)

# Display the outputs. 

displayMap(pc_14$DTM, pc_14$CHM, diff_class, chm_mask)

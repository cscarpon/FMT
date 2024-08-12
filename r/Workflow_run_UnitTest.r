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


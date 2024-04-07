source("fmt/global.R")
source("r/functions.R")
source("r/spatial_container.R")
source("r/meta_obj.R")

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

reticulate::py_config()

# # conda_list()
# use_condaenv("point_clouds", required = TRUE)
# py_install("brotli")
# # miniconda_update(path = miniconda_path())
# # py_install("open3d", envname = "point_clouds")

# seeing if the meta_obj works.


"C:/Users/cscar/FMT/data/"

# in_dir <- paste0(getwd(),"/data/")
# 
# mo_dir <- mo$new(in_dir)
# mo_dir$metadata


dir <- "./data/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)

str(mo_dir$metadata)

# path_14 <- "C:/Users/cscar/FMT/data/TTP_2014_decimate.laz"
# path_14 <- "data/TTP_2014_decimate.laz"



pc_14 <- spatial_container$new(mo_dir$metadata$file_path[1])

pc_14$set_crs(32617)


# path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(mo_dir$metadata$file_path[2])
pc_19$set_crs(32617)

sf::st_crs(pc_19$mask)


# # import SciPy (it will be automatically discovered in "r-reticulate")
# open3D <- import("open3D")
# numpy <- import("numpy")

# source_python("py/ICP_Object.py")

plot(pc_14$mask)

pc_14$to_dtm(1)

pc_19$to_dtm(1)

pc_14$to_chm(1)

pc_19$to_chm(1)

plot(pc_19$CHM)

#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14$CHM_raw, pc_19$CHM_raw, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")

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


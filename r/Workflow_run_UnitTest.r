source("fmt/global.R")
source("r/functions.R")
source("r/spatial_obj.R")
source("r/meta_obj.R")

density_output <- las_density(las, slice_size = 1)
#Methods

# to_dtm()        save_las()
# to_chm()        save_dtm()
# save_mask()     save_chm()
# save_pc()

#Functions

process_raster(source, target, mask_layer)
CHM_diff_classify(earlier, later)
raster_stats(raster)
mask_pc()

reticulate::py_config()

# conda_list()
use_condaenv("point_clouds", required = TRUE)
py_install("brotli")
# miniconda_update(path = miniconda_path())
# py_install("open3d", envname = "point_clouds")

# seeing if the meta_obj works.

dir <- "./data/"
mo_dir <- mo$new(dir)
print(mo_dir$metadata)

path_14 <- "data/TTP_2014_decimate.laz"
pc_14 <- spatial_container$new(path_14)

pc_14$set_crs(32617)

sf::st_crs(pc_14$LPC)

path_19 <- "data/TTP_2019_decimate.laz"
pc_19 <- spatial_container$new(path_19)
pc_19$set_crs(32617)


# # import SciPy (it will be automatically discovered in "r-reticulate")
# open3D <- import("open3D")
# numpy <- import("numpy")

# source_python("py/ICP_Object.py")

plot(pc_14$mask)

pc_14$to_dtm(5)
pc_19$to_dtm(5)

pc_14$to_chm(5)
pc_19$to_chm(5)

#This function aligns the two rasters and returns aligned raster objects.
aligned_chm <- process_raster(pc_14$CHM, pc_19$CHM, source_mask = pc_14$mask, target_mask = pc_19$mask, method = "bilinear")

source_chm <- aligned_chm[[1]]
target_chm <- aligned_chm[[2]]
chm_mask <- aligned_chm[[3]]

# Function to generate CHM and classify the differences

chm_diff_test <- CHM_diff_classify(source_chm, target_chm)

# Display the outputs/. 

displayMap(pc_14$DTM,pc_14$CHM, chm_diff_test, chm_mask)

plot_stats(chm_diff_test)